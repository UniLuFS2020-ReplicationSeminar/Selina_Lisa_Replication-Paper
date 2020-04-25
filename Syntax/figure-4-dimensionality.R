library(data.table)
library(dplyr)
library(ggplot2)
require(readr)
require(haven)

DROPBOX_PATH = '.'
DATA_PATH = '.'
ESTIMATES_PATH = '.'
FIGURE_PATH = '.'

stopifnot(dir.exists(DROPBOX_PATH))
stopifnot(dir.exists(DATA_PATH))
stopifnot(dir.exists(FIGURE_PATH))

paths = c("Economic" = "senate-econ.dta",
  "Social" = "senate-social.dta",
  "Racial" = "senate-race.dta")

ideals = lapply(paths, function(p) haven::read_dta(file.path(DATA_PATH, p)))
ideals = rbindlist(ideals, idcol = "domain")

ideals = ideals[congress <= 113]
ideals = ideals[!(domain == 'Racial' & congress < 85)]
ideals = ideals[!(domain == 'Social' & congress < 85)]

# Normalize scores 
ideals[, dynamic_ideal := (dynamic_ideal - mean(dynamic_ideal)) / sd(dynamic_ideal),
  by = c("domain")]
ideals[, mean(dynamic_ideal), by = "domain"]  # ~0
ideals[, sd(dynamic_ideal), by = "domain"]    # 1

# Drop presidents
ideals = ideals[state_abbrev != "USA"]

# Keep Ds and Rs; recode as such
ideals = ideals[party_code %in% c(100, 200)]
ideals[, party_code := as.character(party_code)]
ideals[party_code == "100", party_code :=  "D"]
ideals[party_code == "200", party_code :=  "R"]
ideals[, party := factor(party_code, c("R", "D"))]
ideals[, state := factor(state_abbrev)]

# Add years
congresses = readRDS(file.path(DATA_PATH, "congresses.Rds"))
ideals = merge(ideals, congresses, by.x = "congress", by.y = "cong", all.x = TRUE, all.y = FALSE)

# Cast domains wide 
ideals = ideals[, .(year, icpsr, congress, dynamic_ideal, bioname, state, party, domain)]
ideals = dcast(ideals, ... ~ domain, value.var = 'dynamic_ideal')

economic_social = ideals[, .(cor = abs(cor(Economic, Social, use="pairwise.complete.obs"))), by = 'year']
economic_racial = ideals[, .(cor = abs(cor(Economic, Racial, use="pairwise.complete.obs"))), by = 'year']
social_racial = ideals[, .(cor = abs(cor(Social, Racial, use="pairwise.complete.obs"))), by = 'year']

senate_cor = rbindlist(list(
    "Economic vs. Social" = economic_social,
    "Economic vs. Racial" = economic_racial,
    "Social vs. Racial" = social_racial), idcol = "Domain")
senate_cor = senate_cor[!is.na(cor)]
setnames(senate_cor, "Domain", "Domains")

# Repeat for state publics

econ_fit = readRDS(file.path(ESTIMATES_PATH, "dgirt-econ.Rds"))
social_fit = readRDS(file.path(ESTIMATES_PATH, "dgirt-social.Rds"))
race_fit = readRDS(file.path(ESTIMATES_PATH, "dgirt-race.Rds"))

mean_tb = function(dgirt_out, start_year = NULL) {
  # Extract theta-bars
  tb = as.data.frame(dgirt_out)

  # Summarize posterior
  tb = tb[, .(value = mean(value)), by = c('D_abb', 'D_pid3', 'D_year')]

  # Use consistent start year
  if (length(start_year)) {
    tb = tb[D_year >= start_year]
  }

  # Standardize point estimates
  tb[, value := (value - mean(value)) / sd(value)]

  # Drop independents
  tb = tb[D_pid3 %in% c("D_pid31", "D_pid33")]

  tb[]
}

econ_tb = mean_tb(econ_fit)
social_tb = mean_tb(social_fit, start_year = 1957)
race_tb = mean_tb(race_fit, start_year = 1957)

tb = rbindlist(list(
    "Economic" = econ_tb,
    "Social" = social_tb,
    "Racial" = race_tb), idcol = "Domain")

tb = dcast(tb, ... ~ Domain, value.var = 'value')

economic_social_tb = tb[, .(cor = abs(cor(Economic, Social,
      use="pairwise.complete.obs"))), by = 'D_year']
economic_racial_tb = tb[, .(cor = abs(cor(Economic, Racial,
      use="pairwise.complete.obs"))), by = 'D_year']
social_racial_tb = tb[, .(cor = abs(cor(Social, Racial,
      use="pairwise.complete.obs"))), by = 'D_year']

public_cor = rbindlist(list(
    "Economic vs. Social" = economic_social_tb,
    "Economic vs. Racial" = economic_racial_tb,
    "Social vs. Racial" = social_racial_tb), idcol = "Domain")
public_cor = public_cor[!is.na(cor)]
setnames(public_cor, "Domain", "Domains")

setnames(public_cor, 'D_year', 'year')
cors = rbindlist(list("Senators" = senate_cor, "State Publics" = public_cor),
  idcol = "Level")

plot_correlations = function(correlations, plot_title) {
  p = ggplot(correlations) +
    geom_line(aes(year, cor, linetype = Domains, color = Domains)) + 
    # Alternative: facet by level
    # facet_wrap(~ Level, ncol = 1) +
    scale_linetype_manual(values = c("Economic vs. Social" = "solid",
        "Economic vs. Racial" = "dotdash", "Social vs. Racial" = "dashed")) +
    scale_color_manual(values = c("Economic vs. Social" = "#f04546",
        "Economic vs. Racial" = "#3591d1", "Social vs. Racial" = "#62c76b")) +
    scale_y_continuous("Correlation Between Domainss", limits = c(0, 1)) +
    scale_x_continuous("Year", limits = c(1955, 2015), breaks = seq(1950, 2010, 10)) +
    ggtitle(plot_title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
    ggsave(file.path(FIGURE_PATH, paste0(Sys.Date(), "__figure-4-",
          tolower(gsub('\\s', '_', plot_title)), ".pdf")), p, width = 6.5,
      height = 3)
    p
}

plots = Map(plot_correlations, list(senate_cor, public_cor),
  list("Senate Ideal Points", "Mass Policy Ideology"))

