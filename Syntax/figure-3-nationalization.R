#!/usr/bin/env Rscript

library(data.table)
library(ggplot2)
library(ggthemes)
library(lme4)
require(readr)
library(broom)
library(dgo)
library(dplyr)

DROPBOX_PATH = '.'
DATA_PATH = '.'
ESTIMATE_PLOT_PATH = '.'

stopifnot(dir.exists(DROPBOX_PATH))
stopifnot(dir.exists(DATA_PATH))
if (!dir.exists(ESTIMATE_PLOT_PATH)) {
	stopifnot(all(dir.create(ESTIMATE_PLOT_PATH, recursive = TRUE)))
}

# Load state public model results
econ_est = readRDS(file.path(DROPBOX_PATH, 'dgirt-econ.Rds'))
social_est = readRDS(file.path(DROPBOX_PATH, 'dgirt-social.Rds'))
race_est = readRDS(file.path(DROPBOX_PATH, 'dgirt-race.Rds'))

# Confirm model domains
# econ_est@dgirt_in$control@item_names
# social_est@dgirt_in$control@item_names
# race_est@dgirt_in$control@item_names

# Extract theta-bars
econ_tb = as.data.frame(econ_est)
social_tb = as.data.frame(social_est)
race_tb = as.data.frame(race_est)

# Combine domains
tb = rbindlist(list("Economic" = econ_tb,
    "Social" = social_tb,
    "Racial" = race_tb), idcol = "domain")

tb = tb[!(domain == 'Social' & D_year < 1957)]
tb = tb[!(domain == 'Racial' & D_year < 1957)]

# Summarize posterior
tb = tb[, .(value = mean(value)), by = c('domain', 'D_abb', 'D_pid3', 'D_year')]

# Standardize point estimates
tb = tb[, value := (value - mean(value)) / sd(value), by = c("domain")]

public_props = tb %>%
  group_by(D_year, domain) %>%
  do(tidy(aov(value ~ D_pid3, data = .))) %>%
  summarise(proportion = sumsq[term == "D_pid3"] / sum(sumsq)) %>%
  rename(year = D_year)

# Add years of Congress
congresses = readRDS(file.path(DATA_PATH, 'congresses.Rds'))
congresses = melt(congresses, id.var = "cong", value.name = "year") %>%
  select(congress = cong, year)
public_props = merge(public_props, congresses, by = "year")

# Load elite ideal points
paths = c("Economic" = "senate-econ.dta",
  "Social" = "senate-social.dta",
  "Racial" = "senate-race.dta")
ideals = lapply(paths, function(p) haven::read_dta(file.path(DATA_PATH, p)))
ideals = rbindlist(ideals, idcol = "domain")

# Normalize scores 
ideals[, dynamic_ideal := (dynamic_ideal - mean(dynamic_ideal)) / sd(dynamic_ideal),
  by = c("domain")]
ideals[, mean(dynamic_ideal), by = "domain"]  # ~0
ideals[, sd(dynamic_ideal), by = "domain"]    # 1

# Drop presidents
ideals = ideals[state_abbrev != "USA"]

# Start racial and social domains in 85th Congress
ideals = ideals[!(domain %in% c("Social", "Racial") & congress < 85)]

# Check number of cross-party states
ideals %>%
  group_by(congress, state_abbrev) %>%
  filter(party_code %in% c(100, 200) & !is.na(dynamic_ideal)) %>%
  summarise(parties = paste(sort(unique(party_code)), collapse = ", ")) %>%
  summarise(n_x_party = sum(parties == "100, 200")) %>%
  arrange(congress)

congresses = readRDS(file.path(DATA_PATH, 'congresses.Rds'))
ideals = merge(ideals, congresses, by.x = "congress", by.y = "cong", all.x =
  TRUE, all.y = FALSE)

elite_props = ideals %>%
  filter(party_code %in% c(100, 200)) %>%
  group_by(congress, year, domain) %>%
  do(tidy(aov(dynamic_ideal ~ party_code, data = .))) %>%
  summarise(proportion = sumsq[term == "party_code"] / sum(sumsq))

props = rbindlist(list("State Publics" = public_props, "Senators" = elite_props),
  fill = TRUE, idcol = "Level")
head(props)
tail(props)

props[, Level := ordered(Level, levels = c('State Publics', 'Senators'))]

p = ggplot(props) +
  geom_line(aes(x = year, y = proportion, linetype = Level, color = Level)) +
  scale_linetype_discrete() +
  scale_color_discrete() +
  theme_bw() +
  facet_wrap(~ domain) +
  scale_x_continuous("Year", limits = c(1945, 2015), breaks = seq(1960, 2000, 20)) +
  # ylim(0, 1) +
  ylab("Proportion of Variation Between Parties") +
  theme(legend.justification=c(0,0),
    legend.position=c(.12,.05),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))
p

ggsave(file.path(ESTIMATE_PLOT_PATH, paste0(Sys.Date(), "__figure-3.pdf")), p,
  width = 6.5, height = 3)
