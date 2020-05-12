#!/usr/bin/env Rscript
# 
# Plot the average across states, for each Congress 83-115, of the intrastate
# divergence in ideal points of Senators from different parties. These scores
# are from MCMCPack estimates. The result is a plot showing divergence in each
# issue domain, saved to disk.

library(data.table)
library(ggplot2)
require(readr)
require(haven)

DROPBOX_PATH = '.'
DATA_PATH = '.'
ESTIMATE_PLOT_PATH = '.'

stopifnot(dir.exists(DROPBOX_PATH))
stopifnot(dir.exists(DATA_PATH))
if (!dir.exists(ESTIMATE_PLOT_PATH)) {
	stopifnot(all(dir.create(ESTIMATE_PLOT_PATH, recursive = TRUE)))
}

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

# Check polarity
party_means = ideals[, .(mean_ideal = mean(dynamic_ideal)) , by = c("domain", "party_code", "congress")]
party_means[, party_code := factor(party_code)]
p = ggplot(party_means[party_code %in% c(100, 200)]) +
  geom_line(aes(congress, mean_ideal, color = party_code)) +
  facet_wrap(~ domain)

mean_state_diff = function(ideal_points, time_var = "congress") {
  ideals = copy(ideal_points)

  # Drop any NA states and NA ideal points
  ideals = ideals[!is.na(get('state_abbrev')) & !is.na(get('dynamic_ideal'))]

  # Keep Democratic and Republican members of Congress
  ideals = ideals[party_code %in% c(100, 200)]

  # Create an indicator for the number of parties (one or two) represented by a
  # state's delegations
  ideals[, two_party := length(unique(party_code)) > 1, by = c('congress', 'state_abbrev')]

  # Keep the states represented by both parties
  ideals = ideals[get("two_party"), ]

  # Take the within-state-party average, for the case in which there's more than
  # one senator/member in a state-congress pair.
  ideals = ideals[, .(dynamic_ideal = mean(dynamic_ideal)), by = c("congress",
    "state_abbrev", "party_code")]

  # Take the within-state differences between parties in each congress
  setkeyv(ideals, c("congress", "state_abbrev", "party_code"))
  diffs = ideals[, .(#state_abbrev = first(state_abbrev), 
    party_code = first(party_code), diff_score =
    dynamic_ideal[1] - dynamic_ideal[2], n = .N), by = c("congress", "state_abbrev")]

    mean_diffs = diffs[, .(mean_diff_score = mean(diff_score, na.rm = TRUE), n = .N), by =
      c(time_var)]

  mean_diffs
}

mean_diff_by_domain = function(ideals) {
  .ideals = copy(ideals)
  econ_diff = mean_state_diff(.ideals[domain == 'Economic'])
  race_diff = mean_state_diff(.ideals[domain == 'Racial'])
  social_diff = mean_state_diff(.ideals[domain == 'Social'])

  diffs = rbindlist(list("Economic" = econ_diff,
      "Racial" = race_diff,
      "Social" = social_diff),
    idcol = "Domain", fill = TRUE)

  congresses = readRDS(file.path(DATA_PATH, "congresses.Rds"))
  merge(diffs, congresses, by.x = "congress", by.y = "cong", all.x = TRUE, all.y = FALSE)
}

mean_diffs = mean_diff_by_domain(ideals)

# Flip for same polarity as previous estimates
mean_diffs[, mean_diff_score := mean_diff_score * -1]

p = ggplot(mean_diffs) +
  geom_line(aes(end_year, mean_diff_score, color = Domain, linetype = Domain)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash")) +
  scale_y_continuous("Avg. Difference") + # , limits = c(-.2, 2.6)) +
  scale_x_continuous("Year", limits = c(1945, 2017), breaks = seq(1950, 2010, 10)) +
  ggtitle("Senate Ideal Points") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(ESTIMATE_PLOT_PATH, paste0(Sys.Date(), "__figure-1-a.pdf")), p,
  width = 6.5, height = 3)
