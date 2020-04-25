#!/usr/bin/env Rscript

library(dgo)
library(dplyr)
library(data.table)
library(ggplot2)
library(glue)

DROPBOX_PATH = '.'
DATA_PATH = '.'
FIGURE_PATH = '.'

econ_fit = readRDS(file.path(DATA_PATH, "dgirt-econ.Rds"))
social_fit = readRDS(file.path(DATA_PATH, "dgirt-social.Rds"))
race_fit = readRDS(file.path(DATA_PATH, "dgirt-race.Rds"))

state_party_means = function(dgirt_out, start_year = NULL) {

  # Extract theta-bars
  tb = as.data.frame(dgirt_out)

  # Standardize
  tb[, value := (value - mean(value)) / sd(value)]

  # Drop independents
  tb = tb[D_pid3 %in% c("D_pid31", "D_pid33")]

  # Keep NY, GA
  tb = tb[D_abb %in% c("D_abbNY", "D_abbGA")]

  # Summarize over samples
  tb_summary = tb[, .(value = mean(value)), by = c("D_year", "D_abb", "D_pid3")]

  if (length(start_year)) {
    tb_summary = tb_summary[D_year >= start_year]
  }

  tb_summary[]
}

econ_means = state_party_means(econ_fit)
social_means = state_party_means(social_fit, start_year = 1957)
race_means = state_party_means(race_fit, start_year = 1957)

means = rbindlist(list(Economic = econ_means, Racial = race_means, Social =
    social_means), idcol = "Domain")

means[D_abb == "D_abbGA", D_abb := "Georgia"]
means[D_abb == "D_abbNY", D_abb := "New York"]

# Flip signs: policy conservatism, not liberalism
means[, `:=`(value = value * -1)]

means[, D_pid3 := ordered(D_pid3, levels = c("D_pid33", "D_pid31"))]

p = ggplot(means) +
  geom_line(aes(D_year, value, color = D_pid3, linetype = D_pid3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  scale_linetype_manual(values=c("solid", "dashed"), labels = c("R", "D")) +
  scale_color_discrete(labels = c("R", "D")) +
  scale_y_continuous("Conservatism") +
  scale_x_continuous("Year", limits = c(1945, 2015), breaks = seq(1960, 2000, 20)) +
  ggtitle("Mass Policy Ideology") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(Domain ~ D_abb, scales = "free_y") +
  labs(linetype = "Party", color = "Party")
p

ggsave(file.path(FIGURE_PATH, paste0(Sys.Date(), "__figure-2.pdf")), p, width = 5, height = 4)
