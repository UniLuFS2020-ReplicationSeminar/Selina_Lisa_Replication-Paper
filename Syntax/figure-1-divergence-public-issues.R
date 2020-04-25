library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(dgo)

FIGURE_PATH = '.'

load('shaped.Rdata')

PropCI <- function (s, n, level=.95) {
  if (length(s) == 1) {
    if (n > 0) {
      lo <- suppressWarnings(prop.test(x=s, n=n, conf.level=level)$conf.int[1])
      hi <- suppressWarnings(prop.test(x=s, n=n, conf.level=level)$conf.int[2])
    } else {
      lo <- hi <- NA
    }
    out <- data.frame(lo, hi)
  } else {
    df <- data.frame(s, n) 
    out <- apply(df, 1, function (row) {
      if (row["n"] > 0) {
        lo <- suppressWarnings(
          prop.test(x=row["s"], n=row["n"], conf.level=level)$conf.int[1])
        hi <- suppressWarnings(
          prop.test(x=row["s"], n=row["n"], conf.level=level)$conf.int[2])
      } else {
        lo <- hi <- NA
      }
      return(c(lo, hi))
    })
    out <- as.data.frame(t(out))
    names(out) <- c("lo", "hi")
  }
  return(out)
}

PO2South <- function (stpos) {
  ifelse(stpos %in% southern_pos, "South", "Non-South")
}

race_grp <- race_shp$group_counts %>%
  mutate(prop_est = s_grp / n_grp,
         prop_Q05 = PropCI(s_grp, n_grp, level=.9)$lo,
         prop_Q95 = PropCI(s_grp, n_grp, level=.9)$hi,
         PID3 = factor(PID3, levels=c("R", "I", "D"))) %>%
  group_by(item) %>%
  mutate(prop0_est = prop_est - weighted.mean(prop_est, n_grp),
         prop0_Q05 = prop_Q05 - weighted.mean(prop_est, n_grp),
         prop0_Q95 = prop_Q95 - weighted.mean(prop_est, n_grp)) %>%
  ungroup()

social_grp <- social_shp$group_counts %>%
  mutate(prop_est = s_grp / n_grp,
         prop_Q05 = PropCI(s_grp, n_grp, level=.9)$lo,
         prop_Q95 = PropCI(s_grp, n_grp, level=.9)$hi,
         PID3 = factor(PID3, levels=c("R", "I", "D"))) %>%
  group_by(item) %>%
  mutate(prop0_est = prop_est - weighted.mean(prop_est, n_grp),
         prop0_Q05 = prop_Q05 - weighted.mean(prop_est, n_grp),
         prop0_Q95 = prop_Q95 - weighted.mean(prop_est, n_grp)) %>%
  ungroup()

econ_grp <- econ_shp$group_counts %>%
  mutate(prop_est = s_grp / n_grp,
         prop_Q05 = PropCI(s_grp, n_grp, level=.9)$lo,
         prop_Q95 = PropCI(s_grp, n_grp, level=.9)$hi,
         PID3 = factor(PID3, levels=c("R", "I", "D"))) %>%
  group_by(item) %>%
  mutate(prop0_est = prop_est - weighted.mean(prop_est, n_grp),
         prop0_Q05 = prop_Q05 - weighted.mean(prop_est, n_grp),
         prop0_Q95 = prop_Q95 - weighted.mean(prop_est, n_grp)) %>%
  ungroup()

## Average over issues ##

mean_issue_divergence = function(props, absolute = FALSE) {
  # Keep non-missing proportions for Ds and Rs, where available for both Ds and Rs
  # in a state-year
  diffs = props %>%
    filter(as.character(PID3) %in% c("D", "R") & n_grp > 0) %>%
    group_by(biennium, StPO, item) %>%
    dplyr::mutate(n = n()) %>%
    filter(n == 2)

  # Take intra-state differences between proportions
  if (absolute) {
    diffs = diffs %>%
      group_by(biennium, StPO, item) %>%
      dplyr::summarize(diff_prop_est = abs(prop_est[1] - prop_est[2]),
        diff_prop0_est = abs(prop0_est[1] - prop0_est[2]))
  } else {
    diffs = diffs %>%
    group_by(biennium, StPO, item) %>%
    dplyr::summarize(diff_prop_est = prop_est[1] - prop_est[2],
      diff_prop0_est = prop0_est[1] - prop0_est[2])
  }

  # Average over issues, within state-years
  diffs = diffs %>%
    group_by(biennium, StPO) %>%
    dplyr::summarize(diff_prop0_est = mean(diff_prop0_est))

  # Average over states, within years
  diffs = diffs %>%
    group_by(biennium) %>%
    dplyr::summarize(diff_prop0_est = mean(diff_prop0_est))
  diffs
}

econ_diffs = mean_issue_divergence(econ_grp)
race_diffs = mean_issue_divergence(race_grp)
social_diffs = mean_issue_divergence(social_grp)

diffs = bind_rows(Economic = econ_diffs, Racial = race_diffs, Social =
  social_diffs, .id = "Domain")

p = diffs %>%
    filter(biennium >= 1957 | Domain == "Economic") %>%
    ggplot(aes(biennium, diff_prop0_est, color = Domain, linetype = Domain)) +
    geom_line() +
    ## geom_smooth(se = FALSE) +
    scale_linetype_manual(values=c("solid", "dashed", "dotdash")) +
    scale_y_continuous("Avg. Difference (%)", labels = percent_format()) +
    scale_x_continuous("Year", limits = c(1945, 2017),
                       breaks = seq(1950, 2010, 10)) +
    ggtitle("Mass Issue Positions") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(FIGURE_PATH, paste0(Sys.Date(), "__figure-1-b.pdf")), p, width =
  6.5, height = 3)

readRDS("/Users/nhatnguyen/Desktop/Uni/FS20/MA Replication/LisaSelina_Replication/dataverse/dgirt-econ.Rds")
load("/Users/nhatnguyen/Desktop/Uni/FS20/MA Replication/LisaSelina_Replication/dataverse/dgirt-econ.Rds")






