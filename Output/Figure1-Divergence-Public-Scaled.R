
library(dgo)
library(dplyr)
library(data.table)
library(ggplot2)
library(glue)

DATA_PATH <- "C:\\Users\\thail\\Documents\\FS20\\r-related_data\\The ideological nationalization of partisan subconstituencies in the American States.zip"  

econ_fit = readRDS(file.path(DATA_PATH, "dgirt-econ.Rds"))
social_fit = readRDS(file.path(DATA_PATH, "dgirt-social.Rds"))
race_fit = readRDS(file.path(DATA_PATH, "dgirt-race.Rds"))



mean_scaled_divergence = function(dgirt_out, start_year = NULL) {
  
  # Extract theta-bars
  tb = as.data.frame(dgirt_out)
  
  # Summarize posterior
  tb = tb[, .(value = mean(value)), by = c('D_abb', 'D_pid3', 'D_year')]
  
  if (length(start_year)) {
    tb = tb[D_year >= start_year]
  }
  
  # Standardize point estimates
  tb[, value := (value - mean(value)) / sd(value)]
  
  # Drop independents
  tb = tb[D_pid3 %in% c("D_pid31", "D_pid33")]
  
  # Take differences between parties within state-years, by iteration
  tb[, n := .N, by = c("D_abb", "D_year")]
  if (!all(tb$n == 2)) {
    message("Dropped ", sum(tb$n != 2), " state-years with < 2 ",
            "party estimates")
    tb = tb[n == 2]
  }
  
  diffs = tb[, .(diff = value[1] - value[2]), by = c("D_abb", "D_year")]
  
  # Average over states, by year and iteration
  diffs = diffs[, .(diff = mean(diff)), by = c("D_year")]
  
  diffs[]
}

econ_diffs = mean_scaled_divergence(econ_fit)
social_diffs = mean_scaled_divergence(social_fit, start_year = 1957)
race_diffs = mean_scaled_divergence(race_fit, start_year = 1957)

diffs = bind_rows("Economic" = econ_diffs,
                  "Racial" = race_diffs,
                  "Social" = social_diffs, .id = "Domain")

p = ggplot(diffs) +
  geom_line(aes(D_year, diff, color = Domain, linetype = Domain)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash")) +
  scale_y_continuous("Avg. Difference") + #, limits = c(-.2, 2.6)) +
  scale_x_continuous("Year", limits = c(1945, 2017), breaks = seq(1950, 2010,
                                                                  10)) +
  ggtitle("Mass Policy Ideology") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(FIGURE_PATH, paste0(Sys.Date(), "__figure-1-c.pdf")), p, width =
         6.5, height = 3)
