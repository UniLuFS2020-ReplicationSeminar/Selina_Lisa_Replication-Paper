library(data.table)
library(ggplot2)
install.packages("readr")
install.packages("haven")
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)

#set wd
here::here()

#load and rename the datasets
setwd("/Users/nhatnguyen/Desktop/Uni/FS20/MA Replication/LisaSelina_Replication/Selina_Lisa_Replication-Paper/Data")

Economic <- get(load("senate-econ.RData"))
Social <- get(load("senate-social.RData"))
Racial <- get(load("senate-race.RData"))

remove(table)

#merge datasets into one called ideals, add a column which identifies from which dataset the values came from
ideals <- bind_rows(Economic = Economic, Social = Social, Racial = Racial, .id = "domain")

#filtering out the dataset to acquire only data from the 85th for the racial and social domain, and to the 113th Congress for all domains (p.137)
ideals <- filter(ideals, congress <= 133,
                 !(domain == "Racial" & congress < 85),
                 !(domain == "Social" & congress < 85))

# Normalize scores (using Z-score: Z = (x-mean)/sd)
ideals <- ideals %>% 
  group_by(domain) %>% 
  mutate(dynamic_ideal_norm = (dynamic_ideal - mean(dynamic_ideal, na.rm = T)) / sd(dynamic_ideal, na.rm = T))

# check if it worked
mean(ideals$dynamic_ideal_norm) # ~0
sd(ideals$dynamic_ideal_norm) # 1

# Drop presidents
ideals <- filter(ideals, !(state_abbrev == "USA"))



# Check polarity
ideals <- ideals %>% 
  mutate(party_code = factor(party_code))

party_means <- ideals %>%
  group_by(domain, party_code, congress) %>% 
  mutate(mean_ideal = mean(dynamic_ideal_norm))

#     party codes: 100 for Democrats, 200 for Republicans, 212 for Conservatives, 328 for Independent
p1 <- ggplot(subset(party_means, party_code %in% c(100, 200))) +
  geom_line(mapping = aes(x = congress, y = mean_ideal, color = party_code)) +
  facet_wrap(~ domain)
p1


#filter out the Conservative and Independent party members
ideals <- filter(ideals, !(party_code == 212), !(party_code == 328))

#filter out the states that are represented by senators of only one party
ideals <- ideals %>% 
  group_by(congress, state_abbrev) %>% 
  mutate(two_party = length(unique(party_code)) > 1)

#keep the states represented by both parties
ideals <- ideals %>% 
  filter(!(two_party == F))

#Take the within-state-party average, for the case in which there's more than
# one senator/member in a state-congress pair.    
ideals <- ideals %>% 
  group_by(congress, state_abbrev, party_code) %>% 
  mutate(dynamic_ideal_norm = mean(dynamic_ideal_norm))


# Take the within-state differences between parties in each congress
mean_state_diff <- function(ideal_points, time_var = "congress") {
  ideals = copy(ideal_points)
  setDT(ideals, key = c("congress", "state_abbrev", "party_code"))
  diffs <- ideals %>% 
    group_by(congress, state_abbrev) %>% 
    mutate(party_code = first(party_code),
           diff_score = dynamic_ideal[1] - dynamic_ideal[2], n = .N)
  mean_diffs <- diffs %>% 
    group_by(congress) %>% 
    mutate(mean_diff_score = mean(diff_score, na.rm = T), n = .N)
  mean_diffs
}
    

mean_diff_by_domain = function(ideals) {
  .ideals = copy(ideals)
  econ_diff <- mean_state_diff(filter(.ideals, domain == "Economic"))
  race_diff <- mean_state_diff(filter(.ideals, domain == "Racial"))
  social_diff <- mean_state_diff(filter(.ideals, domain == "Social"))
  
  diffs = rbindlist(list("Economic" = econ_diff,
                         "Racial" = race_diff,
                         "Social" = social_diff),
                    idcol = "Domain", fill = T)
  
  congresses <- readRDS("congresses.Rds")
  merge(diffs, congresses, by.x = "congress", by.y = "cong", all.x = T, all.y = F)
}

mean_diffs <- mean_diff_by_domain(ideals)


# Flip for same polarity as previous estimates
mean_diffs <- mean_diffs %>% 
  mutate(mean_diff_score = mean_diff_score * -1)



p2 <- ggplot(data = mean_diffs) +
  geom_line(mapping = aes(end_year, mean_diff_score, color = Domain, linetype = Domain)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash")) +
  scale_y_continuous("Avg. Difference", limits = c(-.2, 2.6)) +
  scale_x_continuous("Year", limits = c(1945, 2017), breaks = seq(1950, 2010, 10)) +
  ggtitle("Senate Ideal Points") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

p2
  
```