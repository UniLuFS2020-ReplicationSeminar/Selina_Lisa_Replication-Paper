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
# := means update the column dynamic_ideal by the normalized dynamic_ideal
ideals[, dynamic_ideal := (dynamic_ideal - mean(dynamic_ideal)) / sd(dynamic_ideal),
       by = c("domain")] #Does this mean update column by domain?
ideals[, mean(dynamic_ideal), by = "domain"]  # ~0 -> Is this a command or shouldn't it be already almost 0,because we normalized?
ideals[, sd(dynamic_ideal), by = "domain"]    # 1 -> Is this a command or shouldn't it be already 1, because we normalized?
#             What does the argumetn by = "domain" do exactly?


# Drop presidents
# Subsetting again with []: filtering president, because they represent not one state, but the country
ideals = ideals[state_abbrev != "USA"]


# Check polarity
party_means = ideals[, .(mean_ideal = mean(dynamic_ideal)) , by = c("domain", "party_code", "congress")] #create a list with .() -> same as list()
party_means[, party_code := factor(party_code)] #through factor() change party_code to a nominal variable

p = ggplot(party_means[party_code %in% c(100, 200)]) + #party codes: 100 for Democrats, 200 for Repiblicans and 328 for Independent
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
  diffs = ideals[, .(state_abbrev = first(state_abbrev), party_code = first(party_code), diff_score =
                       dynamic_ideal[1] - dynamic_ideal[2], n = .N), by = c("congress", "state_abbrev")]
  
  mean_diffs = diffs[, .(mean_diff_score = mean(diff_score, na.rm = TRUE), n = .N), by =
                       c(time_var)]
  
  mean_diffs
}

  
  
  
```