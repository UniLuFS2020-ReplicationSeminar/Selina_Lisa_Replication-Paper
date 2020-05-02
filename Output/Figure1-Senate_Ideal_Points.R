library(data.table)
library(ggplot2)
install.packages("readr")
install.packages("haven")
library(haven)
library(readr)
library(tidyverse)
library(data.table)

#not sure what this serves
DROPBOX_PATH = '.'
DATA_PATH = '.'
ESTIMATE_PLOT_PATH = '.'

#rename the datasets
paths = c("Economic" = "senate-econ.dta",
          "Social" = "senate-social.dta",
          "Racial" = "senate-race.dta")

#Is this to load the datasets?
ideals = lapply(paths, function(p) haven::read_dta(file.path(DATA_PATH, p)))

ideals = rbindlist(ideals, idcol = "domain")

ideals = ideals[ideals$congress <= 113]
ideals = ideals[!(domain == 'Racial' & congress < 85)]
ideals = ideals[!(domain == 'Social' & congress < 85)]
```