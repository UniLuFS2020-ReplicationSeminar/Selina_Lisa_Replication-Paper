#Pre-Processing

#Load Data


# load("tydiverse")
# dat <- load(file = here::here("DATASET DIRECTORY", "DATAFRAME"))


library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(dgo)
library(tidyverse)

install.packages("rio")
library("rio")
install.packages("here")
library(here)

rio::import(file= here::here("Data", "shaped.Rdata"))
  
data1 <- rio::import(file= here::here("Data", "shaped.Rdata"))
data1 <- load(file = here::here("Data", "shaped.Rdata"))

summary(data1)
view(data1)

require(readr)
install.packages("haven")
require(haven)






# 1. select key variables -------------------------------------------------
# 1. select only the key variables
#   dat <- as_tibble(dat)
#   select(dat, variable 1, variable 2) #or dat %>% select(variable 1, variable 2)

# 2. drop missing values

# 3. save

#commit 


