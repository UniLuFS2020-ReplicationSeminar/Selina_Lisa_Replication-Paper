setwd("/Users/nhatnguyen/Desktop/Uni/FS20/MA Replication/LisaSelina_Replication/Selina_Lisa_Replication-Paper/Syntax")

#Pre-Processing

#Load Data

<<<<<<< HEAD
# load("tydiverse")
# dat <- load(file = here::here("DATASET DIRECTORY", "DATAFRAME"))
=======

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


summary(data1)
view(data1)


>>>>>>> 90c106f7ff1d06bb3efe6b4f6bfa3c5736786357

# 1. select key variables -------------------------------------------------
# 1. select only the key variables
#   dat <- as_tibble(dat)
#   select(dat, variable 1, variable 2) #or dat %>% select(variable 1, variable 2)

# 2. drop missing values

# 3. save

#commit 

git config --global user.email "selinadang97@yahoo.de"
<<<<<<< HEAD
git config --global user.name "Selina Nguyen"
=======
git config --global user.name "Selina Nguyen"


###hallo
dfkjdlaf
hdlfajdflk
kfjaldkfj
kdfjalf
kjflkdjfl
jdfklajf

>>>>>>> 90c106f7ff1d06bb3efe6b4f6bfa3c5736786357
