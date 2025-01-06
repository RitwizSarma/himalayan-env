library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)

setwd("C:/LargeFiles/Research/himalayan_env/")

pm25 <- read.csv("DATA/shrug-pm25-csv/pm25_shrid.csv")
shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")

hp_shrids <- subset(shrid_states, state_name=="himachal pradesh")
hp_pm25 <- pm25[pm25$shrid2 %in% hp_shrids$shrid2, ]
rm(pm25)


yrs <- sort(unique(hp_pm25$year))
glue("{str_sub(yrs[1], start = 3)}_{str_sub(yrs[2], start = 3)}")
