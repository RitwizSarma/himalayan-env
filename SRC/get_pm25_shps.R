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
shrid2 <- read_sf("SHAPES/shrug-shrid-poly-shp/shrid2_open.shp")
states <- c("hp", "uk")


hp_shrids <- subset(shrid_states, state_name=="himachal pradesh")
hp_pm25 <- pm25[pm25$shrid2 %in% hp_shrids$shrid2, ]
yr_range <- unique(hp_pm25$year)
write.csv(hp_pm25, "DATA/env/pm25_hp.csv")

for (yr in yr_range) {
  dt <- subset(hp_pm25, year==yr)
  dt_sp <- merge(dt, shrid2, by = "shrid2")
  st_write(dt_sp, glue("SHAPES/pm25_hp/pm25_hp{yr}.shp"))
}

uk_shrids <- subset(shrid_states, state_name=="uttarakhand")
uk_pm25 <- pm25[pm25$shrid2 %in% uk_shrids$shrid2, ]
yr_range <- unique(uk_pm25$year)
write.csv(uk_pm25, "DATA/env/pm25_uk.csv")

for (yr in yr_range) {
  dt <- subset(hp_pm25, year==yr)
  dt_sp <- merge(dt, shrid2, by = "shrid2")
  st_write(dt_sp, glue("SHAPES/pm25_uk/pm25_uk{yr}.shp"))
}

target_shrids <- rbind(hp_shrids, uk_shrids)
length(unique(target_shrids$shrid2))
# 32634
table(target_shrids$state_name)
