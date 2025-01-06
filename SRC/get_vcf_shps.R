library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)


setwd("C:/LargeFiles/Research/himalayan_env/")

vcf <- read.csv("DATA/shrug-vcf-csv/vcf_shrid.csv")
shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")
shrid2 <- read_sf("SHAPES/shrug-shrid-poly-shp/shrid2_open.shp")
states <- c("hp", "uk")


hp_shrids <- subset(shrid_states, state_name=="himachal pradesh")
hp_vcf <- vcf[vcf$shrid2 %in% hp_shrids$shrid2, ]
yr_range <- unique(hp_vcf$year)
write.csv(hp_vcf, "DATA/env/vcf_hp.csv")

for (yr in yr_range) {
  dt <- subset(hp_pm25, year==yr)
  dt_sp <- merge(dt, shrid2, by = "shrid2")
  st_write(dt_sp, glue("SHAPES/vcf_hp/vcf_hp{yr}.shp"))
}

uk_shrids <- subset(shrid_states, state_name=="uttarakhand")
uk_vcf <- vcf[vcf$shrid2 %in% uk_shrids$shrid2, ]
yr_range <- unique(uk_vcf$year)
write.csv(uk_vcf, "DATA/env/vcf_uk.csv")

for (yr in yr_range) {
  dt <- subset(uk_pm25, year==yr)
  dt_sp <- merge(dt, shrid2, by = "shrid2")
  st_write(dt_sp, glue("SHAPES/vcf_uk/vcf_uk{yr}.shp"))
}

target_shrids <- rbind(hp_shrids, uk_shrids)
length(unique(target_shrids$shrid2))
# 32634
