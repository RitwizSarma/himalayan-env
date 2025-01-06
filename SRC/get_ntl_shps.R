library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)


library(dplyr)

filter_max_ntl <- function(data, x_col, y_col, ntl_col) {
  # Create a new column for the pairs
  data$pair <- with(data, paste(data[[x_col]], data[[y_col]]))
  
  # Find the maximum 'ntl' for each unique pair
  filtered_data <- data %>%
    group_by(pair) %>%
    filter(data[[ntl_col]] == max(data[[ntl_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    select(-pair)  # Remove the temporary pair column
  
  return(filtered_data)
}






setwd("C:/LargeFiles/Research/himalayan_env/")

dmsp <- read.csv("DATA/shrug-dmsp-csv/dmsp_shrid.csv")
viirs <- read.csv("DATA/shrug-viirs-annual-csv/viirs_annual_shrid.csv")
shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")
shrid2 <- read_sf("SHAPES/shrug-shrid-poly-shp/shrid2_open.shp")


hp_shrids <- subset(shrid_states, state_name=="himachal pradesh")
hp_dmsp <- dmsp[dmsp$shrid2 %in% hp_shrids$shrid2, ]
hp_viirs <- viirs[viirs$shrid2 %in% hp_shrids$shrid2, ]
# hp_dmsp <- unique(hp_dmsp)
yr_range <- unique(hp_dmsp$year)
# yr_range <- c(2021, 2015, 2020, 2014, 2017, 2019, 2018, 2013, 2016)
write.csv(hp_dmsp, "DATA/econ/dmsp_hp.csv")
write.csv(hp_viirs, "DATA/econ/viirs_hp.csv")

hp_dmsp <- filter_max_ntl(hp_dmsp, "shrid2", "year", "dmsp_mean_light_cal")

hp_repeat <- check_unique_pairs(hp_dmsp, "shrid2", "year")

check <- subset(hp_dmsp, shrid2=="11-02-030-00157-019931")
unique(check$dmsp_f_version)

colnames(hp_dmsp) <- c("shrid2", "min", "max", "mean", "sum", "count", "categ", "year")

# for (yr in yr_range) {
#   dt <- subset(hp_dmsp, year==yr)
#   dt_sp <- merge(dt, shrid2, by = "shrid2")
#   st_write(dt_sp, glue("SHAPES/viirs_hp/viirs_hp{yr}.shp"), append=FALSE)
# }

uk_shrids <- subset(shrid_states, state_name=="uttarakhand")
uk_dmsp <- dmsp[dmsp$shrid2 %in% uk_shrids$shrid2, ]
uk_viirs <- viirs[viirs$shrid2 %in% uk_shrids$shrid2, ]
yr_range <- unique(uk_dmsp$year)
write.csv(uk_dmsp, "DATA/econ/dmsp_uk.csv")
write.csv(uk_viirs, "DATA/econ/viirs_uk.csv")

colnames(uk_dmsp) <- c("shrid2", "min", "max", "mean", "sum", "count", "categ", "year")

# for (yr in yr_range) {
#   dt <- subset(uk_dmsp, year==yr)
#   dt_sp <- merge(dt, shrid2, by = "shrid2")
#   st_write(dt_sp, glue("SHAPES/viirs_uk/viirs_uk{yr}.shp"), append=FALSE)
# }
