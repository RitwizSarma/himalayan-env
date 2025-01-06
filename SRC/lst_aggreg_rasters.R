library(glue)
library(ggplot2)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(terra)


setwd("C:/LargeFiles/Research/himalayan_env/")


st_prefix = "uk"
years <- c(2000:2020)


for (yr in years) {
  day_files <- Sys.glob(glue("SHAPES/lst/appears_{st_prefix}/day/MOD11A2.061_LST_Day_1km_doy{yr}*_aid0001.tif"))
  nt_files <- Sys.glob(glue("SHAPES/lst/appears_{st_prefix}/night/MOD11A2.061_LST_Night_1km_doy{yr}*_aid0001.tif"))
  day_r <- rast(day_files)
  nt_r <- rast(nt_files)
  day_mean <- app(day_r, fun = median, na.rm = TRUE)
  nt_mean <- app(nt_r, fun = median, na.rm = TRUE)
  yr_means <- c(day_mean, nt_mean)
  yr_r <- app(yr_means, fun = mean, na.rm = TRUE)
  writeRaster(yr_r, glue("SHAPES/lst/outputs/{st_prefix}_{yr}.tif"), overwrite=TRUE)
  print(glue("Completed for {yr}."))
}

