library(glue)
library(ggplot2)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(tidyverse)
library(terra)
library(earthdatalogin)


# WORKING DIRECTORY
setwd("C:/LargeFiles/Research/himalayan_env/SHAPES/lst/downloads")


# SET CREDS
edl_netrc(username = "ritwizs", password = "6%2pZ_XPM9Pv.J4")


# CHECKING
url <- "https://appeears.earthdatacloud.nasa.gov/api/bundle/1873ca40-e7d6-456e-9d04-4fac27104655/ff864446-0899-4408-a382-c077188f02ec/MOD11A2.061_LST_Day_1km_doy2000049_aid0001.tif"
rast1 <- terra::rast(url, vsi=TRUE)

edl_download(url)

r <- sds("MOD11A2.A2000081.h33v10.061.2020045130820.hdf")
r
