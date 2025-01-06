library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(dplyr)
library(tmap)
library(raster)
library(terra)


quick_plot <- function(shp_data) {
  tmap_mode("view")
  tmap_options(check.and.fix = TRUE)
  tm_shape(shp_data) + tm_borders()
}
fix_invalid <- function(shp) {
  inv <- st_is_valid(shp)
  print(glue("There are {length(inv) - sum(inv)} invalid polygons."))
  new_shp <- st_make_valid(shp)
  new_inv <- st_is_valid(new_shp)
  print(glue("After fixing, there are {length(new_inv) - sum(new_inv)} invalid polygons."))
  return(new_shp)
}
get_st_ntl <- function(st_yr, fin_yr, st_pref) {
  output <- data.frame()
  for (yr in st_yr:fin_yr) {
    st_shp <- read_sf(glue("SHAPES/Harmonized_NTL/Output/{st_pref}_{yr}.shp"))
    input <- data.frame(shrid=st_shp$shrid2, ntl_mean=st_shp$mean, ntl_median=st_shp$median, ntl_sum=st_shp$sum, year=yr)
    output <- rbind(output, input)
  }
  return(output)
}


setwd("C:/LargeFiles/Research/himalayan_env/")

state = "uttarakhand"
st_prefix = "uk"

shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")
shrid2 <- read_sf("SHAPES/shrug-shrid-poly-shp/shrid2_open.shp")

st_shrids <- subset(shrid_states, state_name==state)
st_shp <- shrid2[shrid2$shrid2 %in% st_shrids$shrid2, ]

st_shp <- fix_invalid(st_shp)


start_yr = 2000
finish_yr = 2020

for (yr in start_yr:finish_yr) {
  if (yr < 2014) {
    sat_code = "calDMSP"
  } else {
    sat_code = "simVIIRS"
  }
  tiff_loc <- glue("SHAPES/Harmonized_NTL/Raw/Harmonized_DN_NTL_{yr}_{sat_code}.tif")
  tif <- rast(tiff_loc)
  
  st_shp <- st_transform(st_shp, crs(tif))
  st_vect <- vect(st_shp)
  crop_tif <- crop(tif, st_vect)
  st_vect$sum <- zonal(crop_tif, st_vect, fun="sum")
  st_vect$mean <- zonal(crop_tif, st_vect, fun="mean")
  st_vect$median <- zonal(crop_tif, st_vect, fun="median")

  writeVector(st_vect, glue("SHAPES/Harmonized_NTL/Output/{st_prefix}_{yr}.shp"))
  print(glue("Wrote layer for {yr}."))
}


hp_ntl <- get_st_ntl(start_yr, finish_yr, "hp")
uk_ntl <- get_st_ntl(start_yr, finish_yr, "uk")
ntl <- rbind(hp_ntl, uk_ntl)
write.csv(ntl, r"(DATA\econ\ntl.csv)")





# ntl_panel <- data.frame(shrid=st_shrids$shrid2)
# eg_shp <- read_sf("SHAPES/Harmonized_NTL/Output/hp_2002.shp")
# eg_shp2 <- read_sf("SHAPES/Harmonized_NTL/Output/hp_2004.shp")
# 
# inter <- eg_shp[eg_shp$shrid2 %in% eg_shp2$shrid2, ]
# nrow(eg_shp2)


# ntl_panel <- data.frame(shrid=eg_shp$shrid2, ntl_mean=eg_shp$mean, ntl_median=eg_shp$median, ntl_sum=eg_shp$sum)
# ntl_panel$year = 2132
