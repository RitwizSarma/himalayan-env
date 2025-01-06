library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(dplyr)
library(tmap)
# library(raster)
library(terra)


fix_invalid <- function(shp) {
  inv <- st_is_valid(shp)
  num_inv <- length(inv) - sum(inv)
  if (num_inv == 0) {
    return(shp)
  }
  print(glue("There are {num_inv} invalid polygons."))
  new_shp <- st_make_valid(shp)
  new_inv <- st_is_valid(new_shp)
  print(glue("After fixing, there are {length(new_inv) - sum(new_inv)} invalid polygons."))
  return(new_shp)
}
get_st_ghsl <- function(yrs, st_pref) {
  output <- data.frame()
  for (yr in yrs) {
    st_shp <- read_sf(glue("SHAPES/GHSL_BUILT/outputs/{st_pref}_{yr}.shp"))
    input <- data.frame(shrid=st_shp$shrid2, ghsl_mean=st_shp$mean, ghsl_median=st_shp$median, ghsl_sum=st_shp$sum, year=yr)
    output <- rbind(output, input)
  }
  return(output)
}


setwd("C:/LargeFiles/Research/himalayan_env/")


state = "himachal pradesh"
st_prefix = "hp"

shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")
shrid2 <- read_sf("SHAPES/shrug-shrid-poly-shp/shrid2_open.shp")

st_shrids <- subset(shrid_states, state_name==state)
st_shp <- shrid2[shrid2$shrid2 %in% st_shrids$shrid2, ]

st_shp <- fix_invalid(st_shp)
rm(shrid_states, shrid2)


years <- c(2000,2005,2010,2015,2020)
# years <- c(2010,2015,2020)

for (yr in years) {
  tiff_loc <- glue("SHAPES/GHSL_BUILT/merged_mollweide/ghs_raw_{yr}.tif")
  tif <- rast(tiff_loc)
  
  st_shp <- st_transform(st_shp, crs(tif))
  st_vect <- vect(st_shp)
  crop_tif <- crop(tif, st_vect)
  st_vect$sum <- zonal(crop_tif, st_vect, fun="sum")
  st_vect$mean <- zonal(crop_tif, st_vect, fun="mean")
  st_vect$median <- zonal(crop_tif, st_vect, fun="median")
  
  writeVector(st_vect, glue("SHAPES/GHSL_BUILT/outputs/{st_prefix}_{yr}.shp"))
  print(glue("Wrote layer for {yr}."))
}


hp_ghsl <- get_st_ghsl(years, "hp")
uk_ghsl <- get_st_ghsl(years, "uk")
ghsl <- rbind(hp_ghsl, uk_ghsl)
write.csv(ghsl, r"(DATA\econ\ghsl.csv)")

summary(ghsl$ghsl_median)




