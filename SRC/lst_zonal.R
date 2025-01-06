library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(dplyr)
library(tmap)
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
get_st_lst <- function(yrs, st_pref) {
  output <- data.frame()
  for (yr in yrs) {
    st_shp <- read_sf(glue("SHAPES/lst/shp/{st_pref}_{yr}.shp"))
    input <- data.frame(shrid=st_shp$shrid2, lst_mean=st_shp$mean, lst_median=st_shp$median, lst_sum=st_shp$sum, year=yr)
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
rm(shrid_states, shrid2)


years <- c(2000:2020)

for (yr in years) {
  tiff_loc <- glue("SHAPES/lst/outputs/{st_prefix}_{yr}.tif")
  tif <- rast(tiff_loc)
  
  if (crs(st_shp) != crs(tif)) {
    st_shp <- st_transform(st_shp, crs(tif))
    st_shp <- fix_invalid(st_shp)
  }
  st_vect <- vect(st_shp)
  crop_tif <- crop(tif, st_vect)
  st_vect$sum <- zonal(crop_tif, st_vect, fun="sum")
  st_vect$mean <- zonal(crop_tif, st_vect, fun="mean")
  st_vect$median <- zonal(crop_tif, st_vect, fun="median")
  
  writeVector(st_vect, glue("SHAPES/lst/shp/{st_prefix}_{yr}.shp"))
  print(glue("Wrote layer for {yr}."))
}


hp_lst <- get_st_lst(years, "hp")
uk_lst <- get_st_lst(years, "uk")
lst <- rbind(hp_lst, uk_lst)
write.csv(lst, r"(DATA\env\lst.csv)")


lst <- read.csv(r"(DATA\env\lst.csv)")
plot_lst <- subset(lst, lst$year==2009)
ggplot(plot_lst, aes(x=lst_median)) + geom_histogram(binwidth=.5) + scale_x_continuous(limits = c(260, 320))

for (yr in years) {
  plot_lst <- subset(lst, lst$year==yr)
  print(median(plot_lst$lst_median, na.rm=TRUE))
}

means <- sapply(years, function(x) median(subset(lst, lst$year==x)$lst_median, na.rm=TRUE))

ggplot(lst, aes(x = year, y = mean(lst_mean, na.rm=TRUE))) + geom_point()
