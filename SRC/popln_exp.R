library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)

setwd("C:/LargeFiles/Research/himalayan_env/")

pca91 <- read.csv("DATA/shrug-pca91-csv/pc91_pca_clean_shrid.csv")
pca01 <- read.csv("DATA/shrug-pca01-csv/pc01_pca_clean_shrid.csv")
pca11 <- read.csv("DATA/shrug-pca11-csv/pc11_pca_clean_shrid.csv")
shrid_states <- read.csv("DATA/shrug-shrid-keys-csv/shrid_loc_names.csv")

thi <- merge(pca01, pca91, by="shrid2")
thi <- merge(thi, pca11, by="shrid2")

thi <- thi[, c("shrid2", "pc91_pca_tot_p", "pc01_pca_tot_p", "pc11_pca_tot_p")]

thi$perc91_01 <- (thi$pc01_pca_tot_p - thi$pc91_pca_tot_p) / thi$pc91_pca_tot_p

thi$perc_yr91_01 <- thi$perc91_01 / 10

thi$pc92_pca_tot_p <- thi$pc91_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc93_pca_tot_p <- thi$pc92_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc94_pca_tot_p <- thi$pc93_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc95_pca_tot_p <- thi$pc94_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc96_pca_tot_p <- thi$pc95_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc97_pca_tot_p <- thi$pc96_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc98_pca_tot_p <- thi$pc97_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc99_pca_tot_p <- thi$pc98_pca_tot_p * (1 + thi$perc_yr91_01)
thi$pc00_pca_tot_p <- thi$pc99_pca_tot_p * (1 + thi$perc_yr91_01)


thi$perc01_11 <- (thi$pc11_pca_tot_p - thi$pc01_pca_tot_p) / thi$pc01_pca_tot_p

thi$perc_yr01_11 <- thi$perc01_11 / 10

thi$pc02_pca_tot_p <- thi$pc01_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc03_pca_tot_p <- thi$pc02_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc04_pca_tot_p <- thi$pc03_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc05_pca_tot_p <- thi$pc04_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc06_pca_tot_p <- thi$pc05_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc07_pca_tot_p <- thi$pc06_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc08_pca_tot_p <- thi$pc07_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc09_pca_tot_p <- thi$pc08_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc10_pca_tot_p <- thi$pc09_pca_tot_p * (1 + thi$perc_yr01_11)

thi$pc12_pca_tot_p <- thi$pc11_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc13_pca_tot_p <- thi$pc12_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc14_pca_tot_p <- thi$pc13_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc15_pca_tot_p <- thi$pc14_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc16_pca_tot_p <- thi$pc15_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc17_pca_tot_p <- thi$pc16_pca_tot_p * (1 + thi$perc_yr01_11)
thi$pc18_pca_tot_p <- thi$pc17_pca_tot_p * (1 + thi$perc_yr01_11)

write.csv(thi, "DATA/final/popln.csv")


hp_shrids <- subset(shrid_states, state_name=="himachal pradesh")
thi_hp <- thi[thi$shrid2 %in% hp_shrids$shrid2, ]
write.csv(thi, "DATA/final/hp_popln.csv")

uk_shrids <- subset(shrid_states, state_name=="uttarakhand")
thi_uk <- thi[thi$shrid2 %in% uk_shrids$shrid2, ]
write.csv(thi, "DATA/final/uk_popln.csv")