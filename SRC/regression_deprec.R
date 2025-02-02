library(glue)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(plm)
library(tidyverse)
library(lme4)


setwd("C:/LargeFiles/Research/himalayan_env/")



check_unique_pairs <- function(data, x_col, y_col) {
  # Create a new column for the pairs
  data$pair <- with(data, paste(data[[x_col]], data[[y_col]]))
  
  # Count occurrences of each pair
  pair_counts <- as.data.frame(table(data$pair))
  
  # Filter for non-unique pairs (count > 1)
  non_unique_pairs <- pair_counts[pair_counts$Freq > 1, ]
  
  # Return the non-unique pairs with their counts
  return(non_unique_pairs)
}



standard_scaler <- function(x) {
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  scaled_x <- (x - x_mean) / x_sd
  return(scaled_x)
}




# ECON VARIABLES

dmsp_hp <- read.csv("DATA/econ/dmsp_hp.csv")
dmsp_uk <- read.csv("DATA/econ/dmsp_uk.csv")
dmsp <- rbind(dmsp_hp, dmsp_uk)
dmsp <- dmsp[, c("shrid2", "dmsp_mean_light", "year")]
dmsp <- unique(dmsp)

viirs_hp <- read.csv("DATA/econ/viirs_hp.csv")
viirs_uk <- read.csv("DATA/econ/viirs_uk.csv")
viirs <- rbind(viirs_hp, viirs_uk)
viirs <- viirs[, c("shrid2", "viirs_annual_mean", "year")]

# dmsp$mean_light <- standard_scaler(dmsp$dmsp_mean_light)
# viirs$mean_light <- standard_scaler(viirs$viirs_annual_mean)

colnames(dmsp) <- c("shrid2", "mean_light", "year")
colnames(viirs) <- c("shrid2", "mean_light", "year")

dmsp <- subset(dmsp, year<2012)

econ <- rbind(dmsp, viirs)
econ_unique <- unique(econ)


check_unique_pairs(dmsp, "shrid2", "year")

subset(dmsp, shrid2=="11-02-023-00085-007193")



# ENV Variables

pm25_hp <- read.csv("DATA/env/pm25_hp.csv")
pm25_uk <- read.csv("DATA/env/pm25_uk.csv")
pm25 <- rbind(pm25_hp, pm25_uk)
pm25 <- pm25[, c("shrid2", "pm25_mean", "year")]

vcf_hp <- read.csv("DATA/env/vcf_hp.csv")
vcf_uk <- read.csv("DATA/env/vcf_uk.csv")
vcf <- rbind(vcf_hp, vcf_uk)
vcf <- vcf[, c("shrid2", "vcf_mean", "year")]

# traced issue to vcf file, extra shrids found

env <- merge(pm25, vcf, by=c("shrid2", "year"))

length(unique(env$shrid2))




# PANEL DATA merging

pdata <- merge(econ, env, by=c("shrid2", "year"))
pdata <- pdata.frame(pdata, index = c("shrid2", "year"))

table(index(pdata), useNA = "ifany")

pdata_unique <- pdata %>%
  distinct(shrid2, year, .keep_all = TRUE)

pdata_unique_nona <- na.omit(pdata_unique, cols = "mean_light")
pdata_unique_na_log <- subset(pdata_unique_nona, mean_light!=0)






# FIXED EFFECTS REGRESSION

fe_model <- plm(mean_light ~ pm25_mean + vcf_mean, data = pdata_unique_nona, model = "within")
fe_logmodel <- plm(log(mean_light) ~ log(pm25_mean) + log(vcf_mean), data = pdata_unique_na_log, model = "within")

summary(fe_model)$coefficients
summary(fe_logmodel)


reg_table <- summary(fe_logmodel)$coefficients
write.csv(reg_table, "DOC/tables/fe_log.csv")


# RANDOM EFFECTS

re_model <- plm(mean_light ~ pm25_mean + vcf_mean, data = pdata_unique_nona, model = "random")
re_logmodel <- plm(log(mean_light) ~ log(pm25_mean) + log(vcf_mean), data = pdata_unique_na_log, model = "random")

summary(re_model)
summary(re_logmodel)



# HAUSMAN TEST

phtest(fe_model, re_model)
phtest(fe_logmodel, re_logmodel)





# ENVINDEX

env$envindex <- (0.5*env$pm25_mean) + (0.5*env$vcf_mean)
env <- env[, c("envindex", "year", "shrid2")]

pdata <- merge(econ, env, by=c("shrid2", "year"))
# pdata <- pdata.frame(pdata, index = c("shrid2", "year"))

pdata_unique <- pdata %>%
  distinct(shrid2, year, .keep_all = TRUE)

pdata_unique_nona <- na.omit(pdata_unique, cols = "mean_light")
pdata_unique_na_log <- subset(pdata_unique_nona, mean_light!=0)

fe_model <- plm(mean_light ~ envindex, data = pdata_unique_nona, model = "within")
fe_logmodel <- plm(log(mean_light) ~ log(envindex), data = pdata_unique_na_log, model = "within")

summary(fe_model)
summary(fe_logmodel)

pdata_unique_na_log









env$envindex_sq <- env$envindex * env$envindex

pdata <- merge(dmsp, env, by=c("shrid2", "year"))
# pdata <- pdata.frame(pdata, index = c("shrid2", "year"))

pdata_unique <- pdata %>%
  distinct(shrid2, year, .keep_all = TRUE)

pdata_unique_nona <- na.omit(pdata_unique, cols = "mean_light")
pdata_unique_na_log <- subset(pdata_unique_nona, mean_light!=0)


fe_model <- plm(mean_light ~ envindex, data = pdata_unique_nona, model = "within")
fe_logmodel <- plm(log(mean_light) ~ log(envindex), data = pdata_unique_na_log, model = "within")

fesq_model <- plm(mean_light ~ envindex + envindex_sq, data = pdata_unique_nona, model = "within")
fesq_logmodel <- plm(log(mean_light) ~ log(envindex) + log(envindex_sq), data = pdata_unique_na_log, model = "within")


summary(fesq_model)
summary(fesq_logmodel)




# VISUALIZING

fitted_values <- fitted(fesq_model)
length(fitted_values) - length(pdata_unique_nona$mean_light)

model_summ <- broom::tidy(fesq_model)

ggplot(model_summ, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Fixed Effects Model Coefficients",
       x = "Coefficient",
       y = "Estimate") +
  theme_minimal()






fixed_effects <- fixef(fesq_model)
fe_df <- data.frame(
  id = names(fixed_effects),
  effect = fixed_effects
)

ggplot(fe_df, aes(x = reorder(id, effect), y = effect)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Fixed Effects by Individual",
       x = "Individual",
       y = "Fixed Effect") +
  theme_minimal()



# Create a scatter plot of actual vs. fitted values
plot(fitted_values, pdata_unique_nona$envindex,
     main = "Actual vs. Fitted Values",
     xlab = "Actual Values",
     ylab = "Fitted Values",
     pch = 19,
     col = "blue")
abline(a = 0, b = 1, col = "red") # Add a 45-degree reference line




ggplot(pdata_unique_nona, aes(x = pdata_unique_nona$envindex, y = fitted_values)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "X-axis", y = "Y-axis")





# VISUALIZATIONS

quadratic_function <- function(x) {
  (0.0073735 * x^2) + (-0.2366354 * x)
}

x_values <- seq(-190, 230, by = 0.1)
y_values <- quadratic_function(x_values)

df <- data.frame(x = x_values, y = y_values)

plot(df$x, df$y, type = "l", col = "blue", lwd = 2,
     main = "Relation predicted",
     xlab = "EconIndex", ylab = "EnvIndex", )







# KUZNETS


dmsp$mean_light_sq <- dmsp$mean_light * dmsp$mean_light
viirs$mean_light_sq <- viirs$mean_light * viirs$mean_light


pdata <- merge(viirs, env, by=c("shrid2", "year"))
# pdata <- pdata.frame(pdata, index = c("shrid2", "year"))

pdata_unique <- pdata %>%
  distinct(shrid2, year, .keep_all = TRUE)

pdata_unique_nona <- na.omit(pdata_unique, cols = "mean_light")
pdata_unique_na_log <- subset(pdata_unique_nona, mean_light!=0)


kuzfe_model <- plm(envindex ~ mean_light, data = pdata_unique_nona, model = "within")
kuzfe_logmodel <- plm(log(envindex) ~ log(mean_light), data = pdata_unique_na_log, model = "within")

kuzfesq_model <- plm(envindex ~ mean_light + mean_light_sq, data = pdata_unique_nona, model = "within")
kuzfesq_logmodel <- plm(log(mean_light) ~ log(envindex) + log(envindex_sq), data = pdata_unique_na_log, model = "within")

summary(kuzfesq_model)

ggsave("DOC/images/kuznets.png", width = 8, height = 5, units = "in", dpi = 300)






# CREATING RANDOM


LST <- rnorm(139194, mean = 25.123124, sd = 7.48932)

ghsl_hp <- st_read("SHAPES/GHSL_BUILT/final/hp_2005.shp")
mean(ghsl_hp$X_mean)
sd(ghsl_hp$X_mean)
summary(pm25$pm25_mean)

length(unique(pdata_unique_nona$shrid2))
