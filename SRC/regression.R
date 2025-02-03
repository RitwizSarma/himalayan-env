library(glue)
library(ggplot2)
library(dplyr)
library(stringr)
library(plm)
library(lmtest)
library(tidyverse)
library(lme4)
library(psych)
library(stargazer)


# CUSTOM FUNCTIONS
# gen_regtable <- function(s.error = FALSE, title = NULL, fname = "default", ...) {
#   require(glue)
#   star <- stargazer(..., omit.stat=c("LL","ser", "f", "adj.rsq"), title=title)
#   cat(star, file=glue("DOC/tables/reg_outp_{fname}.tex"), sep = '\n')
# }
gen_regtable <- function(star, fname="default") {
  require(glue)
  cat(star, file=glue("himalayan-env/DOC/tables/reg_outp_{fname}.tex"), sep = '\n')
}
standard_scaler <- function(x) {
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  scaled_x <- (x - x_mean) / x_sd
  return(scaled_x)
}
get_pca_tests <- function(df, n=10000) {
  print(cortest.bartlett(cor(df), n = n))
  KMO(cor(df))
}


# WORKING DIRECTORY
setwd("C:/LargeFiles/Research/himalayan_env/")


# BUILDING INDICATOR DATAFRAMES
ntl_csv <- read.csv("DATA/econ/ntl.csv")
ghsl_csv <- read.csv("DATA/econ/ghsl.csv")
lst_csv <- read.csv("DATA/env/lst.csv")

pm25_hp <- read.csv("DATA/env/pm25_hp.csv")
pm25_uk <- read.csv("DATA/env/pm25_uk.csv")
pm25_csv <- rbind(pm25_hp, pm25_uk)
vcf_hp <- read.csv("DATA/env/vcf_hp.csv")
vcf_uk <- read.csv("DATA/env/vcf_uk.csv")
vcf_csv <- rbind(vcf_hp, vcf_uk)

vcf_csv$vcfd_mean <- 100 - vcf_csv$vcf_mean

ntl <- ntl_csv[, c("shrid", "ntl_mean", "year")]
ghsl <- ghsl_csv[, c("shrid", "ghsl_mean", "year")]
lst <- lst_csv[, c("shrid", "lst_median", "year")]
pm25 <- pm25_csv %>%
  select(shrid2, pm25_mean, year) %>%
  rename(shrid = shrid2)
vcf <- vcf_csv %>%
  select(shrid2, vcfd_mean, year) %>%
  rename(shrid = shrid2)

ntl <- ntl[ntl$shrid %in% pm25$shrid, ]
ghsl <- ghsl[ghsl$shrid %in% pm25$shrid, ]
lst <- lst[lst$shrid %in% pm25$shrid, ]


# BUILDING COMBINED DATAFRAMES
econ <- ntl
econ <- merge(ntl, ghsl, by=c("shrid", "year"))
env <- merge(pm25, vcf, by=c("shrid", "year"))
env <- merge(env, lst, by=c("shrid", "year"))
env <- na.omit(env)


# BUILDING INDICES
env$vcfd_mean_st <- standard_scaler(env$vcfd_mean)
env$lst_median_st <- standard_scaler(env$lst_median)
env$pm25_mean_st <- standard_scaler(env$pm25_mean)
econ$ntl_mean_st <- standard_scaler(econ$ntl_mean)
econ$ghsl_mean_st <- standard_scaler(econ$ghsl_mean)

cor(env[c("lst_median", "vcfd_mean", "pm25_mean")])
cor(econ$ntl_mean, econ$ghsl_mean)

econ_vars <- econ %>% select(ntl_mean_st, ghsl_mean_st)
env_vars <- env %>% select(vcfd_mean_st, pm25_mean_st)
env_vars <- env %>% select(vcfd_mean_st, pm25_mean_st, lst_median_st)
get_pca_tests(env_vars)


# BUILDING VARIABLE DATAFRAMES
econ$econ_index <- (0.5*econ$ntl_mean) + (0.5*econ$ghsl_mean)
env$env_index <- (0.5*env$pm25_mean_st) + (0.5*env$vcfd_mean_st)

econ_pca <- princomp(econ_vars, cor = TRUE, scores = TRUE)
econ_indices <- econ_pca$scores
econ_index <- econ_indices[, "Comp.1"]
econ$econ_index <- econ_index

env_pca <- princomp(env_vars, cor = TRUE, scores = TRUE)
env_indices <- env_pca$scores
env_index <- env_indices[, "Comp.1"]
env$env_index <- env_index

env$env_index_sq <- env$env_index * env$env_index

econ_orig <- econ
env_orig <- env
rm(env_pca, env_indices)

# SELECTING SAMPLE
# env <- env[env$year > 1999 & env$year < 2021, ]

# econ <- econ[econ$year > 2000 & econ$year < 2011, ]
# env <- env[env$year > 2000 & env$year < 2011, ]
econ <- econ[econ$year > 2010 & econ$year < 2021, ]
env <- env[env$year > 2010 & env$year < 2021, ]

# econ <- econ[econ$year %in% c(2000,2005,2010,2015,2020), ]
# env <- env[env$year %in% c(2001,2005,2010,2015,2020), ]

# econ <- econ[econ$year %in% c(2020, 2015), ]
# env <- env[env$year %in% c(2020, 2015), ]

# econ <- econ[econ$year %in% c(2010, 2005, 2001), ]
# env <- env[env$year %in% c(2010, 2005, 2001), ]
# econ$year[econ$year == 2001] <- 2000
# env$year[env$year == 2001] <- 2000


econ <- econ[econ$shrid %in% env$shrid, ]


pdata1 <- merge(econ, env, by=c("shrid", "year"))
pdata1 <- pdata.frame(pdata1, index = c("shrid", "year"))
pdata2 <- merge(econ, env, by=c("shrid", "year"))
pdata2 <- pdata.frame(pdata2, index = c("shrid", "year"))
pdata3 <- merge(econ, env, by=c("shrid", "year"))
pdata3 <- pdata.frame(pdata3, index = c("shrid", "year"))

length(unique(econ$year))


fe_model <- plm(ntl_mean ~ env_index + env_index_sq, data = pdata1, model = "within")
coeftest(fe_model, function(x) vcovHC(x, type = 'HC1'))
re_model <- plm(ntl_mean ~ env_index + env_index_sq, data = pdata1, model = "random")
coeftest(re_model, function(x) vcovHC(x, type = 'HC1'))
# fe_logmodel <- plm(log(ntl_mean+0.00000001) ~ log(env_index+0.00000001), data = pdata1, model = "within")
# re_logmodel <- plm(log(ntl_mean+0.00000001) ~ log(env_index+0.00000001), data = pdata1, model = "random")
summary(fe_model)
summary(re_model)

fe_model2 <- plm(econ_index ~ env_index + env_index_sq, data = pdata2, model = "within")
coeftest(fe_model2, function(x) vcovHC(x, type = 'HC1'))
re_model2 <- plm(econ_index ~ env_index + env_index_sq, data = pdata2, model = "random")
coeftest(re_model2, function(x) vcovHC(x, type = 'HC1'))
summary(fe_model2)
summary(re_model2)

fe_model3 <- plm(econ_index ~ env_index + env_index_sq, data = pdata3, model = "within")
coeftest(fe_model3, function(x) vcovHC(x, type = 'HC1'))
re_model3 <- plm(econ_index ~ env_index + env_index_sq, data = pdata3, model = "random")
coeftest(re_model3, function(x) vcovHC(x, type = 'HC1'))
summary(fe_model3)
summary(re_model3)


phtest(fe_model, re_model)
phtest(fe_model2, re_model2)
phtest(fe_model3, re_model3)


fe_cov.mat <- vcovHC(fe_model, type="HC1")
fe2_cov.mat <- vcovHC(fe_model2, type="HC1")
fe3_cov.mat <- vcovHC(fe_model3, type="HC1")

gen_regtable(fe_model2, fe_model3, fname = "mod1_stdized_epoch1", title="Regression results from Model 1 for Epoch 1.", s.error=c(sqrt(diag(fe2_cov.mat)), sqrt(diag(fe3_cov.mat))))



data <- merge(econ, env, by=c("shrid", "year"))
data <- merge(data, ghsl, by=c("shrid", "year"), all.x=TRUE)
desc_stats <- stargazer(data)
cat(desc_stats,file = "himalayan-env/DOC/tables/desc_stats.tex", sep="\n")



pdata1 <- merge(econ, env, by=c("shrid", "year"))
pdata1 <- pdata.frame(pdata1, index = c("shrid", "year"))

fe_model <- plm(ntl_mean_st ~ env_index + env_index_sq, data = pdata1, model = "within")
coeftest(fe_model, function(x) vcovHC(x, type = 'sss', cluster="group"))
re_model <- plm(ntl_mean_st ~ env_index + env_index_sq, data = pdata1, model = "random")
coeftest(re_model, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_model, re_model)


pdt1 <- merge(econ, env, by=c("shrid", "year"))
pdt1 <- pdt1 %>% rename(VCF_PM_PC = env_index, VCF_PM_PC2 = env_index_sq)
pdt1 <- pdata.frame(pdt1, index = c("shrid", "year"))

fe_mod1 <- plm(ntl_mean_st ~ VCF_PM_PC + VCF_PM_PC2, data = pdt1, model = "within")
coeftest(fe_mod1, function(x) vcovHC(x, type = 'sss', cluster="group"))
re_mod1 <- plm(ntl_mean ~ VCF_PM_PC + VCF_PM_PC2, data = pdt1, model = "random")
coeftest(re_mod1, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_mod1, re_mod1)


pdt2 <- merge(econ, env, by=c("shrid", "year"))
pdt2 <- pdt2 %>% rename(VCF_LST_UW = env_index, VCF_LST_UW2 = env_index_sq)
pdt2 <- pdata.frame(pdt2, index = c("shrid", "year"))

fe_mod2 <- plm(ntl_mean_st ~ VCF_LST_UW + VCF_LST_UW2, data = pdt2, model = "within")
coeftest(fe_mod2, function(x) vcovHC(x, type = 'HC1'))
re_mod2 <- plm(ntl_mean_st ~ VCF_LST_UW + VCF_LST_UW2, data = pdt2, model = "random")
coeftest(re_mod2, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_mod2, re_mod2)


pdt3 <- merge(econ, env, by=c("shrid", "year"))
pdt3 <- pdata.frame(pdt3, index = c("shrid", "year"))

fe_mod3 <- plm(ntl_mean_st ~ env_index + env_index_sq, data = pdt3, model = "within")
coeftest(fe_mod3, function(x) vcovHC(x, type = 'sss', cluster="group"))
re_mod3 <- plm(ntl_mean_st ~ env_index + env_index_sq, data = pdt3, model = "random")
coeftest(re_mod3, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_mod3, re_mod3)


pdt4 <- merge(econ, env, by=c("shrid", "year"))
pdt4 <- pdt4 %>% rename(VCF_PM_PC = env_index, VCF_PM_PC2 = env_index_sq)
pdt4 <- pdata.frame(pdt4, index = c("shrid", "year"))

fe_mod4 <- plm(ntl_mean_st ~ VCF_PM_PC + VCF_PM_PC2, data = pdt4, model = "within")
coeftest(fe_mod4, function(x) vcovHC(x, type = 'sss', cluster="group"))
re_mod4 <- plm(ntl_mean ~ VCF_PM_PC + VCF_PM_PC2, data = pdt4, model = "random")
coeftest(re_mod4, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_mod4, re_mod4)


pdt5 <- merge(econ, env, by=c("shrid", "year"))
pdt5 <- pdt5 %>% rename(VCF_LST_UW = env_index, VCF_LST_UW2 = env_index_sq)
pdt5 <- pdata.frame(pdt5, index = c("shrid", "year"))

fe_mod5 <- plm(ntl_mean_st ~ VCF_LST_UW + VCF_LST_UW2, data = pdt5, model = "within")
coeftest(fe_mod5, function(x) vcovHC(x, type = 'HC1'))
re_mod5 <- plm(ntl_mean_st ~ VCF_LST_UW + VCF_LST_UW2, data = pdt5, model = "random")
coeftest(re_mod5, function(x) vcovHC(x, type = 'HC1'))
phtest(fe_mod5, re_mod5)



fe_cov.mat <- vcovHC(fe_model, type="sss", cluster="group")
fe2_cov.mat <- vcovHC(fe_mod1, type="sss", cluster="group")
fe3_cov.mat <- vcovHC(fe_mod2, type="sss", cluster="group")
fe4_cov.mat <- vcovHC(fe_mod3, type="sss", cluster="group")
fe5_cov.mat <- vcovHC(fe_mod4, type="sss", cluster="group")
fe6_cov.mat <- vcovHC(fe_mod5, type="sss", cluster="group")
st <- stargazer(fe_model, fe_mod1, fe_mod2, fe_mod3, fe_mod4, fe_mod5, omit.stat=c("LL","ser", "f", "adj.rsq"), title="Regression results from Model 1.", digits = 3, se = list(fe_cov.mat, fe2_cov.mat, fe3_cov.mat, fe4_cov.mat, fe5_cov.mat, fe6_cov.mat))
gen_regtable(st, fname = "mod1_stdized_epoch12")
