rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(stringr) 
library(brms)
library(bayesplot)
library(rstan)
library(coda)
library(dagitty)
library(truncnorm)
library(magrittr)
library(purrr)
library(forcats)
library(modelr)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(distributional)

##########################################################
#######          Read cleaned data                 #######
##########################################################
setwd("/Users/haneuljang/Desktop")

d = read.csv("hh_decision_cleaned.csv", header=T, sep=",")

################################################################################
#######                     Bandongo VS BaYaka women                     #######
################################################################################
d_eth = dlong[dlong$Sex == "Female",]

table(d_eth$response, useNA="always")

## make response a factor and order levels according to the level of women autonomy:
d_eth$response=ordered(d_eth$response, levels=c("Me", "Together","Partner","Others"))

d_eth$score[d_eth$response == "Me"] = 2 
d_eth$score[d_eth$response == "Together"] = 1  
d_eth$score[d_eth$response == "Others"] = 0
d_eth$score[d_eth$response == "Partner"] = 0

table(d_eth$score, useNA="always")

###############################################################
####    Model 1:  Women autonomy score ~ ethnicity*domain  ####
###############################################################
d_eth$score=ordered(as.factor(d_eth$score))

get_prior(score ~ Ethnicity*domain + (1 | ind_id), data=d_eth, family=cumulative())

m0 <- brm(score ~ (1 | ind_id),
          data=d_eth, 
          family=cumulative(),
          prior = c(
            set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("student_t(3, 0, 2.5)", class = "sd")), 
          iter = 2000, warmup = 1000, cores = 1, chains = 4)

m1 <- brm(score ~ Ethnicity*domain2 +
            (1 | ind_id),
          data=d_eth, 
          family=cumulative("logit"),
          prior = c(
            set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("student_t(3, 0, 2.5)", class = "sd")),
          iter = 2000, warmup = 1000, cores = 1, chains = 4)

summary(m1)
conditional_effects(m1, effects = "Ethnicity:domain2")


###### Model comparison ######
m0 <- add_criterion(m0, criterion = "loo")
m1 <- add_criterion(m1, criterion = "loo") 

loo::loo_compare(loo(m0), loo(m1))

# Model weights interpreted as the probability that model will perform best on new data (compared to other models being compared)
loo::loo_model_weights(list(m0 = loo(m0), m1 = loo(m1)))


###### Posterior predictive checks ########
#The dark line is the observed distribution whereas the light lines are draws from the posterior.
pp_check(m1, type = "dens_overlay") 

# Empirical cumulative density function
pp_check(m1, type = "ecdf_overlay") 

# We can also check the model predictions by group witin the data
pp_check(m1, type = "dens_overlay_grouped", group = "Ethnicity") 
pp_check(m1, type = "dens_overlay_grouped", group = "domain2") 

# check for focal Id
pp_check(m1, type = "dens_overlay_grouped", group = "ind_id") 

# ECDF plots for each focal woman 
pp_check(m1, type = "ecdf_overlay_grouped", group = "ind_id") 


################################################################################
#######                      BaYaka WOMEN versus MEN                     #######
################################################################################
d_sex = dlong[dlong$Ethnicity == "bayaka",]

table(d_sex$response, useNA="always")

## make response a factor and order levels according to the level of women autonomy:
d_sex$response=ordered(d_sex$response, levels=c("Me", "Together","Partner","Others"))

d_sex$score[d_sex$response == "Me" & d_sex$Sex == "Female"] = 2 
d_sex$score[d_sex$response == "Together" & d_sex$Sex == "Female"] = 1  
d_sex$score[d_sex$response == "Others" & d_sex$Sex == "Female"] = 0
d_sex$score[d_sex$response == "Partner" & d_sex$Sex == "Female"] = 0

d_sex$score[d_sex$response == "Me" & d_sex$Sex == "Male"] = 0 
d_sex$score[d_sex$response == "Together" & d_sex$Sex == "Male"] = 1  
d_sex$score[d_sex$response == "Others" & d_sex$Sex == "Male"] = 0
d_sex$score[d_sex$response == "Partner" & d_sex$Sex == "Male"] = 2

table(d_sex$score, useNA="always")

#########################################################
####    Model 2:  Women autonomy score ~ sex*domain  ####
#########################################################
d_sex$score=ordered(as.factor(d_sex$score))

get_prior(score ~ Sex*domain2 + (1 | ind_id), data=d_sex, family=cumulative())

m0_sex <- brm(score ~ (1 | ind_id),
              data=d_sex, 
              family=cumulative(),
              prior = c(
                set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
                set_prior("student_t(3, 0, 2.5)", class = "sd")), 
              iter = 2000, warmup = 1000, cores = 1, chains = 4)

m2 <- brm(score ~ Sex*domain2 +
            (1 | ind_id),
          data=d_sex, 
          family=cumulative("logit"),
          prior = c(
            set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("student_t(3, 0, 2.5)", class = "sd")),
          iter = 2000, warmup = 1000, cores = 1, chains = 4)

summary(m2)
conditional_effects(m2)

m2.1 <- brm(score ~ Sex + domain2 +
              (1 | ind_id),
            data=d_sex, 
            family=cumulative("logit"),
            prior = c(
              set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
              set_prior("student_t(3, 0, 2.5)", class = "sd")),
            iter = 2000, warmup = 1000, cores = 1, chains = 4)

summary(m2.1)
conditional_effects(m2.1)

###### Model comparison ######
m0_sex <- add_criterion(m0_sex, criterion = "loo")
m2 <- add_criterion(m2, criterion = "loo") 
m2.1 <- add_criterion(m2.1, criterion = "loo") 

loo::loo_compare(loo(m0_sex), loo(m2))
loo::loo_compare(loo(m0_sex), loo(m2.1))
loo::loo_compare(loo(m2), loo(m2.1))

# Model weights interpreted as the probability that model will perform best on new data (compared to other models being compared)
loo::loo_model_weights(list(m0_sex = loo(m0_sex), m2 = loo(m2), m2.1 = loo(m2.1)))

###### Posterior predictive checks ########
#The dark line is the observed distribution whereas the light lines are draws from the posterior.
pp_check(m2, type = "dens_overlay") 
pp_check(m2.1, type = "dens_overlay") 

# Empirical cumulative density function
pp_check(m2, type = "ecdf_overlay") 
pp_check(m2.1, type = "dens_overlay") 

# We can also check the model predictions by group witin the data
pp_check(m2, type = "dens_overlay_grouped", group = "Sex") 
pp_check(m2.1, type = "dens_overlay_grouped", group = "domain2") 

# check for focal Id
pp_check(m2, type = "dens_overlay_grouped", group = "ind_id") 
pp_check(m2.1, type = "dens_overlay_grouped", group = "ind_id") 

# ECDF plots for each focal woman 
pp_check(m2, type = "ecdf_overlay_grouped", group = "ind_id") 
pp_check(m2.1, type = "ecdf_overlay_grouped", group = "ind_id") 


