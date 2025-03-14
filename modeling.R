######### Fitting models for reaction norms ##########
######## created 3-13-25 ###########
####### last modified 3-14-25 ##############
######## for bromecast reaction norm paper ########
######## R. Nelson, M. Vahsen, & P. Adler ######
rm(list = ls())

## load required packages
library(dplyr)
library(ggplot2)
library(lme4)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(rjags); library(janitor); library(patchwork); library(lubridate); 
library(loo)

## read
data <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean_climate.csv", header = TRUE)
## note need to merge climat data so that it doesn't have redudnant latitude 
##### Set training data ########
data <- data %>%
  mutate(site_year = paste(site, year))

data$site <- as.factor(data$site)
data$year <- as.factor(data$year)
data$site_year <- as.factor(data$site_year)

data_sat <- data %>% filter(Type == "Satellite")


set.seed(123)  # For reproducibility

selected_categories <- data_sat %>%
  distinct(site_year) %>%  
  slice_sample(n = 36) %>% 
  pull(site_year)          

training_data <- data %>%
  filter(site_year %in% selected_categories)

### compare data to make sure we have decent coverage of climate 
hist(data$MAP)
hist(training_data$MAP)

hist(data$MAT)
hist(training_data$MAT) ##our random sample loses some of the temp extremes

hist(data$mean_seasonality)
hist(training_data$mean_seasonality)

## overal seems fairly representative of the whole 

##### Scale variables ########
training_data$Fecundity.scaled <- scale(training_data$Fecundity)
training_data$MAP.scaled <- scale(training_data$MAP)
training_data$MAT.scaled <- scale(training_data$MAT)
training_data$mean_seasonality.scaled <- scale(training_data$mean_seasonality)

## Emergence
training_data <- subset(training_data, training_data$Emerged!="missing") 
table(training_data$Emerged)
# make Emerged 1s and 0s
training_data$Emerged <- ifelse(training_data$Emerged=="N",0,1)

#Reproduction
training_data <- subset(training_data, training_data$Reproduced!="missing") 
table(training_data$Reproduced)
# make Reproduced 1s and 0s
training_data$Reproduced <- ifelse(training_data$Reproduced=="N",0,1)


####### Build Model: Climate & Cheatgrass Vital Rates  ############

library(lme4)
library(lmerTest)
library(MuMIn)
library(ggeffects)
library(brms)
library(rstan)

####### Emerged ########
### Emerged Frequentist #######
E_null <- glmer(Emerged ~ 1 + (1|site_year),data=training_data,family="binomial")
E_map <- glmer(Emerged ~ MAP.scaled + (1|site_year),data=training_data,family="binomial")
E_mat <- glmer(Emerged ~ MAT.scaled + (1|site_year),data=training_data,family="binomial")
E_seasonality <- glmer(Emerged ~ mean_seasonality.scaled + (1|site_year),data=training_data,family="binomial")
summary(E_seasonality)
## seasonality and MAP significantly affect germination, MAT and null don't 
AIC(E_null, E_map, E_mat, E_seasonality) ## seasonality has lowest AIC but models aren't fitted to same number of observations 

r2_Null <- r.squaredGLMM(E_null)
print(r2_Null) #R2 marginal = 0

r2_map <- r.squaredGLMM(E_map)
print(r2_map)  #R2 marginal = 0.04

r2_mat <- r.squaredGLMM(E_mat)
print(r2_mat)  #R2 marginal = 0.008-0.009

r2_seasonality <- r.squaredGLMM(E_seasonality)
print(r2_seasonality)  #R2 marginal = 0.07-.08

### Emerged Bayesian #####
# using brms

E_null_bayes <- brm(Emerged ~ 1 + (1|site_year), 
                   data = training_data, 
                   family = bernoulli(),   
                   chains = 4,             
                   iter = 4000,            
                   warmup = 2000,          
                   control = list(adapt_delta = 0.95))  
summary(E_null_bayes)
plot(E_null_bayes)
diagnostics <- posterior_summary(E_null_bayes)



### same model in cmdrstan syntax
#tutorial: https://mc-stan.org/cmdstanr/articles/cmdstanr.html

# Define a Stan model (must be written separately)
model_code <- "
data {
  int<lower=0> N;
 array[N] int<lower=0, upper=1> y;
}
parameters {
  real theta;
}
model {
  theta ~ beta(1,1);
  y ~ bernoulli(theta);
}
"
# Save model and compile
writeLines(model_code, "bernoulli_model.stan")
mod <- cmdstan_model("bernoulli_model.stan")

# Prepare data
data_list <- list(N = length(training_data$Emerged), y = training_data$Emerged)

# Run MCMC sampling
fit <- mod$sample(data = data_list, chains = 4, iter_warmup = 2000, iter_sampling = 4000)
#runs and doesn't seem to generate error messages of brms 
# Get summary of all parameters
summary = fit$summary()

# Get all posterior draws for parameters
posterior <- fit$draws()

mcmc_hist(fit$draws("theta"))
str(fit$sampler_diagnostics())
str(fit$sampler_diagnostics(format = "df"))
fit$diagnostic_summary()

mcmc_hist(fit$draws("theta"), binwidth = 0.025) +
  ggplot2::labs(subtitle = "Posterior from MCMC") +
  ggplot2::xlim(0, 1)

E_map_bayes <- brm(Emerged ~ MAP.scaled + (1|site_year), 
                   data = training_data, 
                   family = bernoulli(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(E_map_bayes)
plot(E_map_bayes)
diagnostics <- posterior_summary(E_map_bayes)


 
E_mat_bayes <- brm(Emerged ~ MAT.scaled + (1|site_year), 
                      data = training_data, 
                      family = bernoulli(),   
                      chains = 4,             
                      iter = 2000,            
                      warmup = 1000,          
                      control = list(adapt_delta = 0.95))  
summary(E_mat_bayes)
plot(E_mat_bayes)
diagnostics <- posterior_summary(E_mat_bayes)


E_seasonality_bayes <- brm(Emerged ~ mean_seasonality.scaled + (1|site_year), 
                   data = training_data, 
                   family = bernoulli(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(E_seasonality_bayes)
plot(E_seasonality_bayes)
diagnostics <- posterior_summary(E_seasonality_bayes)

E_null_loo <- loo(E_null_bayes)
E_mat_loo <- loo(E_mat_bayes)
E_map_loo <- loo(E_map_bayes)
E_seasonality_loo <- loo(E_seasonality_bayes)
loo_compare(E_null_loo, E_mat_loo, E_map_loo, E_seasonality_loo) #issues with diff number of data

###### Reproduced #######
### Reproduced Frequentist: ##############
## make reproduction conditional on emergence 
tmp <- which(training_data$Emerged==1)
myReprD <- training_data[tmp,]

R_null <- glmer(Reproduced ~ 1 + (1|site_year),data=myReprD,family="binomial")
R_map <- glmer(Reproduced ~ MAP.scaled + (1|site_year),data=myReprD,family="binomial")
R_mat <- glmer(Reproduced ~ MAT.scaled + (1|site_year),data=myReprD,family="binomial")
R_seasonality <- glmer(Reproduced ~ mean_seasonality.scaled + (1|site_year),data=myReprD,family="binomial")
summary(R_null)
## only seasonality marginally significant (it is significant if you don't condition on emergence)
AIC(R_null, R_map, R_mat, R_seasonality) ## seasonality has lowest AIC but models aren't fitted to same number of observations 

r2_Null <- r.squaredGLMM(R_null)
print(r2_Null) #R2 marginal = 0

r2_map <- r.squaredGLMM(R_map)
print(r2_map)  #R2 marginal = 0.001

r2_mat <- r.squaredGLMM(R_mat)
print(r2_mat)  #R2 marginal = 1.1 x 10-6

r2_seasonality <- r.squaredGLMM(R_seasonality)
print(r2_seasonality)  #R2 marginal = 0.06 (vs 0.14-0.18 if you don't conditon on emergence)

######### Reproduced Bayesian ###########
R_null_bayes <- brm(Reproduced ~ 1 + (1|site_year), 
                    data = myReprD, 
                    family = bernoulli(),   
                    chains = 4,             
                    iter = 2000,            
                    warmup = 1000,          
                    control = list(adapt_delta = 0.95))  
summary(R_null_bayes)
plot(R_null_bayes)
diagnostics <- posterior_summary(R_null_bayes)

R_map_bayes <- brm(Reproduced ~ MAP.scaled + (1|site_year), 
                   data = myReprD, 
                   family = bernoulli(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(R_map_bayes)
plot(R_map_bayes)
diagnostics <- posterior_summary(R_map_bayes)



R_mat_bayes <- brm(Reproduced ~ MAT.scaled + (1|site_year), 
                   data = myReprD, 
                   family = bernoulli(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(R_mat_bayes)
plot(R_mat_bayes)
diagnostics <- posterior_summary(R_mat_bayes)


R_seasonality_bayes <- brm(Reproduced ~ mean_seasonality.scaled + (1|site_year), 
                           data = myReprD, 
                           family = bernoulli(),   
                           chains = 4,             
                           iter = 2000,            
                           warmup = 1000,          
                           control = list(adapt_delta = 0.95))  
summary(R_seasonality_bayes)
plot(R_seasonality_bayes)
diagnostics <- posterior_summary(R_seasonality_bayes)


###### Fecundity #########
#### Fecundity Frequentist:#######
tmp <- which(myReprD$Reproduced==1 & myReprD$Fecundity > 0)
myFecD <- myReprD[tmp,]

# check data
table(myFecD$Fecundity)
sum(is.na(myFecD$Fecundity))


# fit models
F_null <- glmer(Fecundity ~ 1 + (1|site_year),data=myFecD,family="poisson")
F_map <- glmer(Fecundity ~ MAP.scaled + (1|site_year),data=myFecD,family="poisson")
F_mat <- glmer(Fecundity ~ MAT.scaled + (1|site_year),data=myFecD,family="poisson")
F_seasonality <- glmer(Fecundity ~ mean_seasonality.scaled + (1|site_year),data=myFecD,family="poisson")
summary(F_seasonality)
## only seasonality significant
AIC(F_null, F_map, F_mat, F_seasonality) ## seasonality has lowest AIC but models aren't fitted to same number of observations 

r2_Null <- r.squaredGLMM(F_null)
print(r2_Null) #R2 marginal = 0

r2_map <- r.squaredGLMM(F_map)
print(r2_map)  #R2 marginal = 0.007

r2_mat <- r.squaredGLMM(F_mat)
print(r2_mat)  #R2 marginal = .005

r2_seasonality <- r.squaredGLMM(F_seasonality)
print(r2_seasonality)  #R2 marginal = 0.45


###### Fecundity Bayesian #######
F_null_bayes <- brm(Fecundity ~ 1 + (1|site_year), 
                    data = myFecD, 
                    family = poisson(),   
                    chains = 4,             
                    iter = 2000,            
                    warmup = 1000,          
                    control = list(adapt_delta = 0.95))  
summary(F_null_bayes)
plot(F_null_bayes)
diagnostics <- posterior_summary(F_null_bayes)

F_map_bayes <- brm(Fecundity ~ MAP.scaled + (1|site_year), 
                   data = myFecD, 
                   family = poisson(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(F_map_bayes)
plot(F_map_bayes)
diagnostics <- posterior_summary(F_map_bayes)



F_mat_bayes <- brm(Fecundity ~ MAT.scaled + (1|site_year), 
                   data = myFecD, 
                   family = poisson(),   
                   chains = 4,             
                   iter = 2000,            
                   warmup = 1000,          
                   control = list(adapt_delta = 0.95))  
summary(F_mat_bayes)
plot(F_mat_bayes)
diagnostics <- posterior_summary(F_mat_bayes)


F_seasonality_bayes <- brm(Fecundity ~ mean_seasonality.scaled + (1|site_year), 
                           data = myFecD, 
                           family = poisson(),   
                           chains = 4,             
                           iter = 2000,            
                           warmup = 1000,          
                           control = list(adapt_delta = 0.95))  
summary(F_seasonality_bayes)
plot(F_seasonality_bayes)
diagnostics <- posterior_summary(F_seasonality_bayes)



