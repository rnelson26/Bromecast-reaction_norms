# call after running fit_demography_models.R

## need to figure out what myMassD corresponds to in merged dataset 

library(brms)
library(parallel)

brm_out<- brm(logB ~ perennial + (1 + perennial|site/year), 
             brmsfamily("gaussian"), 
             data = myMassD, 
             chains = 3, #specify the number of Markov chains
             cores = 3,
             iter = 3000, warmup = 1500, thin = 5,
             prior =  c(prior(normal(0, 3), "b"), # set normal prior on regression coefficients (mean of 0, location of 3)
                        prior(normal(0, 3), "Intercept")) # set normal prior on intercept (mean of 0, location of 3)
             ) 

summary(brm_out)
plot(brm_out)

test<-predict(brm_out)
