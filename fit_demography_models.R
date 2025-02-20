
######## fit_demography_models for merged data #######
###### modifies code from fit_demography_models.R written by Peter Adler for satellite sites #####
####### last modified: 2-20-25 by Becca Nelson ########

## this code has been modified to match column names for the combined cg and sat data. The climate variables in the models have been modified to include winter precipitaiton and winter mean temp, the two climate variables I randomly selected from the satellite data with which to test our model 


### outstanding questions ##########

## which variables should be rescaled?
## poisson models appear not to converge. What to do? 


### read in merged dataset
allD <- read.csv("/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/combined_clean.csv", header = TRUE)


# call from run_everything.R

# prep data

allD <- subset(allD, allD$Emerged!="missing") # remove missing data
table(allD$Emerged)

# make Emerged 1s and 0s
allD$Emerged <- ifelse(allD$Emerged=="N",0,1)

# convert site to factor
allD$site <- as.factor(allD$site)

# convert Year to factor
allD$year <- as.factor(allD$year)


### analyze probability of emergence

# check data
table(allD$Emerged)
sum(is.na(allD$Emerged))

### scale and center covariates (modified Peter's code to only do this for numeric columns)
## question: are there only specific columns we want to rescale? 

# Filter numeric columns
numeric_columns <- sapply(allD[, rescale_columns], is.numeric)
rescale_columns <- rescale_columns[numeric_columns]

# Compute means and standard deviations for numeric columns only
raw_means <- colMeans(allD[, rescale_columns], na.rm = TRUE)
raw_sds <- apply(allD[, rescale_columns], 2, sd, na.rm = TRUE)

# Rescale (standardize) the numeric columns
allD[, rescale_columns] <- (allD[, rescale_columns] - 
                              matrix(raw_means, nrow = nrow(allD), ncol = length(raw_means), byrow = TRUE)) /
  matrix(raw_sds, nrow = nrow(allD), ncol = length(raw_sds), byrow = TRUE)

#### run models

library(lme4)
library(lmerTest)

# fit models
E_null <- glmer(Emerged ~ 1 + (1|site/year),data=allD,family="binomial")
#E_null <- gam(Emerged ~ s(SiteCode, bs="re") + s(SiteCode,Year, bs="re"),data=allD,family="binomial",method="REML")


# Does composition matter? Remove records without composition data
allDcomp <- subset(allD, !is.na(allD$annual))
E_trt <- glmer(Emerged ~ Treatment + (1|site/year),data=allD,family="binomial")
# No treatment effect
rm(E_trt)

## include some climate correlates
# E_comp <- glmer(Emerged ~ prcp.Fall + tmean.Fall + (1|SiteCode/Year),data=allD,family="binomial")

### analyze prob of reproduction conditional on emergence

tmp <- which(allD$Emerged==1)
myReprD <- allD[tmp,]

myReprD <- subset(myReprD, myReprD$Reproduced!="missing")
table(myReprD$Reproduced)
sum(is.na(myReprD$Reproduced))

# make Reproduced 1s and 0s
myReprD$Reproduced <- ifelse(myReprD$Reproduced=="N",0,1)

R_null <- glmer(Reproduced ~ 1 + (1|site/year),data=myReprD,family="binomial")

#R_null <-  gam(Reproduced ~ s(SiteCode, bs="re") + s(SiteCode,Year, bs="re"),data=myReprD,family="binomial",method="REML")

# does Treatment matter?
R_compTrt <- update(R_null, ~ . + Treatment)
# yes, higher prob reproducing in removals
rm(R_compTrt)

# so let's fit functional group effects
R_comp  <- glmer(Reproduced ~ annual + perennial + shrub + 
                   + (1|site/year),data=myReprD,family="binomial")

rm(R_null,R_comp)

# add climate variables

R_clim <- glmer(Reproduced ~ annual + perennial + shrub + 
                        tmean.Win + prcp.Win +
                       #   swe_mean.Win + prcp.Spr + tmean.Spr +
                          (1|site/year),data=myReprD,family="binomial")



# R_comp  <- gam(Reproduced ~ annual + perennial + shrub+ 
#                           s(SiteCode, bs="re") +  s(SiteCode,Year, bs="re")+
#                            s(SiteCode,Year,annual,bs="re") + 
#                            s(SiteCode,Year,perennial,bs="re") +
#                            s(SiteCode,Year,shrub,bs="re"),
#                         data=myReprD,family="binomial",method="REML")


### analyze fecundity conditional on reproduction

tmp <- which(myReprD$Reproduced==1 & myReprD$Fecundity > 0)
myFecD <- myReprD[tmp,]

# check data
table(myFecD$Fecundity)
sum(is.na(myFecD$Fecundity))
# myFecD[myFecD$fecundityflag==1,]  # might try models with and without flagged records

# fit

#F_null <-  gam(Fecundity ~ s(SiteCode, bs="re") + s(SiteCode,Year, bs="re"),data=myFecD,family="poisson",method="REML")

 F_trt <- glmer(Fecundity ~ Treatment + 
                  + (1+Treatment|site/year),data=myFecD,family="poisson")
# pairs(ranef(F_trt)[[2]]) # bigger removal effects at sites with higher intercepts
rm(F_trt)

# with flagged data
F_comp <- glmer(Fecundity ~ annual + perennial + shrub + 
                + (1|site/year),data=myFecD,family="poisson")

F_comp2 <- glmer(Fecundity ~ annual + perennial + shrub+ 
                  + (1|site/year),
                 data=subset(myFecD, fecundityflag!=1),
                 family="poisson")
#virtually identical coefficients
rm(F_comp, F_comp2)


F_clim  <- glmer(Fecundity ~ annual + perennial + shrub + prcp.Win + tmean.Win +
                                #swe_mean.Win + prcp.Spr  + tmean.Spr +
                                 #annual:prcp.Spr + 
                                #perennial:prcp.Spr + perennial:swe_mean.Win +
                                 #shrub:prcp.Spr + 
                                 (1|site/year),data=myFecD,family="poisson")

# plot obs vs predicted (including random effects)
plot(log(F_clim@resp$y),log(fitted(F_clim)))
abline(0,1)
# plot obs vs predicted (fixed effects only)
plot(log(F_clim@resp$y),log(predict(F_clim,re.form=NA)))
abline(0,1)

# F_comp <- gam(Fecundity ~ annual + perennial + shrub+ 
#                      s(SiteCode, bs="re") +  s(SiteCode,Year, bs="re") + 
#                      s(SiteCode,Year,annual,bs="re") + 
#                     s(SiteCode,Year,perennial,bs="re") +
#                      s(SiteCode,Year,shrub,bs="re"),
#                    data=myFecD,family="poisson",method="REML")


# ### analyze biomass conditional on reproduction 
# # results end up looking identical to fecundity analysis
# 
# # first, characterize biomass to fecundity relationship
# tmp <- which(myReprD$Reproduced==1 & myReprD$Fecundity > 0 & myReprD$Biomass > 0)
# myFecD <- myReprD[tmp,]
# 
# # check data
# table(myFecD$Fecundity)
# sum(is.na(myFecD$Fecundity))
# sum(is.na(myFecD$Biomass))
# hist(log(myFecD$Biomass))
# 
# # log transforms
# myFecD$logFec <- log(myFecD$Fecundity)
# myFecD$logB <- log(myFecD$Biomass)
# 
# plot(myFecD$logB,myFecD$logFec)
# summary(lm(logFec~logB, data=myFecD))
# summary(lmer(logFec~logB +(1|SiteCode), data=myFecD))
# # summary(lmer(logFec~logB +(logB|SiteCode), data=myFecD)) # random slope doesn't help
# 
# 
# # redo the data subset, keeping observations with bad measures of fecundity but good for biomass
# tmp <- which(myReprD$Reproduced==1 & !is.na(myReprD$Biomass) & myReprD$Biomass > 0)
# myMassD <- myReprD[tmp,]
# 
# # check data
# sum(is.na(myMassD$Biomass))
# hist(log(myMassD$Biomass))
# min(myMassD$Biomass)
# 
# myMassD$logB <- log(myMassD$Biomass)
# 
# # with flagged data
# B_comp <- lmer(logB ~ perennial + 
#                    (1|SiteCode/Year),data=myMassD)
# 
# B_clim <- lmer(logB ~ perennial + prcp*swe_mean +
#                  (1|SiteCode/Year),data=myMassD)

# 
# # ### visualize relationships
# library(visreg)
# visreg(F_clim, "perennial", by="swe_mean.Win",ylab="Fecundity")
# visreg2d(F_clim, "perennial", "prcp.Spr", plot.type="rgl")
# 

# save models
save(allDcomp,E_null,R_clim,F_clim,file="./../output/fitted.RDATA")

rm(myFecD,myReprD)

