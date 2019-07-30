# Script to check general trends in bleaching drivers
# Authors: Steph and Emily
# Date: December 2017

# Clean environment
rm(list=ls())

# Load packages
#source(file="R code/brt.functions.R")
library(here)
library(PerformanceAnalytics)
library(MASS)
library(tree)
library(gbm)
library(rpart)
library(dismo)

# Set directory
wd <- "/Users/stephdagata/Documents/GitHub/2016-Bleaching-Analysis/"
setwd(wd)
# Load data
rm(BVAR)
BVAR <- read.csv("Data/masterfile - analysis variables_20Dec2017.csv",header=T,sep=";",dec=".",check.names = F, stringsAsFactors = T)
head(BVAR); dim(BVAR)

# Summary bleaching data
      ### There are 226 sites (rows)
      ### Location is a random effect, as a grouping variables of sites. 
      ### The response variable is "bleach_intensity". 
      ### And then the predictor variables are all the columns from X (longitude) through CA1 (community axis). 
      ### They're mostly independent and not collinear. 

summary(BVAR)

  # Check for normality of bleaching intensity
  hist(BVAR$bleach_intensity)
  
  # quick correlogram
  BVAR.num <- BVAR[,sapply(BVAR,is.numeric) | sapply(BVAR,is.integer)] # Subset only numeric columns from BVAR
  chart.Correlation(BVAR.num, histogram=TRUE, pch=16) # numeric variables only
  
  # quick boxplot for qualitative/quantitative variables
  BVAR_Q <- BVAR[,sapply(BVAR,is.factor)]
  for i in seq_along(length(BVAR[,sapply(BVAR,is.factor)])){
    for (j in seqalong(length(BVAR.num))){
  boxplot(BVAR.num[,i] ~ BVAR_Q$habitat)[i]
    }
  }
  
  
### Simple BRT model
  # The response variable we are interested in 
  rm(Resp)
  Resp <- which(colnames(BVAR) == "bleach_intensity"); Resp
  
  # Predictors
  rm(Pred)
  Pred=c("management","habitat","X","Y","average.dhw.90days","dip.Statistic.sst",
         "bimodality.coefficient","bimodality.ratio","avg.high.spell.duration.90days",
         "avg.high.spell.rise.90days","avg.spell.peak","sd.spell.peak",
         "avg.low.spell.duration.90dys","dhd_mmmplus1","CA1")
  
  # removed due to correlation:
    ## "max.dhw.90days"
    ## "max.high.spell.duration.90days"
    ## "n.h.spell.events_90Days"
  
  # Ncol = the vector containing the column number of each variable use to predict S
  rm(Ncol); Ncol<- which(colnames(BVAR) %in% Pred); Ncol 
  # chck
  length(Pred) == length(Ncol)
  
  # 1st Step: Built the full brt model and check for goodness of fit
  rm(BLEACH_brt)
  BLEACH_brt <- gbm.step(gbm.y=Resp,gbm.x=Ncol,data=BVAR,tree.complexity=10,learning.rate=0.005,bag.fraction=0.7,n.trees=50,family="gaussian",n.folds=10,max.trees=10000)
  
  # output of the model:
  ls(BLEACH_brt)
  
  # Contributions of each variable
  BLEACH_brt$contributions
  
  # summary brt
  summary(BLEACH_brt)
  abline(v=5,col="red")
  
  # marginal distributions
  windows()
  plot.gbm(BLEACH_brt,5)
  
  # CV AUC
  BLEACH_brt$cv.statistics$discrimination.mean
  
  # plot observed vs residuals
  plot(BVAR$bleach_intensity,BLEACH_brt$fit,
       xlim=c(0,100),ylim=c(0,100),
       xlab="Observed Bleaching Int.",ylab="Predicted Bleaching Int.",pch=16)
  abline(a=0,b=1)
    # linear model between predicted vs fitted
  lm_BlEACH <- lm(BLEACH_brt$fit~BVAR$bleach_intensity)
  abline(lm_BlEACH$coefficients[1],lm_BlEACH$coefficients[2],lty=2,col="red")
  # plot residuals vs observed data
  plot(BVAR$bleach_intensity,BLEACH_brt$residuals,
       xlim=c(0,100),ylim=c(-10,10),
       xlab="Observed Bleaching Int",ylab="Residuals",pch=16)
  abline(h=0)
  
  hist(BLEACH_brt$residuals)
  # plot residuals vs fitted data
  plot(BLEACH_brt$fit,BLEACH_brt$residuals,
       xlim=c(0,100),
       xlab="Fitted Biomass",ylab="Residuals",pch=16)
  abline(h=0)
  
  # 2. Look for interactions
  rm(BLEACH_int)
  BLEACH_int <- gbm.interactions(BLEACH_brt)  
  BLEACH_int$rank.list
  # plot the 3D plot between the most interacting variables
  gbm.perspec(BLEACH_brt, 1, 11,z.range=c(0,60))
  gbm.perspec(BLEACH_brt, 9, 10,z.range=c(0,60)) # sd spell and avg spell
  gbm.perspec(BLEACH_brt, 9, 5,z.range=c(0,60)) # bimod. coef and avg spell
  gbm.perspec(BLEACH_brt, 9, 3,z.range=c(0,60)) # bimod. coef and avg spell
  gbm.perspec(BLEACH_brt, 8, 3,z.range=c(0,60)) # bimod. coef and avg spell
  #etc.
  
  # 3. Simplify the model
  BLEACH_brt_simpl <- gbm.simplify(BLEACH_brt,n.drops=10,n.folds=10) # 7 last variables to remove
  
    # New model with only significant explanatory variables
  rm(BLEACH_brt_simpl)
  BLEACH_brt_SIMPL <- gbm.step(gbm.y=Resp,gbm.x=BLEACH_brt_simpl$pred.list[[7]],data=BVAR,tree.complexity=10,learning.rate=0.005,bag.fraction=0.7,n.trees=50,family="gaussian",n.folds=10,max.trees=10000)
  
  # Contributions
  BLEACH_brt_SIMPL$contributions
  
  # Interactions
  rm(BLEACH_int_SIMPL)
  BLEACH_int_SIMPL <- gbm.interactions(BLEACH_brt_SIMPL)  
  BLEACH_int_SIMPL$rank.list
  gbm.perspec(BLEACH_brt_SIMPL, 5, 4,z.range=c(0,60)) # sd spell and avg spell
  gbm.perspec(BLEACH_brt_SIMPL, 6, 3,z.range=c(0,60)) # bimod. coef and avg spell

    
  
