##McClanahan et al. “Novel temperature patterns and geographic context critical to coral bleaching during the 2016 El Niño” 
##josephMaina_2018_19
rm(list=ls())
library(usdm)
library(MuMIn)
library(parallel)
library(ttutils)
library(pROC)
library(glmmTMB)
library(glmmADMB)
library(splines)
require(Hmisc)
library(coefplot2)
library(piecewiseSEM)

##bootstrapping
library(ggplot2)
library(scales)
library(MASS)
library(boot)
library(caret)
library(plyr)
library(dplyr)
library(tidyr)
library(directlabels)

source('~/Documents/R_projects/CoralBleaching/R/functions_analyses_interaction_glmmADMB.R')
source('~/Documents/R_projects/CoralBleaching/R/database.R')

###PRdictor correlations as a table
corstarsl <- function(x){  
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
} 

cor.all<-corstarsl(prVar)
write.csv(cor.all,"/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/cor_all.csv")
##employs the xtable package that produces La
##combine variables for each predictor
#1. Create list with all possible combinations between predictors 
vifPredCombinations  <-  list()
varnames<-colnames(PredictVar)#

maxCombs  <-  getMaximumNOfCombs(varnames)
for(j in 1:maxCombs) {
  vifPredCombinations  <-  append(runPredCombinations(j, varnames), vifPredCombinations)
}

##2. filter the combinations above with VIF<1.5
vifPredCombinations_new<- c()
for(con in vifPredCombinations){
  r <- subset(PredictVar, select = con)
  conClasses   <-  unique(sapply(r, class))
  numOfClasses  <-  length(conClasses)
  twoNCols <- ncol(r)==2
  numDf <- r[sapply(r,is.numeric)]
  zeroNumDf<-ncol(numDf)==0
  numeriCols<- ncol(numDf)>1
  
  if(length(con)<2){
    vifPredCombinations_new <- c(vifPredCombinations_new, list(con))
  }else{
    if (zeroNumDf) { 
      vifPredCombinations_new <- c(vifPredCombinations_new, list(con))
    }
    if (numOfClasses==2 && twoNCols) { 
      vifPredCombinations_new <- c(vifPredCombinations_new, list(con))
    }
    if(numeriCols && max(vif(numDf)["VIF"])<1.5){##vif cutoff
      vifPredCombinations_new <- c(vifPredCombinations_new, list(con))
    }
    next
  }
}

##3. Run all models following this combination
#modelList  <-  lapply(vifPredCombinations_new, runModelText, data.stds)
#system.time(modelListpercBl  <-  mclapply(vifPredCombinations_new[1:5], runModelText, data, #mc.preschedule=FALSE, mc.cores=2))
#trial  <-  lapply(vifPredCombinations_new, runModelText, data)
#modelText<-unlist(modelText, recursive=TRUE)
#a<-lapply(modelText[1:2],evalTextModel)
#require(parallel)
#UnstTwoClassModels.tr1<-mclapply(varListFin, runModelText)# for unstandardized


##ns refers to models wirh no random slopes
modelText<-lapply(vifPredCombinations_new, prepareModelText, data.stds)

modelText.a<-unlist(modelText)
#modelText.b<-as.list(modelText.a)
##
bl.int.interactions<-mclapply(modelText.a[1:10000], evalTextModel)
save.image('/Users/maina/Documents/R_projects/CoralBleaching/R/Output/bl.int.interactions.10k')

##merge lists
AllTaxaTMBModels<-c(bl.int.interactions,bl.int.interactions.20k,bl.int.interactions.30k,bl.int.interactions.40k,bl.int.interactions.50k )
#bl.int.interactions<-bl.int.interactions[!sapply(bl.int.interactions, is.null)] 
#model.sel.bl.int<-model.sel(all.taxa.modList, rank.args = list(REML = FALSE), extra =c(AIC, BIC, Ovd = function(x) overdisp_fun(x)[[4]]))


##run models#35991
all.taxa.modList<-mclapply(modelText.b, evalTextModel)

all.taxa.modList.1<-all.taxa.modList[!sapply(all.taxa.modList, is.null)] 

#save.image("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/bl.int.modList.1.RData")
##write the modelLists to file
save.image("~/Documents/R_projects/CoralBleaching/Output/allResponseModList_10k.RData", compress="xz")
save.image("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/BlIntWithIntrction.RData", compress="xz")
write.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/allResponseModList.RData", compress="xz")
write.csv(model.sel.bl.int.interactions,"/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/ModelSel_BlIntWithInteraction.csv")

#colnames(respVar)
#[1] "bleach_intensity"     "Acropora.bi"          "Porites.massive.bi"  
# [4] "Montipora.bi"         "Porites.branching.bi" "Pocillopora.bi"      
#[7] "Favites.bi"           "Galaxea.bi"           "Fungia.bi"           
#[10] "Goniastrea.bi"        "Favia.bi"             "Pavona.bi"           
#[13] "Echinopora.bi"        "Synarea.bi"           "Platygyra.bi"        
#[16] "Seriatopora.bi"       "Diploastrea.bi"       "Millepora.bi"        
#[19] "Isopora.bi"           "Heliopora.bi"         "Cyphastrea.bi"       
#[22] "Hydnophora.bi"        "Montastrea.bi"        "Stylophora.bi"       
#[25] "Astreopora.bi"    "Lobophyllia.bi"       "Leptastrea.bi"       
#[28] "Coscinaraea.bi"       "Goniopora.bi"         "Leptoseris.bi"       
#[31] "Pachyseris.bi"  

load("~/Documents/R_projects/CoralBleaching/Output/allResponseModList.RData")
load("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/BlIntWithIntrction.RData")


spnames<-as.list(colnames(respVar))
mod.sub<-list()
mod.sub.ls<-list()
modelSel<-list()
top.models<-list()
avgmod<-list()
Coef.Conditional<-list()
coefTable<-list()
mod.sub.narm<-list()
for (i in seq_along(colnames(respVar[1:10]))){
  ## create a model list for each response/taxa from the grande model list
  mod.sub[[i]]<-all.taxa.modList[seq(i, length(all.taxa.modList), 31)]
  #remove 'bad' models
  mod.sub.ls[[i]]<-mod.sub[[i]][!sapply(mod.sub[[i]], is.null)]
  mod.sub.narm[[i]] <- Filter(function(x) {!is.na(AIC(x))}, mod.sub.ls[[i]])
  #perform model selection
  modelSel[[i]]<-model.sel(mod.sub.narm[[i]],rank.args = list(REML = FALSE), extra =c(AIC, BIC))
  top.models[[i]]<-get.models(modelSel[[i]], subset=delta<2)
  #model average top 2aic models
  if(length(top.models[[i]]) ==1) next
  avgmod[[i]]<- model.avg(top.models[[i]])
  #extract averaged coefficients
  Coef.Conditional[[i]]<-data.frame(avgmod[[i]]$coefficients[2,])
  coefTable[[i]]<-cbind(data.frame(confint(avgmod[[i]], level = 0.975),confint(avgmod[[i]],level = 0.95), Coef.Conditional[[i]]))
  colnames(coefTable[[i]])<-c("conf2.5","conf97.5","conf5","conf95","StdCoef")
  coefTable[[i]]$var<-rownames(coefTable[[i]])
  write.csv(modelSel[[i]], paste("/Users/maina/Documents/bleachpaper/", "ModelSel_",spnames[[i]], ".csv", sep=""))
  write.csv(coefTable[[i]], paste("/Users/maina/Documents/bleachpaper/", "coefTable_",spnames[[i]], ".csv", sep=""))
}

######################
devtools::install_github("bbolker/broom.mixed")
library(broom.mixed)
tidy(mod.sub[[i]][[10]], effects = "fixed", conf.int=TRUE)
summ.table<-do.call(rbind, lapply(d, broom::glance))
####################


####if not in a loop
top.models<-get.models(model.sel.bl.int.interactions, subset=delta<2)
#model average top 2aic models
avgmod<- model.avg(top.models)
#extract averaged coefficients
Coef.Conditional<-data.frame(avgmod$coefficients[2,])
coefTable<-cbind(data.frame(confint(avgmod, level = 0.975),confint(avgmod,level = 0.95), Coef.Conditional))
colnames(coefTable)<-c("conf2.5","conf97.5","conf5","conf95","StdCoef")
coefTable$var<-rownames(coefTable)

write.csv(coefTable, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/BlIntWithInteractions.csv")


##bleaching intensity model
m1<-get.models( model.sel.bl.int.interactions, subset=1) [[1]]
m2<-get.models( model.sel.bl.int.interactions, subset=2) [[1]]
m8<-get.models( model.sel.bl.int.interactions, subset=8) [[1]]
mdhw.ave<-get.models( model.sel.bl.int.interactions, subset=589) [[1]]
mdhw.max<-get.models( model.sel.bl.int.interactions, subset=445) [[1]]


##plot residuals
plot.glmmadmb <- function(x, ...)
{
  plot(x$fitted, x$residuals, xlab="Fitted values", ylab="Residuals", ...)
}


x=m1$fitted
y=m1$residuals 

datE<-cbind(x,y) 
colnames(datE)<-c("x","y")

plot(x, y, abline(lm(y~x),col="red",lwd=1.5))


##plotTopModel
ggplot(coefTable, aes(var,StdCoef)) + geom_point(stat="identity") +  ggtitle("Model Averaged coefficients")+ coord_flip() + geom_hline(yintercept=0)+geom_errorbar(data = coefTable, aes(ymin = conf5, ymax = conf95),colour = 'black', width = 0.4)


##########
dat<-data.frame(cbind(bl.env[['bleach_intensity']],bl.env[['location']], data.std))
colnames(dat)[1]<-'bl.intens'
colnames(dat)[2]<-'location'
library(reshape2)
mtmelt <- melt(dat, id = c("bl.intens", "location"))
mtmelt$location<-bl.env$location

ggplot(mtmelt, aes(x = value, y = bl.intens)) + geom_smooth(method="lm")+
  facet_wrap(~variable, scales = "free") +
  geom_point(aes(colour=location) )



#------------------simple bootstrap comparison-------------------------
data<-data.stds

data <- data[complete.cases(data), ]
# create a function suitable for boot that will return the goodness of fit
# statistics testing models against the full original sample.
compare <- function(orig_data, i){
  # create the resampled data
  train_data <- data[i, ]
  test_data <- data # ie the full original sample
  # fit the three modelling processes
  #model_full <- glmmadmb(respVar.perc_bleachedScaled ~bimodality.coefficient + avg.low.spell.duration #+ low.spell.freq + (1|country), family="beta",link="logit", data=train_data)
  m1<-glmmadmb(bleach_intensity ~ X * bimodality.coefficient * avg.spell.peak + (1|location), family="beta",link="logit", data=train_data,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
  m2<-glmmadmb(bleach_intensity ~X * avg.spell.peak * avg.low.spell.duration.90dys + (1|location), family="beta",link="logit", data=train_data,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
  avgmod<- model.avg(m1, m2)
  mdhw.ave<-glmmadmb(bleach_intensity ~average.dhw.90days+ (1|location), family="beta",link="logit", data=train_data,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
  mdhw.max<-glmmadmb(bleach_intensity ~max.dhw.90days+ (1|location), family="beta",link="logit", data=train_data,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))
  
  #avgmod.95p <- model.avg(modelListpercBl_modelSel, cumsum(weight) <= .95)
  avgmod<- model.avg(m1,m2)
  
  # predict the values on the original, unresampled data
  model.avg  <- predict(avgmod, newdata = test_data, re.form=NA)
  m1  <- predict(m1, newdata = test_data, re.form=NA)
  m2  <- predict(m2, newdata = test_data, re.form=NA)
  dhw_ave  <- predict(mdhw.ave, newdata = test_data, re.form=NA)
  dhw_max  <- predict(mdhw.max, newdata = test_data, re.form=NA)
  
  # return a vector of 6 summary results
  results <- c(modelAve_R2  = R2(inv.logit(model.avg), test_data$bleach_intensity),
               m1_R2  = R2(inv.logit(m1), test_data$bleach_intensity),
               m2_R2  = R2(inv.logit(m2), test_data$bleach_intensity),
               dhw_ave_R2  = R2(inv.logit(dhw_ave), test_data$bleach_intensity),
               dhw_max_R2  = R2( inv.logit(dhw_max), test_data$bleach_intensity),
               modelAve_rmse  = RMSE(inv.logit(model.avg), test_data$bleach_intensity),
               m1_rmse  = RMSE(inv.logit(m1), test_data$bleach_intensity),
               m2_rmse  = RMSE(inv.logit(m2), test_data$bleach_intensity),
               dhw_ave_rmse  = RMSE(inv.logit(dhw_ave) , test_data$bleach_intensity),
               dhw_max_rmse  = RMSE(inv.logit(dhw_max), test_data$bleach_intensity))
  return(results)
}

# perform bootstrap
Repeats <- 10
res <- boot(data, statistic = compare, R = Repeats)

tests <- as.data.frame(res)

names(tests) <- c("modelAve_R2","m1_R2 ","m2_R2 ","dhw_ave_R2","dhw_max_R2","modelAve_rmse","m1_rmse","m2_rmse","dhw_ave_rmse","dhw_max_rmse")


simple <- apply(tests, 2, mean)


simple





##Check Autocorrelation for the three response variables
##Use Moran's I simmilarity spline correlograms for the three response variables
##Check for autocorrelation on the model residuals

library(ncf)
library(ade4)
library(ecodist)

#avg.bleach.intensity
data=data.stds
CorRes.bInt<- spline.correlog(data$long, data$lat, data$bleach_intensity, xmax = FALSE,resamp=1000,latlon=TRUE, quiet=TRUE, filter=TRUE, na.rm=T)
CorRes.bInt.res <- spline.correlog(data$long, data$lat, as.vector(resid(m1)),xmax = FALSE, resamp=1000,latlon=TRUE, quiet=TRUE, filter=TRUE, na.rm=T)
CorRes.bInt.res.m2 <- spline.correlog(data$long, data$lat, as.vector(resid(m2)),xmax = FALSE, resamp=1000,latlon=TRUE, quiet=TRUE, filter=TRUE, na.rm=T)



a_x<-CorRes.bInt$real$predicted$x
a_y<-CorRes.bInt$real$predicted$y
a<-t(data.frame(CorRes.bInt$boot$boot.summary$predicted$y))
a_yconfint<-a[,'0.95']
dat_a<-data.frame(cbind(a_x,a_y,a_yconfint))
dat_a$u<-dat_a$a_y+dat_a$a_yconfint
dat_a$l<-dat_a$a_y-dat_a$a_yconfint
dat_a$u<-dat_a$a_y+dat_a$a_yconfint
dat_a$l<-dat_a$a_y-dat_a$a_yconfint
dat_a$u<-dat_a$a_y+dat_a$a_yconfint
dat_a$l<-dat_a$a_y-dat_a$a_yconfint
dat_a$Data<-"(A) Raw data"
dat_a$Variable<-"Average Bleaching Intensity"
dat_a$xInt<-CorRes.bInt$real$x.intercept
colnames(dat_a)<-c("x","y","confint","u","l","Data","Variable","xInt")


b_x<-CorRes.bInt.res$real$predicted$x
b_y<-CorRes.bInt.res$real$predicted$y
b<-t(data.frame(CorRes.bInt.res$boot$boot.summary$predicted$y))
b_yconfint<-b[,'0.95']
dat_b<-data.frame(cbind(b_x,b_y,b_yconfint))
dat_b$u<-dat_b$b_y+dat_b$b_yconfint
dat_b$l<-dat_b$b_y-dat_b$b_yconfint
dat_b$u<-dat_b$b_y+dat_b$b_yconfint
dat_b$l<-dat_b$b_y-dat_b$b_yconfint
dat_b$u<-dat_b$b_y+dat_b$b_yconfint
dat_b$l<-dat_b$b_y-dat_b$b_yconfint
dat_b$Data<-"(B) Residuals.model.1"
dat_b$Variable<-"Average Bleaching Intensity"
dat_b$xInt<-CorRes.bInt.res$real$x.intercept
colnames(dat_b)<-c("x","y","confint","u","l","Data","Variable","xInt")

c_x<-CorRes.bInt.res.m2$real$predicted$x
c_y<-CorRes.bInt.res.m2$real$predicted$y
c<-t(data.frame(CorRes.bInt.res.m2$boot$boot.summary$predicted$y))
c_yconfint<-c[,'0.95']
dat_c<-data.frame(cbind(c_x,c_y,c_yconfint))
dat_c$u<-dat_c$c_y+dat_c$c_yconfint
dat_c$l<-dat_c$c_y-dat_c$c_yconfint
dat_c$u<-dat_c$c_y+dat_c$c_yconfint
dat_c$l<-dat_c$c_y-dat_c$c_yconfint
dat_c$u<-dat_c$c_y+dat_c$c_yconfint
dat_c$l<-dat_c$c_y-dat_c$c_yconfint
dat_c$Data<-"(C) Residuals.model.2"
dat_c$Variable<-"Average Bleaching Intensity"
dat_c$xInt<-CorRes.bInt.res.m2$real$x.intercept
colnames(dat_c)<-c("x","y","confint","u","l","Data","Variable","xInt")


dat_all<-data.frame(rbind(dat_a,dat_b,dat_c))

hline.data <- data.frame(z = unique(dat_all$xInt)
                         ,  Variable= c("Average Bleaching Intensity","Average Bleaching Intensity","Average Bleaching Intensity") ,Data= c("(A) Raw data","(B) Residuals.model.1","(C) Residuals.model.2"))

pdf("SplineCorrelog.pdf")
ggplot(dat_all, aes(x, y)) + geom_line(col="blue") + geom_ribbon(data=dat_all,aes(ymin=l, ymax=u), alpha=0.2) + facet_grid(Variable~Data, scales="free") + geom_vline(aes(xintercept=z),data=hline.data, size=0.3,linetype = "longdash") + geom_hline(yintercept=0, size=0.3) + theme_bw() + lims(y = c(-0.5, 0.5)) + theme(panel.border = element_rect(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + xlab("Distance (km)") + ylab("Moran's I similarity")
dev.off()


##### Mantel's Test to test for overal autocorrelation (numerical representation)
library(ade4)
library(ecodist)

#Generate distance matrices
Site.dists <- dist(cbind(data$long, data$lat))
bi.dists <- dist(data$bleach_intensity) 
bi.distsresiduals <- dist(resid(m1)) 
bi.distsresiduals.m2 <- dist(resid(m2)) 
#Mantel Test
#mantel.results=mantel.rtest(BP.dists, Site.dists, #nrepet=999)  

#Plot Mantel Test
mantel1=mgram(bi.dists, Site.dists)
mantel2=mgram(bi.distsresiduals, Site.dists)
mantel3=mgram(bi.distsresiduals.m2, Site.dists)

pdf("MantelTest.pdf")
par(mfrow=c(2,2), mai=c(0.5,1,0.2,0.2), mgp=c(0.5,0.6,0), mar=c(1.5,2.5,1,0.2), oma=c(2.5,1.5,0,0.5), bg="white", fg="black", cex.axis=1.05)
plot(mantel1, ylim=c(-1,1), ann=F);abline(0,0, lty=2,col="black",lwd = 1.5)
legend("topleft", "(A) Raw data", bty="n", cex=1.1)
mtext("Mantel r", side=2, adj=0.5, line=2, cex=1.1) 
plot(mantel2, ylim=c(-1,1), ann=F);abline(0,0, lty=2,col="black",lwd = 1.5)
legend("topleft", "(B) Residuals.model.1", bty="n", cex=1.1)
mtext("Distance", side=1, adj=0.5, line=2, cex=1.1)  
plot(mantel3, ylim=c(-1,1), ann=F);abline(0,0, lty=2,col="black",lwd = 1.5)
legend("topleft", "(C) Residuals.model.2", bty="n", cex=1.1)
mtext("Distance", side=1, adj=0.5, line=2, cex=1.1) 
mtext("Mantel r", side=2, adj=0.5, line=2, cex=1.1) 
dev.off()



