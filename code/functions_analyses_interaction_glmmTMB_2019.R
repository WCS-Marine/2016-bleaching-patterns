##McClanahan et al. “Novel temperature patterns and geographic context critical to coral bleaching during the 2016 El Niño” 
##JosephMaina_2018_19
#this function evaluates the models and returns a model selection object
#for poisson models 
evalTextModel  <-  function(modelText,...) {
  #theMod <- model.sel(try(eval(parse(text = modelText))), extra =c(AIC, BIC, Cp,Nc = function(x) mean(ID)))
  #theMod <- model.sel(try(eval(parse(text = modelText))), extra =c(AIC, BIC, Cp,Nc = function(x) ModelRef(modelTextList, modelText)[[1]]))
  theMod <- try(eval(parse(text = modelText))) 
  tryErrorCheck  <-  all(class(theMod) %in% 'try-error')
  if(!tryErrorCheck)
  theMod
  #model.sel(theMod, rank.args = list(REML = FALSE), extra =c(AIC, BIC, Cp))
  #model.sel(theMod, rank=QAIC, rank.args=list(chat = deviance(theMod$mer) / df.residual(theMod$mer), REML = NULL), extra =c(AIC, BIC, Cp,OvDispPval = function(x) overdisp_fun(x$mer)[[4]]))
}

#arcsin squareroot transformation formula for percentages
trans.arcsine <- function(x){
  asin(sign(x) * sqrt(abs(x)))
}



checkNewVarAndAdd <-  function(originalData, targetData, newVar) {
  responseAndPredictorCheck  <-  sizesMatch(originalData, targetData)
  newVarNameDoesNotExist     <-  !(newVar %in% names(targetData))
  
  if(responseAndPredictorCheck & newVarNameDoesNotExist)
    originalData[[newVar]]
}

sizesMatch  <-  function(originalData, targetData) {
  nrow(originalData) == nrow(targetData)
}

runPredCombinations  <-  function(x, vec) {
  lapply(data.frame(combn(vec, x), stringsAsFactors=FALSE), identity)
}

##prepare the model text



prepareModelText  <-  function(Predictors, data) {
  ##dat           <-  data[, Predictors]
  dat <- subset(data, select = Predictors)
  dataClasses   <-  unique(sapply(dat, class))
  numOfClasses  <-  length(dataClasses)
  mixedClasses  <-  numOfClasses > 1
  if(mixedClasses) {
    fixStepNum   <-  paste0(Predictors[sapply(dat, class) == 'numeric'], collapse='*')
    fixStepCha   <-  paste0(Predictors[sapply(dat, class) == 'character'], collapse='*')
    fixStep      <-  paste0(fixStepNum, '*', fixStepCha)
  } else {
    if(dataClasses == 'numeric') {
      fixStep   <-  paste0(Predictors[sapply(dat, class) == 'numeric'], collapse='*')
      ranStep1  <-  paste0('(0+', Predictors, '|location)' ,  collapse='+')
    }
    if(dataClasses == 'character') {
      fixStep   <-  paste0(Predictors[sapply(dat, class) == 'character'], collapse='*')
    }
  }
  
  #paste0('glmmadmb(bleach_intensity ~', fixStep,'+ (1|location)',', family="beta",link="logit"',', data=data.stds,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))')
  lapply(seq_along(respVar), function(i) paste0('glmmadmb(',colnames(respVar)[i], '~', fixStep,'+ (1|location)',', family="beta",link="logit"',', data=data.stds,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))'))
  #paste0('glmmadmb(bleach_intensity ~', fixStep,'+ (1|location) + ',ranStep1,', family="beta",link="logit"',', data=data.stds,admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))') 
  #lapply(seq_along(respVar), function(i) paste0('glmmadmb(',colnames(respVar[i]), '~', fixStep,'+ (1|region) + ',ranStep1,', family="beta",link="logit"',', data=data.stds, admb.opts=admbControl(shess=FALSE,noinit=FALSE, impSamp=200,maxfn=1000,imaxfn=500,maxph=5))'))
  #paste0('glmmadmb(avg.bleach.intensityScaled ~', fixStep,'+ (depth|location)',', family="beta",link="logit"',', data=data)') 
  #paste0('glmmadmb(avg.bleach.intensity ~', fixStep,'+ (depth|location)',', family="gamma",link="log"',', data=data)') 
  #paste0('glmmadmb(site.susceptibility ~', fixStep,'+ (depth|location)',', family="gaussian",link="log"',', data=data)') 
  #paste0('uGamm(no_genera ~ 1 + s(lnReefLength, k = 5, bs = "cr") +', fixStep, ',family=poisson(link=log)',', random=~(1|Province)','+ (1|Year)','+ (1|Method)','+ (1|Habitat)',',lme4=TRUE',', data=data)') 
  #paste0('uGamm(LogitFDRao ~ 1 + s(lnReefLength, k = 5, bs = "cr") +', fixStep, ',family=gaussian(link=identity)',', random=~(1|Province)','+ (1|Year)','+ (1|Method)','+ (1|Habitat)',',lme4=TRUE',', data=data)')
  #paste0('uGamm(PD_Rao ~ 1 + s(lnReefLength, k = 5, bs = "cr") +', fixStep, ',family=Gamma(link="log")',', random=~(1|Province)','+ (1|Year)','+ (1|Method)','+ (1|Habitat)',',lme4=TRUE',', data=data)') 
  #paste0('try(uGamm(no_genera ~ 1 +', fixStep, ',family=poisson(link=log)',', random=~(1|Province)','+ (1|Year)','+ (1|Method)','+ (1|Habitat)',',lme4=TRUE',', data=data), silent=T)') 
  #paste0('uGamm(no_genera ~', fixStep, ',family=poisson(link=log)',',random=list(Province=~1',', Year=~1',', Habitat=~1',',Method=~1)',',methods="REML", data=data)')
  #paste0('uGamm(FDRaoRescaled ~', fixStep, ',family=betar(link="logit")',',random=list(Province=~1',', Year=~1',', Habitat=~1',',Method=~1)',',methods="REML", data=data)')
  #paste0('uGamm(lnPDRao ~', fixStep, ',family=gaussian(link = "identity")',',ran
  #paste0('uGamm(SumCover ~', fixStep, ',family=gaussian(link = "identity")',',random=list(Province=~1',', Year=~1',', Habitat=~1',',Method=~1)',',methods="REML", data=data)') 
}

runModelText  <-  function(...) {
  modelText  <-  prepareModelText(...)
  evalTextModel(modelText)
}

###to test the list of predictors
#lapply(yourListOfPredictors, prepareModelText, data=yourDataset)to call this to the dataset

#getMaximumNOfCombs  <-  function(vecOfPredictors, combCeiling=3)##here adjust the number of variable combinations for fixed effects
getMaximumNOfCombs  <-  function(vecOfPredictors, combCeiling=3) {
  if(length(vecOfPredictors) >= combCeiling) {
    combCeiling
  } else {
    length(vecOfPredictors)
  }
}

##correlation matrix and table
corstarsl <- function(x){ 
  require(Hmisc) 
  require(xtable)
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

##compute an overdispersion factor, straight from http://glmm.wikidot.com/faq
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

