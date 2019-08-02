#### Dynamic Time Warping Script ####
rm(list = ls())
library(dtw)
library(zoo)
library(gam)
library(readr)
#/Users/Azali/Dropbox/Global Bleaching/Paper/BleachingResponseModels/analysis/Env2017/Satellite V Insitu/90DaySat_Insitu.csv
dat <- read_delim("~/Dropbox/Global Bleaching/Paper/BleachingResponseModels/analysis/Env2017/Satellite V Insitu/90DaySat_Insitu.csv", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)
colnames(dat) <- make.names(colnames(dat))
Kisite <- dat[1:91,c(1:3)]

Kisite$X28.Kisite.date<- as.Date(Kisite$X28.Kisite.date, format="%d/%m/%Y")
reference<-Kisite$X28.IN_Kisite
query<-Kisite$X28.Kisite.Kisite.corner
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Kisite Warm Season, d= 12.58, normd= 0.07")

##### DTW Mombasa Coral Gardens 1
rm(list=c("reference","query"))

#dat <- read.csv("90DaySat_Insitu.csv", as.is = TRUE)
MsaCG1 <- dat[1:54,c(4:6)]
MsaCG1$X105.Mombasa.date<- as.Date(MsaCG1$X105.Mombasa.date, format="%d/%m/%Y")
reference<-MsaCG1$X105.IN_Mombasa.lagoon
query<-MsaCG1$X105.Kenya.fringing.Coral.Gardens.1
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Mombasa CG Warm Season, d= 21.43, normd= 0.19")

##### DTW Mombasa Lagoon
rm(list=c("reference","query"))

#dat <- read.csv("90DaySat_Insitu.csv", as.is = TRUE)
MsaLG <- dat[1:86,c(7:9)]
MsaLG$X35.Mombasa.date<- as.Date(MsaLG$X35.Mombasa.date, format="%d/%m/%Y")
reference<-MsaLG$X35.IN_Mombasa.lagoon
query<-MsaLG$X35.Kenya.fringing.Mombasa.2
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Mombasa LG Warm Season, d= 28.38, normd= 0.16")

##### DTW Mombasa Coral Gardens 2
rm(list=c("reference","query"))

MsaCG2 <- dat[1:91,c(10:12)]
MsaCG2$X33..Mombasa.date<- as.Date(MsaCG2$X33..Mombasa.date, format="%d/%m/%Y")
reference<-MsaCG2$X33.IN_Mombasa.lagoon
query<-MsaCG2$X33.Kenya.fringing.Coral.gardens.monitoring
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Mombasa CG2 Warm Season, d= 30.07, normd= 0.17")

##### DTW Mombasa Coral Gardens 2
rm(list=c("reference","query"))

MsaMushroom <- dat[1:91,c(13:15)]
MsaMushroom$X34.Mombasa.Mushroom.date<- as.Date(MsaMushroom$X34.Mombasa.Mushroom.date, format="%d/%m/%Y")
reference<-MsaMushroom$X34.IN_Mombasa.Mushroom
query<-MsaMushroom$X34.Kenya.fringing.Mushroom
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Mombasa Mushroom Warm Season, d= 29.73, normd= 0.16")

##### DTW Mradi
rm(list=c("reference","query"))

Mradi <- dat[1:91,c(16:18)]
Mradi$X39..Mradi.date<- as.Date(Mradi$X39..Mradi.date, format="%d/%m/%Y")
reference<-Mradi$X39.IN_Mradi
query<-Mradi$X39.Kenya.fringing.Mradi.tengefu
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Mradi Warm Season, d= 29.84, normd= 0.16")

##### DTW Kuruwitu
rm(list=c("reference","query"))

Kuruwitu <- dat[1:91,c(19:21)]
Kuruwitu$X41.Kuruwitu.date<- as.Date(Kuruwitu$X41.Kuruwitu.date, format="%d/%m/%Y")
reference<-Kuruwitu$X41.IN_Vipingo
query<-Kuruwitu$X41.Kenya.fringing.Kiruwitu.Community.Closure
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Kuruwitu Warm Season, d= 44.37, normd= 0.24")

#### DTW Watamu
rm(list=c("reference","query"))

Watamu <- dat[1:91,c(22:24)]
Watamu$X42.Watamu.date<- as.Date(Watamu$X42.Watamu.date, format="%d/%m/%Y")
reference<-Watamu$X42.IN_Watamu
query<-Watamu$X42.Kenya.fringing.Watamu.South
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Kuruwitu Warm Season, d= 38.07, normd= 0.21")

##### DTW Bongoyo
rm(list=c("reference","query"))

Bongoyo <- na.omit(dat[1:91,c(25:27)])
Bongoyo$X86.Bongoyo.date<- as.Date(Bongoyo$X86.Bongoyo.date, format="%d/%m/%Y")
reference<-Bongoyo$X86.IN_Bongoyo
query<-Bongoyo$X86.Tanzania.islands.Bongoyo.Site.1
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Bongoyo Warm Season, d= 16.75, normd= 0.09")

##### DTW Changuu
rm(list=c("reference","query"))

Changuu <- na.omit(dat[1:91,c(28:30)])
Changuu$X87.Changuu<- as.Date(Changuu$X87.Changuu, format="%d/%m/%Y")
reference<-Changuu$X87.IN_Changuu
query<-Changuu$X87.Tanzania.islands.Changuu.1
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Changuu Warm Season, d= 23.13, normd= 0.12")

##### DTW Chumbe
rm(list=c("reference","query"))

Chumbe <- na.omit(dat[1:91,c(31:33)])
Chumbe$X90.Chumbe.date<- as.Date(Chumbe$X90.Chumbe.date, format="%d/%m/%Y")
reference<-Chumbe$X90.IN_Chumbe
query<-Chumbe$X90.Tanzania.islands.Chumbe.South
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Chumbe Warm Season, d= 12.26, normd= 0.09")

##### DTW Mbudya
rm(list=c("reference","query"))

Mbudya <- na.omit(dat[1:91,c(34:37)])
Mbudya$X100.99..Mbudya.date<- as.Date(Mbudya$X100.99..Mbudya.date, format="%d/%m/%Y")
reference<-Mbudya$X100.99.IN_Mbudya
query<-Mbudya$X99.Tanzania.islands.Mbudya.Edge.2
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Chumbe Warm Season, d= 15.38, normd= 0.09")

##### DTW PLACH ALINZ
rm(list=c("reference","query"))

PlanchAlinz <- dat[1:91,c(38:40)]
PlanchAlinz$X144.Plach.Alinz<- as.Date(PlanchAlinz$X144.Plach.Alinz, format="%d/%m/%Y")
reference<-PlanchAlinz$IN_Planch.Alinz
query<-PlanchAlinz$X144.Reunion.Planch.Aliz
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Plach Alinz Warm Season, d= 49.76, normd= 0.27")

##### DTW Potato patch
rm(list=c("reference","query"))

reference<-na.omit(dat$IN_Potato_Patch)
query<-na.omit(dat$X221.Lakshadweeps.Potato_Patch_shallow)
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Plach Alinz Warm Season, d= 25.49, normd= 0.14")

##### DTW Japanese Garden
rm(list=c("reference","query"))

Japanese.garden <- na.omit(dat[,44:46])
reference<-na.omit(Japanese.garden$IN_Japanese_Garden)
query<-na.omit(Japanese.garden$X208.Lakshadweeps.Japanese_Garden_shallow)
align<-dtw(query, reference, keep=TRUE, step.pattern = symmetric2, dist.method = "Euclidean")
align$distance
align$normalizedDistance
plot(align, type = "two", main="Plach Alinz Warm Season, d= 19.03, normd= 0.14")


### Error statistics y= satellite, x= In situ ####

rm(list = ls())
dat <- read.csv("Sat_Insitu_stack.csv")

library(reshape2)

library(Metrics)

dat <- dcast(dat, formula = Days_Index ~ ind, value.var = c("values"))

#Kisite
rm(list=c("actual","predicted"))

Kisite.lm <- lm(Sat_Kisite~IN_Kisite, data= dat)
predicted <- Kisite.lm$fitted.values
actual <- dat$Sat_Kisite
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Mombasa lagoon
rm(list=c("actual","predicted"))

Mombasa.lm <- lm(Sat_Mombasa_lagoon~IN_Mombasa_lagoon, data= dat)
predicted <- Mombasa.lm$fitted.values
actual <- dat$Sat_Mombasa_lagoon
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Mombasa offshore lagoon
rm(list=c("actual","predicted"))

MombasaO.lm <- lm(Sat_Mombasa_Offshore~IN_Mombasa_Offshore, data= dat)
summary(MombasaO.lm)
predicted <- MombasaO.lm$fitted.values
actual <- dat$Sat_Mombasa_Offshore
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Mradi
rm(list=c("actual","predicted"))

Mradi.lm <- lm(Sat_Mradi~IN_Mradi, data= dat)
summary(Mradi.lm)
predicted <- Mradi.lm$fitted.values
actual <- dat$Sat_Mradi
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Kuruwitu
rm(list=c("actual","predicted"))

Kuruwitu.lm <- lm(Sat_Kuruwitu~IN_Kuruwitu, data= dat)
summary(Kuruwitu.lm)
predicted <- Kuruwitu.lm$fitted.values
actual <- dat$Sat_Kuruwitu
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Watamu
rm(list=c("actual","predicted"))

Watamu.lm <- lm(Sat_Watamu~IN_Watamu, data= dat)
summary(Watamu.lm)
predicted <- Watamu.lm$fitted.values
actual <- dat$Sat_Watamu
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Bongoyo
rm(list=c("actual","predicted"))

Bongoyo.lm <- lm(Sat_Bongoyo~IN_Bongoyo, data= dat)
summary(Bongoyo.lm)
predicted <- Bongoyo.lm$fitted.values
actual <- na.omit(dat$Sat_Bongoyo)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Bongoyo
rm(list=c("actual","predicted"))

Bongoyo.lm <- lm(Sat_Bongoyo~IN_Bongoyo, data= dat)
summary(Bongoyo.lm)
predicted <- Bongoyo.lm$fitted.values
actual <- na.omit(dat$Sat_Bongoyo)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Changuu
rm(list=c("actual","predicted"))

Changuu.lm <- lm(Sat_Changuu~IN_Changuu, data= dat)
summary(Changuu.lm)
predicted <- Changuu.lm$fitted.values
actual <- na.omit(dat$Sat_Changuu)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Chumbe
rm(list=c("actual","predicted"))

Chumbe.lm <- lm(Sat_Chumbe~IN_Chumbe, data= dat)
summary(Chumbe.lm)
predicted <- Chumbe.lm$fitted.values
actual <- na.omit(dat$Sat_Chumbe)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Mbudya
rm(list=c("actual","predicted"))

Mbudya.lm <- lm(Sat_Mbudya~IN_Mbudya, data= dat)
summary(Mbudya.lm)
predicted <- Mbudya.lm$fitted.values
actual <- na.omit(dat$Sat_Mbudya)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Planch_Alinz
rm(list=c("actual","predicted"))

Planch_Alinz.lm <- lm(Sat_Planch_Alinz~IN_Planch_Alinz, data= dat)
summary(Planch_Alinz.lm)
predicted <- Planch_Alinz.lm$fitted.values
actual <- na.omit(dat$Sat_Planch_Alinz)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Potato_Patch
rm(list=c("actual","predicted"))

Potato_Patch.lm <- lm(Sat_Potato_Patch~IN_Potato_Patch, data= dat)
summary(Potato_Patch.lm)
predicted <- Potato_Patch.lm$fitted.values
actual <- na.omit(dat$Sat_Potato_Patch)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

#Potato_Patch
rm(list=c("actual","predicted"))

Japanese.garden <- na.omit(dat[,c("Sat_Japanese_Garden","IN_Japanese_Garden")])
Japanese_garden.lm <- lm(Sat_Japanese_Garden~IN_Japanese_Garden, data= Japanese.garden)
summary(Japanese_garden.lm)
predicted <- Japanese_garden.lm$fitted.values
actual <- na.omit(Japanese.garden$Sat_Japanese_Garden)
round(rmse(actual,predicted),2)
round(mae(actual,predicted),2)
bias(actual,predicted)
percent_bias(actual,predicted)
mape(actual,predicted)

