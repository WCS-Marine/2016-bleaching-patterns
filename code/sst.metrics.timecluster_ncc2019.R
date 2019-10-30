##prepared by Maina. j Mar 2018
## takes a temporal subset (90 days) of time series SST to do temporal clusteriung and to calculate metrics

rm(list=ls())
library(hydrostats)
library(modes)
library(diptest)
library(grid)
library(gridExtra)
library(gtable)
library(plyr)
library(ggplot2)
library(pdc)
library(reshape)

##prepare temoerature data
#Observed bleaching data
#pts1<-read.csv("/Users/josephmaina/Dropbox/Global Bleaching/Paper/Paper 1/data/masterfile.for.analyses.withtaxa_16feb.csv")
pts<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/masterfile_29Oct2017.csv")

#x<-(pts1$unique.id)
#y<-(pts$unique.id)
#diff<-x[!(x %in% y)]
#diff<-x[is.na(match(x,y))]
#diff<-which(!x%in%y)

##load and prepare SST data
sst.ts<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/sst.extr.csv")

##remove rows diff
#sst.ts<-sst.ts[-diff,]
sst.ts.t<-t(sst.ts)
sstDat<-  data.frame(sst.ts.t[2:1392,])
dim(sstDat)
rownames(sstDat)<-seq(1:1391)

date<-seq(as.Date("2013/03/12"), by = "day", length.out = 1391)
year<-as.numeric(format(date,"%Y"))
month<-as.numeric(format(date,"%m"))
sstData<-cbind(data.frame(date),month,year,sstDat)
pts_sub<-pts[,1:29]
labels<-as.list(paste(pts_sub$unique.id, pts_sub$location,pts_sub$site, sep="."))
colnames(sstData)[4:229]<-labels
sst_a<-sstData[,4:229]

##get threshold
#mmm.dhw<-read.csv("~/Dropbox/Global Bleaching/PaperVersions2&3/data/Env2017/dhw/dhw.extr.csv")
mmm.dhw<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/dhw.extr.092017.csv")
#mmm.dhw<-mmm.dhw[-diff,]

thresso<-as.list(mmm.dhw[,2])
thres<-lapply(thresso, function(x) x+1)

#create sapmling dates
bl.dates<-pts[,c(7,8,9)]
bl.dates$Obsdate<-paste(bl.dates[,1],bl.dates[,2],bl.dates[,3], sep="-")
bl.dates$onset<- as.Date(bl.dates$Obsdate)-90
onset<-as.list(bl.dates$onset)
obsdate<-as.list(bl.dates$Obsdate)


###time series anlayses
output <- list()
lowspell=list()
highspell=list()
baseflows_mean=list()
baseflows_annual=list()
sstoutputsub=list()
bimodality.ampl<-list()
bimodality.coeff<-list()
bimodality.ratio<-list()
sst.modes<-list()
dip.Statistic<-list()
dhd<-list()
ave<-list()
stdev<-list()


mypath <- file.path("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output",paste("myplots_HighSpell", ".pdf", sep = ""))
pdf(file=mypath)

for(i in seq_along(names(sst_a))){
    		y <- sst_a[,i]
        output[[i]] = cbind(date, data.frame(y))
    		colnames(output[[i]])<-c("Date","Q")
    		sstoutputsub[[i]] <- with(output[[i]], output[[i]][(Date >= onset[[i]] & Date <= obsdate[[i]]), ])
    		stdev[[i]]<-sd(sstoutputsub[[i]][,2])
    		ave[[i]]<-mean(sstoutputsub[[i]][,2])
     		#lowspell[[i]]<-low.spells(sstoutputsub[[i]], quant=0.1, ann.stats=TRUE,hydro.year=FALSE)
     		#mypath <- file.path("~/Documents/LatestWorkingFolder/envData/extremes/figures/",paste(i, ".pdf", sep = ""))
     		#pdf(file=mypath)
     		#highspell[[i]]<-high.spells(sstoutputsub[[i]], ind.days = 5, volume=TRUE, threshold=thres[[i]])
        #highspell[[i]]<-high.spells(sstoutputsub[[i]], ind.days = 5, volume=TRUE, quant = 0.9, threshold=NULL)
     		
     	    #baseflows_mean[[i]]<-baseflows(output[[i]],a=0.975, ts="mean")
	 		    #baseflows_annual[[i]]<-baseflows(output[[i]],a=0.975, ts="annual")
	 	
	 		    #dip.Statistic[[i]]<-dip(sstoutputsub[[i]][,2])
			     #bimodality.ampl[[i]]<-bimodality_amplitude(sstoutputsub[[i]][,2],TRUE)
		        #bimodality.coeff[[i]]<-bimodality_coefficient(sstoutputsub[[i]][,2],TRUE)
		        #bimodality.ratio[[i]]<-bimodality_ratio(sstoutputsub[[i]][,2], list = FALSE)
	       	 #sst.modes[[i]]<-modes(sstoutputsub[[i]][,2], type = 1, digits = "NULL", nmore = "NULL")
           #dhd[[i]]<-sstoutputsub[[i]] - thres[[i]]
         }
         
      dev.off()  

   dhd.df <- do.call("cbind",dhd)
   sstoutputsub.df <- do.call("cbind",sstoutputsub)
   lowspell.df <- do.call("rbind",lowspell)
   highspell.df<- do.call("rbind",highspell)
   #baseflows_mean.df <- do.call("rbind", baseflows_mean)
   #baseflows_annual.df<- do.call("rbind",baseflows_annual) 
   bimodality.ampl.sst<-cbind(bimodality.ampl)
   bimodality.coeff.sst<-cbind(bimodality.coeff)
   bimodality.ratio.sst<-cbind(bimodality.ratio)
   sst.modes.sst<-cbind(sst.modes)
   dip.Statistic.sst<-rbind(dip.Statistic)

write.csv(dhd.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/dhd.df.csv")
  
   write.csv(bimodality.ampl.sst, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/bimodality.ampl.sst.csv")
   write.csv(bimodality.coeff.sst, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/bimodality.coeff.ss.csv")
   write.csv(bimodality.ratio.sst, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/bimodality.ratio.sst.csv") 
   write.csv(sst.modes.sst, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/sst.modes.sst.csv")
   write.csv(dip.Statistic.sst, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/dip.Statistic.sst.csv")
   write.csv(highspell.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/highSpell_sst_90perc.csv")
   write.csv(lowspell.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/sst_lowspell.df.csv")
   write.csv(sstoutputsub.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/SST_uniqueDates.csv") 
   
   
###time series cluster analyses 

##if clustering entiure time series
sstDat<-data.frame(apply(sstDat, 2, function(x) as.numeric(x)))

#if clustering based on 90 days
a<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/SST_uniqueDates.csv") 
dim(a)
b<-append("Q",paste("Q", seq(1:234), sep="."))
sstDat<-subset(a, select=b)
dim(sstDat)
colnames(sstDat)<-labels
clustering <- pdclust(sstDat)
clustering
#truth.col <- rep(c("red", "blue","black","green","grey"), each = 47)
#truth.col <- rep(c("red", "blue","black"), each = 78)
#plot(clustering, cols = truth.col, type ="triangle")
plot(clustering, labels=F, type="rectangle",timeseries.as.labels = F, p.values=F)
#loo1nn(clustering, truth.col)
mine <- entropyHeuristic(sstDat,m.min=3, m.max=7, t.min = 1, t.max = 1)
summary(mine)
plot(mine,type = "image", mark.optimum = TRUE,col = heat.colors(12))

#plot(clustering, cols=c(rep("red",5),rep("blue",5)))
clust<-cutree(clustering, k = 4)
sstdatClust<-rbind(clust,sstDat)
sstdatClustFin<-t(sstdatClust)
colnames(sstdatClustFin)[1]<-"cluster"
colnames(sstdatClustFin)[2:92]<-seq(1:91)
sstdatClustFin<-as.data.frame(sstdatClustFin)
sstdatClustFin$cluster<-as.character(sstdatClustFin$cluster)
sstdatClustFin$site<-rownames(sstdatClustFin)
molten<-melt(sstdatClustFin, id.var =c("site","cluster"))
molten$variable<-as.numeric(molten$variable)
molten$x<-as.numeric(molten$variable)
molten.sstClust<-cbind(molten,rep(1:235, 91))
colnames(molten.sstClust)<-c("site","cluster","day","sst","x","stack")

molten.sstClust1<-cbind(molten.sstClust,rep(pts$grp6.check , 91))

colnames(molten.sstClust1)<-c("site","cluster","day","sst","x","stack","siteCoralClust")

write.csv(sstdatClustFin,"/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/sstDistributionCluster.csv")


Clust.Mean<-aggregate(sstdatClustFin[,2:92], by=list(Cluster=sstdatClustFin$cluster),FUN = mean, na.rm=TRUE)
Clust.sd<-aggregate(sstdatClustFin[,2:92], by=list(Cluster=sstdatClustFin$cluster),FUN = sd, na.rm=TRUE)
 
dat.mean<-as.data.frame(t(Clust.Mean))
dat.mean<-dat.mean[2:91,]
dat.mean.num<-apply(dat.mean, 2, function(x) as.numeric(x))
dat.mean.num<-as.data.frame(cbind(dat.mean.num, seq(1:90)))

dat.sd<-data.frame(t(Clust.sd))
dat.sd<-dat.sd[2:91,]
dat.mean.sd<-apply(dat.sd, 2, function(x) as.numeric(x))
dat.mean.sd<-as.data.frame(dat.mean.sd)

mean.stack<-melt(dat.mean.num, id="V5")
colnames(mean.stack)<-c("day","cluster","ave.sst")
sd.stack<-stack(dat.mean.sd)
colnames(sd.stack)<-c("sd","cluster")

plot.sst<-cbind(mean.stack,sd.stack$sd)
colnames(plot.sst)[4]<-"sd"

ggplot(plot.sst, aes(day,ave.sst)) + geom_line()+facet_wrap(~cluster) + geom_ribbon(aes(ymin=ave.sst-sd, ymax=ave.sst+sd, x=day), alpha = 0.3) + ylab("Average SST")



### dhw calculation
rm(list=ls())
pts1<-read.csv("/Users/josephmaina/Dropbox/Global Bleaching/Paper/PaperVersion2/data/masterfile.for.analyses.withtaxa_16feb.csv")
pts<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/masterfile_29Oct2017.csv")
x<-(pts1$unique.id)
y<-(pts$unique.id)
diff<-which(!x%in%y)
##load and prepare dhw data
dhw<-read.csv("/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/data/dhw.extr.092017.csv")
dhw<-dhw[-diff,]

##remove rows diff
dhw.ts.t<-t(dhw)
dhwDat<-  data.frame(dhw.ts.t[3:1310,])
dim(dhwDat)
rownames(dhwDat)<-seq(1:1308)
date<-seq(as.Date("2013/06/03"), by = "day", length.out = 1308)
year<-as.numeric(format(date,"%Y"))
month<-as.numeric(format(date,"%m"))
dhwData<-cbind(data.frame(date),month,year,dhwDat)
pts_sub<-pts[,1:29]
labels<-as.list(paste(pts_sub$unique.id, pts_sub$location,pts_sub$site, sep="."))
colnames(dhwData)[4:229]<-labels
dhw_a<-dhwData[,4:229]

##get threshold
#mmm.dhw<-read.csv("~/Dropbox/Global Bleaching/PaperVersions2&3/data/Env2017/dhw/dhw.extr.csv")
thresso<-as.list(dhw[,2])
thres<-lapply(thresso, function(x) x+1)

#create sapmling dates
bl.dates<-pts[,c(7,8,9)]
bl.dates$Obsdate<-paste(bl.dates[,1],bl.dates[,2],bl.dates[,3], sep="-")
bl.dates$onset<- as.Date(bl.dates$Obsdate)-90
onset<-as.list(bl.dates$onset)
obsdate<-as.list(bl.dates$Obsdate)

##subset the dhw

output <- list()
sstoutputsub=list()
maxdhw<-list()
avedhw<-list()
dhwoutputsub=list()
cummsumdhw=list()
sumdhw=list()


for(i in seq_along(names(dhw_a))){
        y <- dhw_a[,i]
        output[[i]] = cbind(date, data.frame(y))
        colnames(output[[i]])<-c("Date","Q")
        dhwoutputsub[[i]] <- with(output[[i]], output[[i]][(Date >= onset[[i]] & Date <= obsdate[[i]]), ])
        maxdhw[[i]]<-max(dhwoutputsub[[i]][,2])
        avedhw[[i]]<-mean(dhwoutputsub[[i]][,2])
        sumdhw[[i]]<-sum(dhwoutputsub[[i]][,2])
         }
           
   
sumdhw.df <- do.call("rbind",sumdhw)

   dhwoutputsub.df <- do.call("cbind",dhwoutputsub)
   dhwmax.90days.df <- do.call("rbind",maxdhw)
   dhwave.90days.df <- do.call("rbind",avedhw)
   dhd.df<-do.call("cbind",dhd)
write.csv(sumdhw.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/cummsumdhw.df.csv")
  
   write.csv(dhwoutputsub.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/dhwoutputsub.df.csv")
   write.csv(dhwmax.90days.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/dhwmax.90days.df.csv")
  write.csv(dhwave.90days.df, "/Users/josephmaina/OneDrive - Macquarie University/Macquarie/Papers/BleachingPaper/GlobalBleachingVer3/output/dhwave.90days.df.csv")
  




































