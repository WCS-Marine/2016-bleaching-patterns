rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)
library(here)
# Load data of matched satellite and insitu SST data 90 days before bleaching observations. Temperature differences (Insitu - Satellite) have been precalculated in the sheet 
dat <- read.csv(here("data","90DaySat_Insitu_Diff.csv"))
dat2 <- dat %>%
  melt(id.vars="Days_Index", measure.vars=c(27:39), variable.name="Site", value.name = "values")
dat2$Site1 <- ordered (dat2$Site, levels= c("Watamu","Kuruwitu","Mradi","Mombasa_lagoon","Mombasa_Offshore","Kisite","Changuu","Bongoyo","Mbudya","Chumbe","Planch_Aliz","Potato_Patch","Japanese_Garden"), labels= c("Watamu-Kenya","Kuruwitu-Kenya","Mradi-Kenya","Mombasa lagoon-Kenya","Mombasa Offshore-Kenya","Kisite-Kenya","Changuu-Tanzania","Bongoyo-Tanzania","Mbudya-Tanzania","Chumbe-Tanzania","Planch' Alizé-Reunion","Kadmat East-India","Agatti East-India"))

#plot data
ggsat_Insitu_Diff <- ggplot(data = dat2, aes(x=Days_Index, y=values)) + geom_point(aes(fill=values), shape=21 , size=3) + facet_wrap(~Site1) + geom_abline(slope =  0, intercept = 0, lty=2) + theme_bw() + ylab (expression(paste("Temperature"^" 0","C, In situ - satellite"))) + xlab ("Days before bleaching observations") + scale_fill_gradient2(low = "blue", high="red")+ theme(legend.position = c(0.4,0.08),strip.text.x = element_text(size=10), axis.title = element_text(size=12), axis.text = element_text(size=10), legend.box = "vertical", legend.direction = "horizontal") + labs(fill=expression(paste("Temperature"^" 0","C"))) + guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

ggsat_Insitu_Diff

#Save as .eps 
postscript(file=here("plots","Fig_Insitu_Sat_Diff.eps"), width=7.5, height=6)
ggsat_Insitu_Diff
dev.off()
