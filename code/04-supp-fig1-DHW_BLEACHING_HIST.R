# Aim: PLot biplot between DHW and Bleaching intensity with density of each variable as marginal histogram plot
# Author: Steph D'agata
# Date: April 2018


## Require packages
library(ggplot2)
library(ggExtra)
library(dplyr)

## 1. Load data
#rm(FULL)
#FULL <- read.csv("/Users/stephdagata/Dropbox/Global Bleaching/Paper/Paper 2/analysis/masterfile - analysis variables_20Dec2017.csv",header=T,sep=",",stringsAsFactors = T,check.names = F)
FULL <- fread(here::here("data", "masterfile - analysis variables_20Dec2017.csv"))

head(FULL); dim(FULL)
names(FULL)

# Simple plot
plot(FULL$average.dhw.90days,FULL$bleach_intensity,pch=16)
abline(v=4)


# Classification in 5 categories
HIGH <- (FULL$bleach_intensity >= 40) %>% which()
MED <- (FULL$bleach_intensity >= 20 & FULL$bleach_intensity < 40 ) %>% which()
LOW <- (FULL$bleach_intensity >= 4 & FULL$bleach_intensity < 20 ) %>% which()
rm(V.LOW); V.LOW <- (FULL$bleach_intensity > 0 & FULL$bleach_intensity < 4 ) %>% which()
ZERO <- (FULL$bleach_intensity == 0) %>% which()

# Create new `group`vector
rm(group); group <- vector()
group[HIGH] <- 4
group[MED] <- 3
group[LOW] <- 2
group[V.LOW] <- 1
group[ZERO] <- 0

# cbind new group vector with full matrix
FULL <- cbind(FULL,group)
FULL$group <- FULL$group %>% as.factor()
FULL %>% summary()

table(FULL$group)

# Compute % site for which bleaching > 3 and DHW < 4
PERC_HIGH <- (100*(FULL$group == c(3,4) & FULL$average.dhw.90days < 4) %>% which() %>% length()/ (FULL$group == c(3,4)) %>% which() %>% length()) %>% round(0)
PERC_HIGH

# Multiplot with density distribution
#Remove ZERO group 
FULL <- FULL %>% 
  filter(group != "0")

unique(levels(as.factor(FULL$group)))

# scatter plot of DHW and bleaching variables
# color by groups
#COL <- c("red","orange","yellow","green","grey") # assign colors
COL <- c("#009E73", "#F0E442","#E69F00", "#D54C00")
COL

names(FULL)
summary(FULL$average.dhw.90days)
summary(FULL$max.dhw.90days)

scatterPlot <- ggplot(FULL, aes(max.dhw.90days, bleach_intensity, color=group)) + 
  geom_point(alpha = 0.75, size = 3) + 
  scale_color_manual("Bleaching \nintensity", 
                     values = COL, 
                     labels = c("0-4", "4-20", "20-40", "40+")) + 
  xlab("Degree heating weeks") +
  ylab("Bleaching intensity") +
  theme_sleek() +
  theme(legend.position="none") + #no legend to line up with cowplot
  geom_hline(yintercept=5, linetype="dashed") +
  geom_vline(xintercept=4, linetype="dashed") + 
  scale_x_continuous(limits = c(0,16), expand = c(0.01,0.01)) + 
  scale_y_continuous(limits = c(0,68), expand = c(0.01,0.01), 
                     breaks = c(0,5,20,40,60))

scatterPlot
  
# Marginal density plot of x (top panel)
xdensity <- ggplot(FULL, aes(average.dhw.90days, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = COL) + 
  ylab("Density") +
  theme_sleek() + 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(), 
        legend.position = "none") + 
  scale_x_continuous(limits = c(0,16), expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0))
  
xdensity

# Marginal density plot of y (right panel)
ydensity <- ggplot(FULL, aes(bleach_intensity, fill=group)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = COL) + 
  xlab("Bleaching intensity") +
  theme_sleek() +  
  coord_flip() + 
  scale_x_continuous(limits = c(0,68), expand = c(0.01,0.01)) + 
  scale_y_continuous(expand = c(0,0)) + 
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        legend.position = "none", 
        axis.line = element_line()) + 
  ylab("Density")

ydensity

plot_grid(xdensity, NULL, scatterPlot, ydensity, 
          nrow = 2, 
          rel_heights = c(0.33, 0.67), 
          rel_widths = c(0.75, 0.25))

ggsave(here::here("plots", "SuppFig1-density plots.pdf"), 
       width = 8, height = 8)
