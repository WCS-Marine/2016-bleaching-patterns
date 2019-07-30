#This code produces the NCC Fig 1 Bleaching Alerts NOAA raster with bleaching points overlaid
#Uses new package 'ggnewscale' to add a second fill layer :) :) 

library(here)
here()

#load global world map layer
source(here::here("code", "00-load-libraries.R"))
source(here::here("code", "01-data-source.R"))
source(here::here("code", "02-noaa-2016-alerts.R"))
source(here::here("code", "global-base-map_v2.R"))

##alerts.coarse is BAA 7-day max for 2016 base file
head(alerts.coarse)
names(alerts.coarse)
str(alerts.coarse)

#masterfile is to plot
names(masterfile)
summary(masterfile$bleach_intensity)

#create new discrete bleaching intensity score

#rainbow histogram based on set bin sizes
gg_b <- ggplot_build(ggplot(data = masterfile) + 
                       geom_histogram(aes(x = bleach_intensity), binwidth=5))
nu_bins <- dim(gg_b$data[[1]])[1] #find number of bins

ggplot(data = masterfile) + 
  geom_histogram(aes(x = bleach_intensity), binwidth=5, fill = rainbow(nu_bins))

#set bins based on cutoffs of bleaching intensity
masterfile$bleach.class <- as.factor(ifelse(masterfile$bleach_intensity <= 4, "0",
                                            ifelse(masterfile$bleach_intensity > 4 & 
                                                     masterfile$bleach_intensity <= 20, "1", 
                                                   ifelse(masterfile$bleach_intensity > 20 & 
                                                            masterfile$bleach_intensity <= 40, "2", "3"))))
table(masterfile$bleach.class)


#update to colour-blind friendly palette
bleach.scale <- c("#009E73", "#F0E442","#E69F00", "#D54C00")

masterfile %>% 
  select(bleach_intensity) %>% 
  arrange(bleach_intensity)

## GGPLOT Fig 1
fig1a <- ggplot() + geom_raster(data = alerts.coarse, 
                        aes(x = X, y = Y, 
                            fill = BAA.max), 
                        interpolate = TRUE) + #fills in 180 break 
  scale_fill_brewer(palette = "Greys", direction = 1, 
                       #na.value = "grey80",
                       "2016 NOAA \nbleaching alerts", 
                    breaks = c(1,2,3,4), 
                    labels = c("Watch", "Warning", 
                              "Alert Level 1", "Alert Level 2")) +
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id),
           fill = "white", colour = "white") +
  new_scale_fill() +
  geom_point(data = masterfile,
             aes(x = X, y = Y, fill = bleach.class),
             size = 6, alpha = 0.75,
             shape = 21, colour = "black",
             position = position_jitter(width = 1, height = 1)) +
  scale_fill_manual(values = bleach.scale, 
                    #values = c("olivedrab3","yellow","orange","red"), 
                    "Bleaching \nintensity", 
                    labels = c("0-4", "4-20", "20-40", "40+")) +
  scale_x_continuous(limits = c(25,185), expand=c(0,0)) +
  scale_y_continuous(limits = c(-40,30), expand=c(0,0),
                     breaks = c(-30,-20,-10,0,10,20)) +
  coord_equal() +
  theme_sleek(base_size = 14) +
  theme(axis.title = element_blank()) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1)))

fig1a

ggsave(here::here("plots", "fig1-maps-top-row.png"),
       bg = "transparent",
       height = 3.75, width = 10, dpi = 300)


## -----------------------------------------------------
#Zoom ins
#WIO
wio <- ggplot() +  geom_raster(data = alerts.coarse, 
                        aes(x = X, y = Y, 
                            fill = BAA.max)) + #fills in 180 break 
  scale_fill_brewer(palette = "Greys", direction = 1) +
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id),
           fill = "white", colour = "white") +
  new_scale_fill() +
  geom_point(data = masterfile,
             aes(x = X, y = Y, fill = bleach.class),
             size = 4, alpha = 0.75,
             shape = 21, colour = "black",
             position = position_jitter(width = 0.5, height = 0.5)) +
  scale_fill_manual(values = bleach.scale) + 
  scale_x_continuous(limits = c(32,60), expand=c(0,0)) +
  scale_y_continuous(limits = c(-27,-1), expand=c(0,0)) +
  coord_equal() +
  theme_sleek(base_size = 14) +
  theme(axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(),
        legend.position = "none") 
wio

#Australia-Indo
aus.indo <- ggplot() +  geom_raster(data = alerts.coarse, 
                        aes(x = X, y = Y, 
                            fill = BAA.max)) + #fills in 180 break 
  scale_fill_brewer(palette = "Greys", direction = 1) +
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id),
           fill = "white", colour = "white") +
  new_scale_fill() +
  geom_point(data = masterfile,
             aes(x = X, y = Y, fill = bleach.class),
             size = 4, alpha = 0.75,
             shape = 21, colour = "black",
             position = position_jitter(width = 0.5, height = 0.5)) +
  scale_fill_manual(values = bleach.scale) + 
  scale_x_continuous(limits = c(100,140), expand=c(0,0), 
                     breaks = c(100,110,120,130)) +
  scale_y_continuous(limits = c(-25,-2), expand=c(0,0)) +
  coord_equal() +
  theme_sleek(base_size = 14) +
  theme(axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(),
        legend.position = "none") 
aus.indo

#N WIO/Bay of Bengal
n.wio <- ggplot() +  geom_raster(data = alerts.coarse, 
                        aes(x = X, y = Y, 
                            fill = BAA.max)) + #fills in 180 break 
  scale_fill_brewer(palette = "Greys", direction = 1) +
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id),
           fill = "white", colour = "white") +
  new_scale_fill() +
  geom_point(data = masterfile,
             aes(x = X, y = Y, fill = bleach.class),
             size = 4, alpha = 0.75,
             shape = 21, colour = "black",
             position = position_jitter(width = 0.25, height = 0.25)) +
  scale_fill_manual(values = bleach.scale) + 
  scale_x_continuous(limits = c(70,100), expand=c(0,0), 
                     breaks = c(70,80,90)) +
  scale_y_continuous(limits = c(2,15), expand=c(0,0)) +
  coord_equal() +
  theme_sleek(base_size = 14) +
  theme(axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(),
        legend.position = "none")
n.wio 

#Melanesia
melanesia <- ggplot() +  geom_raster(data = alerts.coarse, 
                        aes(x = X, y = Y, 
                            fill = BAA.max)) + #fills in 180 break 
  scale_fill_brewer(palette = "Greys", direction = 1) +
  geom_map(map=world_map, data=world_map, 
           aes(map_id=id),
           fill = "white", colour = "white") +
  new_scale_fill() +
  geom_point(data = masterfile,
             aes(x = X, y = Y, fill = bleach.class),
             size = 4, alpha = 0.75,
             shape = 21, colour = "black",
             position = position_jitter(width = 0.25, height = 0.25)) +
  scale_fill_manual(values = bleach.scale) + 
  scale_x_continuous(limits = c(150,185), expand=c(0,0)) +
  scale_y_continuous(limits = c(-20,-5), expand=c(0,0)) +
  coord_equal() +
  theme_sleek(base_size = 14) +
  theme(axis.title = element_blank(), 
        #axis.text = element_blank(), 
        #axis.ticks = element_blank(),
        legend.position = "none")
melanesia


#put together into cowplot

#put bottom plots together
bottom.maps <- plot_grid(wio, n.wio, aus.indo, melanesia, 
                         labels = "", 
                        # labels = c("B Western Indian Ocean", 
                        #            "C Northern Indian Ocean", 
                        #            "D Australia-Indonesia", 
                        #            "E Melanesia"), 
                        align = "hv", #h most promising 
                        nrow = 1, 
                        rel_widths = c(1, 1, 1, 1))

## -----------------------------------------------------
##Add histograms of bleaching and DHWs as bottom plots F-G

#calculate Hughes et al. 2017 DHW class
names(masterfile)
masterfile$DHW.class <- ifelse(masterfile$max.dhw.90days <= 4, "low",
                               ifelse(masterfile$max.dhw.90days >= 8, "severe", 
                                      "moderate"))

masterfile %>% 
  group_by(DHW.class) %>% 
  summarize(min = min(max.dhw.90days), 
            max = max(max.dhw.90days), 
            n = n(), 
            perc_reefs = round(n/235 * 100,2))

#moderate + severe = 30% of reefs
25.11 + 5.11

#use floor DHW to get integers for plot
dhw.histogram <- ggplot(data = masterfile,
       aes(x = floor(masterfile$max.dhw.90days))) + 
  geom_histogram(aes(fill = DHW.class), 
                 colour = "black", binwidth = 1) + 
  theme_sleek(base_size = 14) +
  theme(panel.grid=element_blank(), 
        #text=element_text(family="Times"),
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_fill_manual(values = c("white","grey","black")) +
  xlab("Degree heating weeks") +
  ylab("# of reefs") +
  scale_x_continuous(breaks = c(0,4,8,12,16))

dhw.histogram

#use floor bleach_intensity to get integers for plot
bleach.histogram <- ggplot(data = masterfile,
       aes(x = floor(masterfile$bleach_intensity))) + 
  geom_histogram(aes(fill = bleach.class), 
                 colour = "black", binwidth = 2) + 
  theme_sleek(base_size = 14) +
  theme(panel.grid=element_blank(), 
        #text=element_text(family="Times"),
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_fill_manual(values = bleach.scale) +
  xlab("Bleaching intensity") +
  ylab("# of reefs") +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60))

bleach.histogram

#need to plot grid together with bottom map maybe
histograms <- plot_grid(dhw.histogram, bleach.histogram, 
          nrow =1, 
          labels = c("F", "G"))

plot_grid(bottom.maps, histograms, 
          align = "h", axis = "l", nrow = 2, 
          rel_heights = c(2.15,1.5))

## ADD ALL TOGETHER FOR FIG 1
ggsave(here::here("plots", "fig1-bottom-map-histogram.png"), 
       bg = "transparent", 
       height = 4.5, width = 10, dpi = 300)



