
PROJHOME <- "/Users/emilydarling/GitHub/WCS-Marine/2016 Bleaching"
DROPBOX <- "/Users/emilydarling/Dropbox/Global Bleaching"

source(file.path(PROJHOME, "R code", "0_DATA SOURCE.R"))

brt <- read.csv(file.path(DROPBOX, "Paper", "Paper 2", "analysis", 
                          "Brt output", "variables_rel importance.csv"), 
                header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

#add variable names
brt <- left_join(brt, vars)
head(brt)

#set factor level for context, temp grouping
brt$label <- factor(brt$label, 
                    levels=unique(brt$label[order(brt$order, brt$label)]), ordered=TRUE)
levels(brt$label)

#set colour by rel-influence and context 
brt$important <- ifelse(brt$rel.inf <=5 , "low", "high")
unique(brt$important)

#ggplot - fig 2 variable importance
ggplot(data = brt) + 
  #geom_bar(aes(x = reorder(label, rel.inf), y = rel.inf, 
   geom_bar(aes(x = label, y = rel.inf,              
               fill  = group, 
               alpha = important), 
           stat = "identity") + 
  scale_x_discrete(limits = rev(levels(brt$label))) + 
  scale_y_continuous(breaks = c(0,5,10,20,30)) + 
  scale_alpha_discrete(range = c(1,0.1), 
                       guide = "none") + 
  theme_bw(base_size=18) + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(size = 18),
        #axis.title.x=element_blank(), 
        legend.position = "top",
        plot.margin = unit(c(1,1,1,1), "cm"),
        text=element_text(family="Times"), 
        plot.background = element_rect(fill = "transparent"), 
        axis.ticks.y = element_blank()) +
  coord_flip() + 
  scale_fill_manual("Variable type", values = c("grey30", "red")) + 
  ylab("Relative influence") + 
  #scale_y_discrete(position = "right") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

ggsave(file.path(DROPBOX,"Paper", "Paper 2", 
                 "analysis", "figures", "from R", 
                 "Fig 2 - BRT relative influence.pdf"), 
       height = 8.6, width = 7)
