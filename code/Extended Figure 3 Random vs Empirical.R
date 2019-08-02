library(ggplot2)
library(dplyr)
library(patchwork)

random <- read.csv(here("data", "RandomFrequencies.csv"))


a <- random %>%
  filter(Category_lat %in% c("Empirical", "Random")) %>%
  ggplot(aes(x=lat_bin, y=lat_.tot, color = Category_lat)) + 
  geom_point() + geom_line() + theme_bw(base_size = 14, base_family = "Helvetica") +
  theme(legend.position = "top") + labs(colour="Category", x= "Latitude", y="% of total sample")

b <- random %>%
   filter(Category %in% c("Empirical", "Random")) %>%
   ggplot(aes(x=lon_bin, y=lon_.tot, color = Category)) + 
   geom_point() + geom_line() + theme_bw(base_size = 14, base_family = "Helvetica") +
   theme(legend.position = "none") + xlab("Longitude") +ylab("% of total sample")

 a/b
ggsave(here("plots", "Extended Figure 3 Random vs Empirical.pdf"), height = 8, width = 6) 
