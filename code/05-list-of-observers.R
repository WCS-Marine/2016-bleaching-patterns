#this file creates a table of sources 

#recoding using here() functions 
library(here)
source(here::here("code", "00-load-libraries.R"))

# ----------------------------------------------------------------------------
#read masterfile
masterfile <- fread(here::here("data","masterfile_29Oct2017.csv"))
names(masterfile)

unique(masterfile$observer)

masterfile <- masterfile %>% 
  mutate(observer = fct_recode(observer,
         "Celine/Emma/Natalie" = "Celine / Emma", 
         "Celine/Emma/Natalie" = "Celine/Natalie", 
         "Celine/Emma/Natalie" = "Natalie", 
         "Julien Leblond" = "JulienLeblond", 
         "Azhar dan Muhidin" = "Azhar", 
         "Azhar dan Muhidin" = "Muhidin"))

unique(masterfile$observer)

source.supp <- masterfile %>% 
  group_by(observer, country) %>% 
  tally() %>% 
  dplyr::summarize(Countries = paste(country, collapse= ","), 
                   Sites = sum(n)) %>% 
  arrange(-Sites)

source.supp

here::here()
fwrite(source.supp, here::here("outputs", "list of observers.csv"))
