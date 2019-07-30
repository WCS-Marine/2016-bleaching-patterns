#this file loads data source

#usr <- "stephdagata"  
#usr <- "emilydarling"
# usr <- "Azali"
# PROJHOME <- paste("/Users/",usr,"/GitHub/WCS-Marine/2016 Bleaching",sep="")
# DROPBOX <- paste("/Users/",usr,"/Dropbox/Global Bleaching",sep="")

#recoding using here() functions 
here()


# ----------------------------------------------------------------------------
#read masterfile
masterfile <- fread(here::here("data","masterfile_29Oct2017.csv"))

#12 countries
unique(masterfile$country)
unique(masterfile$region)

head(masterfile)
names(masterfile)
#----------------------------------------------------------------------------

#check Antongil Bay site and remove
# masterfile <- masterfile %>% 
#   as_tibble() %>% 
#   filter(location != "Antongil Bay")

nrow(masterfile)

#how is perc bleached correlated to bleach severity index
cor(masterfile$perc_bleached, masterfile$bleach_intensity)

summary(masterfile$perc_bleached)
summary(masterfile$bleach_intensity)


# ----------------------------------------------------------------------------
#read dhw Maina max and timing calcs -- 235 sites included in analysis
# dhw.sampling <- read.csv(file.path(PROJHOME, "PaperVersions2&3", "data",
#                                    "Env2017", "GlobalBleachingVer3", "data",
#                                    "dhw.maxdate.15to16.csv"), 
#                        header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
# head(dhw.sampling)
# names(dhw.sampling)
# 
# #read dhw extract - raw - 235 rows and many columns - daily DHWs 
# dhw.extract <- read.csv(file.path(PROJHOME, "PaperVersions2&3", "data",
#                                    "Env2017", "GlobalBleachingVer3", "data",
#                                    "dhw.extr.092017.csv"), 
#                          header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>% 
#   select(X:maximum.of.monthly.mean.sea.surface.temperature.climatology, X20150901:X20161231)
# nrow(dhw.extract)
# names(dhw.extract)
# 
# dhw.sampling <- cbind(dhw.sampling, dhw.extract)
# write.csv(dhw.sampling, file.path(PROJHOME, "PaperVersions2&3", "data",
#                                   "Env2017", "GlobalBleachingVer3", "data",
#                                   "dhw.maxdate.15to16_withextract.csv"), 
#           row.names = FALSE)

#read clipped DHW extract (only heating event around sampling date)
#dhw.extract <- read.csv(file.path(DROPBOX, "Paper", "Paper 2",
#"analysis", "Env2017", "GlobalBleachingVer3", "data",
# "dhw.maxdate.15to16_withextract.csv"), 
#header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 

#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# read SST extract
#sst.extract <- read.csv(file.path(DROPBOX, "Paper", "Paper 2",
# "analysis", "Env2017", "GlobalBleachingVer3", "data",
#  "SSTData.csv"), 
# header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) %>% 
#select(-c(X,month,year)) %>% 
# melt(id.var = "date", variable.name = "site", value.name = "sst") %>% 
# select(date,site,sst) %>% 
# mutate(date = ymd(date)) %>% 
# mutate(site = str_replace_all(site, "X", ""),
#   site =  gsub("[[:punct:] ]+","",site))

#head(sst.extract)
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------
#need to update for version 3
#export variable names from masterfile
# var.names <- masterfile %>% 
#   select(DHWCummSUM1:CA1)
# names(var.names)
# 
# new.export <- data.frame("var" = names(var.names))
# new.export
# write.csv(new.export, 
#           file.path(file.path(DROPBOX, "Paper", "Paper 2",
#                               "analysis", "Env2017", "GlobalBleachingVer3", "data",
#                               "new variable names.csv")), 
#           row.names = FALSE)


#read variable names
#vars <- read.csv(file.path(DROPBOX, "Paper", "Paper 2", "analysis",
#"variable names-labels.csv"), 
# header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
#head(vars)
#unique(vars$label)
vars <- fread(here::here("data", "var_names.csv"))
head(vars)

vars$var #var
vars$label #label for plots

