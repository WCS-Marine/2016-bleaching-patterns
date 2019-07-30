#not sure about this code

library(here)
source(here::here("code", "00-load-libraries.R"))
source(here::here("code", "01-data-source.R"))

library(stringr)

#read masterfile
# aic <- read.csv(file.path(DROPBOX,"Paper", "Paper 1", "Paper1 Version3","data",
#                           "ModelSel_bleach_intensity.csv"),
#                 header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 

aic <- fread(here::here("data",  "ModelSel_bleach_intensity.csv"))
head(aic)
names(aic)

names(aic)[1:5]

head(vars)

#remove any duplicated rows
aic <- distinct(aic[,-1], .keep_all = TRUE) %>% #removes model number
  arrange(delta) %>% 
  mutate(model.order = seq(1, nrow(aic), by = 1))

head(aic)


#top2 AIC tables
length(names(aic))
names(aic)
aic.2AIC <- filter(aic, delta <= 2) %>% 
  melt(id.vars = c("model.order","AIC","BIC", "df", "logLik", "AICc", "delta", "weight")) %>% 
  na.omit()
head(aic.2AIC)

unique(aic.2AIC$variable)

#single variables - in top models
min(aic$df)
aic.single <- filter(aic, df < 6)  %>% #note doesn't grab habitat, df > 5
  melt(id.vars = c("model.order","AIC","BIC", "df", "logLik", "AICc", "delta", "weight")) %>% 
  na.omit()
head(aic.single)
unique(aic.single$variable)

#find habitat
aic.habitat <- filter(aic, habitat != "NA") %>% 
  filter(df == min(df)) %>% 
  select(model.order,df,logLik,AICc,delta,weight)
aic.habitat$variable <- "habitat"

#bind
head(aic.2AIC)
head(aic.single)
head(aic.habitat)


aic.output <- bind_rows(aic.2AIC, aic.single, aic.habitat)
names(aic.output)

names(aic.output)[9] <- "var"
head(aic.output)

head(vars)

aic.output <- left_join(aic.output, vars)
names(aic.output)

unique(aic.output$label)

#summarize into AIC output table
aic.output

aic.output <- aic.output %>% 
  filter(var != "Int") %>% 
  select(model.order,df,logLik,AICc,delta,weight,label) %>% 
  mutate(label = as.character(label)) %>% 
  group_by(model.order,df,logLik,AICc,delta,weight) %>% 
  summarize(vars = as.character(str_c(label,collapse=' + ')))

aic.output

write.csv(aic.output, file.path(DROPBOX,"Paper", "Paper 1", "Paper1 Version3","outputs",
                                "Table - AIC output.csv"),
          row.names = FALSE)

##### Significance coef_bleach Intensity ####
coefbleach <- read.csv(file.path(DROPBOX,"Paper", "Paper 1",
                                 "Paper1 Version3", "data", "CoeficientTables",
                                 "coeftable_bleach_intensity.csv"),
                       header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

coefbleach <- coefbleach %>% 
  mutate(coef.direction = ifelse(StdCoef <= 0, "neg","pos"), 
         sig = ifelse(coef.direction == "pos" & `conf2.5` < 0, "ns", 
                      ifelse(coef.direction == "neg" & `conf97.5` > 0, "ns", "sig")), 
         sig.direction = ifelse(sig == "sig", 
                                paste(sig, coef.direction, sep = "_"), "ns"))
