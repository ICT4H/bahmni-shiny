library(dplyr)
library(readr)
library(data.table)
obs_grp <- read_csv("observations_2017_2_18.csv")

obs_grp %>% group_by(`Patient Identifier`, `Obs Date`) %>% summarise(Count = n())
by_species <- iris %>% group_by(Species)
by_species %>% summarise_at(vars(matches("Width")), mean)
myfunc<-function(x){
  paste0(unique(x),collapse = ";")
}
myreplace<- function(x){
  ifelse(is.na(x),"",x)
}
tmp <- obs_grp %>% group_by(`Patient Identifier`, `Obs Date`) %>% 
  lapply(function(x){ifelse(is.na(x),"",x)}) %>% 
  summarise_at(vars(`Delivery Note, Fetal Presentation`,`Delivery Note, Method of Delivery`),myfunc)
tmp <- myfunc(obs_grp$`Delivery Note, Fetal Presentation`)
paste(unique(obs_grp$`Delivery Note, Fetal Presentation`),collapse = "")
