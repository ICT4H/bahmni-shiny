library(dplyr)
library(readr)
library(data.table)
obs_grp <- read_csv("observations_2017_2_18.csv")
names(obs_grp) <- make.names(names(obs_grp))


initial_group <- c("Patient.Identifier", "Gender", "Age", "Obs.Date","Location")
dots = sapply(initial_group, . %>% {as.formula(paste0('~', .))})

obs_grp <- obs_grp %>% group_by_(.dots = dots) %>% summarise_all(funs(paste0(., collapse=" ")))

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
#by=list(GroupID, Patient.Identifier, Gender, Age, Obs.Date,Location)
#obs_dt <- obs_dt[, lapply(.SD, paste0, collapse=" "),
#                keyby=list("GroupID", "Patient.Identifier", "Gender", "Age", "Obs.Date","Location")]