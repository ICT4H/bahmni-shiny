 output$summary <- renderPrint({
    #summary(mtcars)
    op <- matrix(paste("Content", LETTERS[1:16]), 
             ncol=4, byrow = TRUE)
    htmlTable(op,
              header =  paste(c("1st", "2nd",
                                "3rd", "4th"), "header"),
              rnames = paste(c("1st", "2nd",
                               "3rd", "4th"), "row"),
              rgroup = c("Group A",
                         "Group B"),
              n.rgroup = c(2,2),
              cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
              n.cgroup = c(2,2), 
              caption="Basic table with both column spanners (groups) and row groups",
              tfoot="&dagger; A table footer commment")
    
  })

obs_tod_pres <- read_csv("obs_tod_presentation.csv")


obs_tod_pres_1 <- obs_tod_pres %>% rename(PatID = `Patient Identifier`,ObsDate = `Obs Date`) 

obs_tod_pres_1<-obs_tod_pres_1 %>% replace(is.na(obs_tod_pres_1),"")
df <- data.table(obs_tod_pres_1)

df1 <- df[, lapply(.SD, paste0, collapse=" "), by=list(PatID,Gender,Age,ObsDate,Location)]
df1 <- as.data.frame(df1)
df1[, sapply(df1, is.character)] <-
  sapply(df1[, sapply(df1, is.character)],
         str_trim)
grp_cols <- c("Delivery Note, Method of Delivery","Delivery Note, Fetal Presentation")
dots <- lapply(grp_cols, as.symbol)
tableop <- ftable(df1[c("Delivery Note, Method of Delivery","Delivery Note, Fetal Presentation", "Gender")])





library(htmlTable)



    fasting_sugar <- read_csv("obs_fast_sugar_1.csv")
    fasting_sugar <- fasting_sugar %>% mutate(Gender = as.factor(Gender))
    fast_table <- ftable(Gender + Age.Group ~  Obs.Date + Blood.Sugar.Fasting.Group, data = fasting_sugar)
    tmp <- htmlTable(fast_table, rnames = rep(unlist(attr(fast_table, "row.vars")[2]), 70), 
              rgroup=unlist(attr(fast_table, "row.vars")[1]),
              n.rgroup = rep(4,70), 
              header = rep(unlist(attr(fast_table, "col.vars")[[1]]), 2), 
              cgroup = unlist(attr(fast_table, "col.vars")[[2]]), 
              n.cgroup = c(2,2),caption =  names(attr(fast_table, "col.vars")) )



                            Gender         F             M       
                          Age.Group Adults Elders Adults Elders
Blood.Sugar.Fasting.Group                                      
Chronic                                  7      7     15     11
High                                     4      0     10     16
Normal                                  15     16     44     46
PreDiabetic                             16     10     34     40



    fasting_sugar <- read_csv("obs_fast_sugar_1.csv")
    fasting_sugar <- fasting_sugar %>% mutate(Gender = as.factor(Gender))
    fast_table <- ftable(Gender + Age.Group ~ Blood.Sugar.Fasting.Group, data = fasting_sugar)
    htmlTable(fast_table, rnames = unlist(attr(fast_table, "row.vars")[1]), 
              header = rep(unlist(attr(fast_table, "col.vars")[[1]]), 2), 
              cgroup = unlist(attr(fast_table, "col.vars")[[2]]), 
              n.cgroup = c(2,2),caption =  "BloodSugarTable" )

              

