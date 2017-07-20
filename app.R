#To be executed only once for first time when this app is run on a particular machine
#to ensure correct packages are installed
# pkgs_to_remove <- c("install.load", "devtools", "DBI", "pool", "shiny",
#                     "tidyr", "stringr", "readr","lubridate", "RMySQL","readr",
#                           "ggplot2", "scales", "eeptools", "data.table",
#                           "dplyr","DT","shinyjs","shinyBS","purrr",
#                           "lazyeval","DescTools")
#
# remove.packages(pkgs_to_remove)
library(install.load)
library(DBI)
library(shiny)
#load the required packages
pkgs_to_load <-
  c(
    "tidyr",
    "stringr",
    "readr",
    "lubridate",
    "RMySQL",
    "readr",
    "ggplot2",
    "scales",
    "eeptools",
    "data.table",
    "dplyr",
    "DT",
    "shinyjs",
    "shinyBS",
    "purrr",
    "lazyeval",
    "rjson",
    "DescTools"
  )
lapply(pkgs_to_load, library, character.only = TRUE)

options(shiny.trace = F)

source("uiModules.R")
source("dao.R")
source("serverModules.R")

buildPluginUI <- function(name) {
    tabPanel(name,contentPanelUI(name))
}


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome!"),
  uiOutput("pluginstab")
)

server <- function(input, output) {
  tabnames <- list()
  values <- list()
  pathToPluginsFolder <- "/Users/mritunjd/Documents/projects/bahmni/bahmni-shiny/plugins"
  files <- list.files(pathToPluginsFolder)
  
  lapply(files, FUN=function(file){
    configFileName = paste(pathToPluginsFolder,"/",file,"/config.json",sep="")
    config = fromJSON(file = configFileName)
    tabnames <<- c(tabnames, config$name)
    values <<- c(values, contentPanelUI(tolower(config$name)))
  })
  output$pluginstab <- renderUI({
    myTabs <- list("Observation")
    restTabs <- list("Tests",tabPanel("RBC",contentPanelUI("rbc")),widths = c(2,10))
    pluginTabs <- tabnames %>% map2(values, function(.x, .y){
      tabPanel(.x, .y)
    })
    myTabs <- c(myTabs, pluginTabs, restTabs)
    do.call(navlistPanel, myTabs)
  })
  
  lapply(tabnames, FUN = function(tabname){
    callModule(contentPanel, tolower(tabname))
  })
  callModule(contentPanel, "rbc")
}

shinyApp(ui = ui, server = server)