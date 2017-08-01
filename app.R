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
    "ggmap",
    "DescTools"
  )
lapply(pkgs_to_load, library, character.only = TRUE)

options(shiny.trace = F)

source("uiModules.R")
source("dao.R")
source("serverModules.R")
source("pluginServer.R")
source("pluginUI.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome!"),
  uiOutput("tabs")
)

server <- function(input, output, session) {
  pluginTabs <- list()
  pathToPluginsFolder <- "/Users/mritunjd/Documents/projects/bahmni/bahmni-shiny/plugins"
  files <- list.files(pathToPluginsFolder)
  
  lapply(files, FUN=function(file){
    configFileName <- paste(pathToPluginsFolder,"/",file,"/config.json",sep="")
    daoFileName <- paste(pathToPluginsFolder,"/",file,"/dao.R",sep="")
    config <- fromJSON(file = configFileName)
    tabInfo <- list()
    tabInfo$name <- config$name
    tabInfo$ui <- pluginUI(tolower(config$name))
    tabInfo$dataSourceFile <- daoFileName
    pluginTabs <<- c(pluginTabs, list(tabInfo))
  })

  output$tabs <- renderUI({
    myTabs <- list("Observation")
    restTabs <- list("Search",tabPanel("Observations",contentPanelUI("observations")),widths = c(2,10))
    newTabs <- lapply(pluginTabs,FUN = function(tab){
      tabPanel(tab$name, tab$ui)
    })
    myTabs <- c(myTabs, newTabs, restTabs)
    do.call(navlistPanel, myTabs)
  })
  lapply(pluginTabs, FUN = function(pluginTab){
     callModule(plugin, tolower(pluginTab$name), pluginTab$dataSourceFile)
  })
  
  session$onSessionEnded(function() {
    all_cons <- dbListConnections(MySQL())
    for(con in all_cons)
      +  dbDisconnect(con)
  })
  # callModule(contentPanel, "observations")
}

shinyApp(ui = ui, server = server)