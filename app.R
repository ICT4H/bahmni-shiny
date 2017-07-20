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

pluginUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("content"))
  )
}

plugin <- function(input, output, session, data){
  output$content <- renderText(do.call(paste,data))
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome!"),
  uiOutput("tabs")
)

server <- function(input, output) {
  pluginTabs <- list()
  pathToPluginsFolder <- "/Users/mritunjd/Documents/projects/bahmni/bahmni-shiny/plugins"
  files <- list.files(pathToPluginsFolder)
  
  lapply(files, FUN=function(file){
    configFileName <- paste(pathToPluginsFolder,"/",file,"/config.json",sep="")
    daoFileName <- paste(pathToPluginsFolder,"/",file,"/dao.R",sep="")
    config <- fromJSON(file = configFileName)
    envir <- new.env()
    source(daoFileName,local=envir)
    data <- envir$fetchData(pool)
    envir <- NULL
    tabInfo <- list()
    tabInfo$name <- config$name
    tabInfo$ui <- pluginUI(tolower(config$name))
    tabInfo$data <- data
    
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
  print(pluginTabs)
  lapply(pluginTabs, FUN = function(pluginTab){
    # callModule(plugin, tolower(pluginTab$name), pluginTab$data)
  })
  callModule(contentPanel, "observations")
}

shinyApp(ui = ui, server = server)