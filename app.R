library(install.load)
library(DBI)
library(shiny)
library(properties)

#load the required packages
pkgs_to_load <-
  c(
    "tidyr",
    "stringr",
    "lubridate",
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
    "leaflet",
    "plotly",
    "bcrypt",
    "RSQLite",
    "shinycssloaders",
    "DescTools"
  )
lapply(pkgs_to_load, library, character.only = TRUE)

options(shiny.trace = F)

source("connector.R")
source("uiModules.R")
source("serverModules.R")
source("pluginServer.R")
source("pluginUI.R")

ui <- fluidPage(
  tags$link(rel="stylesheet", type="text/css",href="style.css"),
  tags$style("#shiny-notification-panel {top:0;left:35%;width:40%}"),
  tags$style("#shiny-modal .shiny-spinner-placeholder {height:150px !important}"),
  useShinyjs(),
  ## Login module;
  div(class = "login",
      uiOutput("uiLogin")
  ), 
  uiOutput("tabs")
)

properties <- read.properties("app.properties")

initializeApp <- function(input, output, session) {
  if(!dir.exists("derivedColumns")){
    dir.create("derivedColumns")
  }
  pluginTabs <- list()
  pathToPluginsFolder <- properties$pluginsFolder
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
    newTabs <- lapply(pluginTabs,FUN = function(tab){
      tabPanel(tab$name, tab$ui)
    })
    myTabs <- c(newTabs, list(widths = c(2,10)))
    do.call(navlistPanel, myTabs)
  })
  lapply(pluginTabs, FUN = function(pluginTab){
     callModule(plugin, tolower(pluginTab$name), pluginTab$dataSourceFile, pluginTab$name)
  })
}

server <- function(input, output, session) {
  source("login.R", local=T)
  observe({
    if(USER$Logged){
      initializeApp(input, output, session)
    }
  })
}

shinyApp(ui = ui, server = server)