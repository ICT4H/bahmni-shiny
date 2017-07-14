#To be executed only once for first time when this app is run on a particular machine
#to ensure correct packages are installed
# pkgs_to_remove <- c("install.load", "devtools", "DBI", "pool", "shiny",
#                     "tidyr", "stringr", "readr","lubridate", "RMySQL","readr",
#                           "ggplot2", "scales", "eeptools", "data.table",
#                           "dplyr","DT","shinyjs","shinyBS","purrr",
#                           "lazyeval","DescTools")
#
# remove.packages(pkgs_to_remove)
install.packages('RMySQL', type = 'source')
if (!"properties" %in% rownames(installed.packages()))
  install.packages("properties")
if (!"install.load" %in% rownames(installed.packages()))
  install.packages("install.load")
library(install.load)
if (!"devtools" %in% rownames(installed.packages()))
  install.packages("devtools")
library(devtools)
if (!"DBI" %in% rownames(installed.packages()))
  devtools::install_github("rstats-db/DBI")
if (!"pool" %in% rownames(installed.packages()))
  devtools::install_github("rstudio/pool")
if (!"shiny" %in% rownames(installed.packages()))
  devtools::install_github("rstudio/shiny")
library(DBI)
library(shiny)
#install the required packages
pkgs_to_install_load <-
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
    "DescTools"
  )
sapply(pkgs_to_install_load, install_load)





# library("tidyr")
# library("stringr")
# library("lubridate")
# library("RMySQL")
# library("readr")
# library("ggplot2")
# library("scales")
# library("eeptools")
# library(data.table)
# library(DBI)
# library(pool)
# library("dplyr")
# library(shiny)
# library(ggplot2)
# library(DT)
# library(shinyjs)
# library(shinyBS)
# library(purrr)
# library(lazyeval)
# library(DescTools)
options(shiny.trace = F)

source("connector.R")
source("ui.R")
source("dao.R")
source("serverModules.R")

pool <- getConnectionPool()
observationTabNS <- "observation"
barChartsTabNS <- "barChart"

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$head(tags$style(".btnbottomAlign{margin-top: 25px;}")),
  bsButton(
    "inExplorer",
    label = "Files",
    block = F,
    type = "toggle",
    value = TRUE
  ),
  pageWithSidebar(
    headerPanel("Welcome!"),
    sideBarUI("sideBar"),
    mainPanel(tabsetPanel(
      id = "inTabPanel",
      tabPanel('Observations', observationTabUI(observationTabNS)),
      tabPanel('Bars and Charts', barChartTabUI(barChartsTabNS)),
      tabPanel('Dashboard')
    ))
  )
)

server <- function(input, output, session) {
  main_table <- reactiveValues(data = NULL)
  main_plot <- reactiveValues(data = NULL)
  table_data <- reactiveValues(data = NULL)
  selectChoices <-
    reactiveValues(data = list(
      "Class" = 1,
      "Question" = 2,
      "Answer" = 3
    ))
  
  callModule(sideBar, "sideBar")
  callModule(observationTab, observationTabNS, pool, main_table)
  callModule(barChartTab,
             barChartsTabNS,
             main_table,
             table_data,
             main_plot)
  
  dateTimeConcepts <- getDateTimeConcepts(input, pool)
  conceptForSelection <- getConceptForSelection(input, pool)
  conceptAnswers <- getConceptAnswers(input, pool)
  
  observeEvent(input$inExplorer, {
    if (input$inExplorer) {
      shinyjs::show(id = "sideBar-inSaveSideBar")
    }
    else{
      shinyjs::hide(id = "sideBar-inSaveSideBar")
    }
  })
  
  observeEvent(input$inTabPanel, {
    if (input$inTabPanel == "Bars and Charts") {
      updateCheckboxGroupInput(
        session,
        "barChart-inDimensions",
        choices = names(main_table$data),
        selected = NULL
      )
      updateSelectInput(
        session,
        "barChart-inCharts",
        choices = list(
          "Table" = 1,
          "Bar Chart" = 2,
          "Histogram" = 3,
          "Scatter Plot" = 4
        )
      )
      
    }
    else{
      # Can also set the label and select items
      updateSelectInput(
        session,
        "observation-inSelect",
        label = "",
        choices = selectChoices$data,
        selected = 2
      )
      updateSelectInput(session,
                        "observation-inDateBy",
                        label = "Date Filter",
                        choices = dateTimeConcepts())
      
    }
  })
}

shinyApp(ui = ui, server = server)