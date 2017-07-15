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

options(shiny.trace = F)

source("ui.R")
source("dao.R")
source("serverModules.R")

observationTabNS <- "observation"
barChartsTabNS <- "barChart"

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Welcome!"),
  navlistPanel(
    "Observations",
    tabPanel("Hypertension",contentPanelUI("hypertension")),
    tabPanel("Diabetes",contentPanelUI("diabetes")),
    "Tests",
    tabPanel("RBC",contentPanelUI("rbc")),
    widths = c(2,10)
  )
)

server <- function(input, output, session) {
  callModule(contentPanel, "hypertension")
  callModule(contentPanel, "diabetes")
  callModule(contentPanel, "rbc")
}

shinyApp(ui = ui, server = server)