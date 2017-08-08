install.packages('RMySQL', type = 'source', repos="http://cran.rstudio.com/")
if (!"properties" %in% rownames(installed.packages()))
  install.packages("properties", repos="http://cran.rstudio.com/")
if (!"install.load" %in% rownames(installed.packages()))
  install.packages("install.load", repos="http://cran.rstudio.com/")
library(install.load)
if (!"devtools" %in% rownames(installed.packages()))
  install.packages("devtools", repos="http://cran.rstudio.com/")
library(devtools)
if (!"DBI" %in% rownames(installed.packages()))
  devtools::install_github("rstats-db/DBI")
if (!"pool" %in% rownames(installed.packages()))
  devtools::install_github("rstudio/pool")
if (!"shiny" %in% rownames(installed.packages()))
  devtools::install_github("rstudio/shiny")

 pkgs_to_install_load <-
  c(
    "tidyr",
    "stringr",
    "readr",
    "lubridate",
    "RMySQL",
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
    "plotly",
    "ggmap",
    "leaflet",
    "DescTools"
  )
lapply(pkgs_to_install_load, FUN = function(pkg){
  install.packages(pkg, repos="http://cran.rstudio.com/")
})