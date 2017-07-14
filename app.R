#To be executed only once for first time when this app is run on a particular machine
#to ensure correct packages are installed
# pkgs_to_remove <- c("install.load", "devtools", "DBI", "pool", "shiny",
#                     "tidyr", "stringr", "readr","lubridate", "RMySQL","readr",
#                           "ggplot2", "scales", "eeptools", "data.table",
#                           "dplyr","DT","shinyjs","shinyBS","purrr",
#                           "lazyeval","DescTools")
# 
# remove.packages(pkgs_to_remove)
install.packages('RMySQL', type='source')
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
pkgs_to_install_load <- c("tidyr", "stringr", "readr","lubridate", "RMySQL","readr",
                          "ggplot2", "scales", "eeptools", "data.table",
                          "dplyr","DT","shinyjs","shinyBS","purrr",
                          "lazyeval","DescTools")
sapply(pkgs_to_install_load,install_load)





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
options(shiny.trace=F)

source("connector.R")
source("ui.R")
source("dao.R")
source("serverModules.R")

pool <- getConnectionPool()
observationTabNS <- "observation"

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$head(tags$style(".btnbottomAlign{margin-top: 25px;}")),
  bsButton("inExplorer", label = "Files",
           block = F, type = "toggle", value = TRUE),
  pageWithSidebar(
    headerPanel("Welcome!"),
    sideBarUI("sideBar"),
    mainPanel(
      tabsetPanel(
        id="inTabPanel",
        tabPanel('Observations', observationTabUI(observationTabNS)),
        tabPanel('Bars and Charts',barChartTabUI()),
        tabPanel('Dashboard')
      )
    )
  )
)

server <- function(input, output, session) {
  main_table <- reactiveValues(data = NULL)
  main_plot <- reactiveValues(data = NULL)
  table_data <- reactiveValues(data = NULL)
  selectChoices <- reactiveValues(data = list("Class" = 1, "Question" = 2, "Answer" = 3))
  
  callModule(sideBar, "sideBar")
  callModule(observationTab,observationTabNS, pool, main_table)

  dateTimeConcepts <- getDateTimeConcepts(input, pool)
  conceptForSelection <- getConceptForSelection(input, pool)
  conceptAnswers <- getConceptAnswers(input, pool)

  observeEvent(input$inExplorer, {
    if(input$inExplorer){shinyjs::show(id = "sideBar-inSaveSideBar")}
    else{shinyjs::hide(id = "sideBar-inSaveSideBar")}
  })
  
  observeEvent(input$inTabPanel, {
    if(input$inTabPanel=="Bars and Charts"){
      updateCheckboxGroupInput(session, 
                               "inDimensions",
                               choices = names(main_table$data),
                               selected = NULL
      )
      updateSelectInput(session, 
                        "inCharts",
                        choices = list("Table"=1, "Bar Chart"=2, "Histogram"=3,"Scatter Plot"=4)
      )
      
    }
    else{
      # Can also set the label and select items
      updateSelectInput(session, "observation-inSelect",
                        label = "",
                        choices = selectChoices$data,
                        selected = 2
      )
      updateSelectInput(session, "observation-inDateBy",
                        label = "Date Filter",
                        choices = dateTimeConcepts()
      )
      
    }
  })

  #scatter plot ranges
  scatter_ranges <- reactiveValues(x = NULL, y = NULL)
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$scatter_dblclick, {
    brush <- input$scatter_brush
    if (!is.null(brush)) {
      scatter_ranges$x <- c(brush$xmin, brush$xmax)
      scatter_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      scatter_ranges$x <- NULL
      scatter_ranges$y <- NULL
    }
  })
  #Charts and Graphs
  observeEvent(input$inShow, {
    chartOption <- input$inCharts
    grp_cols <- input$inDimensions
    obs <- main_table$data
    dots <- lapply(grp_cols, as.symbol)
    if(chartOption == 1){
      tableop <- ftable(droplevels(obs[grp_cols]))
      table_data$data <- tableop
      output$tableDF <- renderTable(as.matrix(tableop),rownames = T)
      selectedValue <- "Table" 
    }else if(chartOption == 2){ #barchart
      output$barPlot <- renderPlot({
        bar_1 <- obs %>% ggplot(aes_string(grp_cols[1]))
        bar_1 <- bar_1 +  geom_bar()
        main_plot$data <- bar_1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
        main_plot$data
      })
      selectedValue <- "Bar Chart" 
    }else if(chartOption == 3){ #histogram
      output$histPlot <- renderPlot({
        hist_1 <- obs %>% ggplot(aes_string(grp_cols[1]))
        main_plot$data <- hist_1 +  geom_histogram(binwidth = input$inHistInput)
        main_plot$data
      })
      selectedValue <- "Histogram" 
    }else if(chartOption == 4){ #scatter plot
      output$scatterPlot <- renderPlot({
        scatter_plot <- obs %>% ggplot(aes_string(x = grp_cols[1], y = grp_cols[2], col = "Gender"))
        scatter_plot <- scatter_plot + geom_point()
        main_plot$data <- scatter_plot + coord_cartesian(xlim = scatter_ranges$x, ylim = scatter_ranges$y)
        main_plot$data
      })
      selectedValue <- "Scatter Plot" 
    }
    updateNavbarPage(session,"inChartMenu", selected = selectedValue )
    output$tableDownload <- renderUI({
      tagList(
        downloadButton('downloadTable', 'Download'),
        actionButton("inAddtoDB","Add to Dashboard")
      )
    })
  })

  #Download Table
  output$downloadTable <- downloadHandler(
    filename = function() { 
      chartOption <- input$inCharts
      fextn <- ifelse(chartOption ==1, ".csv",".png")
      fprefix <- ifelse(chartOption ==1, "table","plot")
      paste(fprefix, "_",year(ymd_hms(Sys.time())),
            "_",month(ymd_hms(Sys.time())),
            "_",day(ymd_hms(Sys.time())),
            "_",hour(ymd_hms(Sys.time())),
            "_",minute(ymd_hms(Sys.time())),
            "_",second(ymd_hms(Sys.time())),
            fextn, sep='') 
    },
    content = function(file) {
      chartOption <- input$inCharts
      if(chartOption == 1){
        op_df <- stats:::format.ftable(table_data$data, quote=FALSE)
        write.csv(op_df, file)
      }else{
        ggsave(file, plot = main_plot$data, device = "png" )
      }
    }
  )
}

shinyApp(ui = ui, server = server)