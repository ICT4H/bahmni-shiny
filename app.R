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

pool <- getConnectionPool()

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$head(tags$style(".btnbottomAlign{margin-top: 25px;}")),
  bsButton("inExplorer", label = "Files",
           block = F, type = "toggle", value = TRUE),
  pageWithSidebar(
    headerPanel("Welcome!"),
    sideBarUI(),
    mainPanel(
      tabsetPanel(
        id="inTabPanel",
        tabPanel('Observations', observationTabUI()),
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
  
  dateTimeConcepts <- getDateTimeConcepts(input, pool)
  conceptForSelection <- getConceptForSelection(input, pool)
  conceptAnswers <- getConceptAnswers(input, pool)

  observeEvent(input$inExplorer, {
    if(input$inExplorer){shinyjs::show(id = "inSaveSideBar")}
    else{shinyjs::hide(id = "inSaveSideBar")}
  })
  observeEvent(input$inShowColumns, {
    if(input$inShowColumns){
      shinyjs::show(id = "inColumnNamePanel")
      updateButton(session,"inShowColumns", "Hide Column Selection")
    }else{
      shinyjs::hide(id = "inColumnNamePanel")
      updateButton(session,"inShowColumns", "Show Column Selection")
    }
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
      updateSelectInput(session, "inSelect",
                        label = "",
                        choices = selectChoices$data,
                        selected = 2
      )
      updateSelectInput(session, "inDateBy",
                        label = "Date Filter",
                        choices = dateTimeConcepts()
      )
      
    }
  })
 
  observeEvent(input$inSelect, {
    updateSelectInput(session, 
                      "inCheckboxGroup",
                      label = "",
                      choices = conceptForSelection(),
                      selected = tail(conceptForSelection(), 1)
                    )
  })
  observeEvent(c(input$inCheckboxGroup,input$inSelect),{
      updateSelectInput(session, 
                        "inAnswers",
                        label = "",
                        choices = conceptAnswers(),
                        selected = tail(conceptAnswers(), 1)
                        )
  })
  observeEvent(input$inColumnNames, {
    obs <- main_table$data
    obs <- obs[,input$inColumnNames, drop=F]
    output$obsDT <- DT::renderDataTable(obs, 
                                        options = list(paging=T), 
                                        rownames=F, 
                                        filter = "top")
  })
  observeEvent(input$inApply,{
      #conDplyr <- src_pool(pool)
      #conDplyr <- my_db
      filterBy <- input$inSelect
      listBy <- as.list(input$inCheckboxGroup)
      dateBy <- input$inDateBy
      dateRange <- as.character(input$inDateRange)
      conceptDates <- as.list(input$inDateBy)
      
      concepts <- getConcepts(input, pool)
      if(filterBy ==1){#Concept class
        obs <- getObsForSelection(pool, concepts, "class_id", listBy)
      }else if(filterBy==2){#Concept Name
        obs <- getObsForSelection(pool, concepts, "concept_id", listBy)
        answers <- as.list(input$inAnswers)
        if(!is.null(answers)){
          if(length(answers) > 0){
            obs <- obs %>% filter(value_coded %in% answers)
          }
        }
      }else{
        obs <- getObsForSelection(pool, concepts, "value_coded", listBy)
      }
      conceptNames = getAllConceptNames(pool)
      obs <- mapObsWithConceptNames(obs, conceptNames)
      
      if(dateBy == 1){#ObsDate
        obs <- obs %>% 
          filter(obs_datetime >=ymd(dateRange[1]), obs_datetime<=ymd(dateRange[2]))
      }else{#Concept Date
        obs <- filterObsByDateConcept(pool, obs, conceptDates)
      }
      patIdentifiers <- getPatientIdentifiers(pool)
      personDemographics <- getPersonDemographics(pool)
      
      obs <- obs %>% 
        inner_join(personDemographics,by=c("person_id"="person_id")) %>% 
        inner_join(patIdentifiers, by=c("person_id"="patient_id")) %>% 
        mutate(person_id = identifier,
               obs_datetime = ymd(paste(year(obs_datetime),"-",
                                        month(obs_datetime),"-",
                                        day(obs_datetime)
               )
               ),
               value_datetime = ymd(paste(year(value_datetime),"-",
                                          month(value_datetime),"-",
                                          day(value_datetime)
               )
               )
        ) %>% 
        select(-identifier) %>% 
        rename(Patient.Identifier = person_id,
               Obs.Date = obs_datetime,
               Question = concept_name,
               ID = obs_id,
               Location = location_id,
               GroupID = obs_group_id)
      
      obs <- obs %>% 
        gather(Key,Value, starts_with("value_")) %>% 
        filter(!is.na(Value)) %>% 
        select(-Key) %>% 
        rename(Answer = Value, Comments = comments) %>% 
        select(ID, Patient.Identifier, 
               Gender, Age, Question, 
               Obs.Date, Location, 
               Answer, Comments, GroupID)
      cat("number of obs: ")
      cat(nrow(obs))
      obs_dt <- data.frame()
      if(nrow(obs) > 0){
        cols_bf <- names(obs)
  
        obs <- obs %>% spread(Question, Answer)
        names(obs) <- make.names(names(obs))
        cols_af <- names(obs)
    
        obs <- obs %>% replace(is.na(obs),"")
        #Check if any of the newly added columns via spread are completely numeric which can be
        #converted to as.numeric
        df_newly_added_cols <- obs[cols_af[!(cols_af %in% cols_bf)]]
        cols_with_no_na <- as.data.frame(df_newly_added_cols %>% map(~ as.numeric(as.character(.)))) %>% 
          map_lgl(~sum(is.na(.)) ==0)
        col_n <- names(cols_with_no_na)[cols_with_no_na]
        cat("\nColumn Names:\n")
        cat(names(col_n))
  
        obs_dt <- obs
    
        initial_group <- c("GroupID", "Patient.Identifier", "Gender", "Age", "Obs.Date","Location")
        dots <- sapply(initial_group, . %>% {as.formula(paste0('~', .))})
        if(length(listBy) == 1){
          add_dots <- sapply(cols_af[!(cols_af %in% cols_bf)], . %>% {as.formula(paste0('~', .))})
          group_list <- append(dots, add_dots)
        } else{
          group_list <- dots
        }
   
        obs_dt <- obs_dt %>% group_by_(.dots = group_list) %>% summarise_all(funs(paste0(., collapse=" ")))
        
        obs_dt[, sapply(obs_dt, is.character)] <-
          sapply(obs_dt[, sapply(obs_dt, is.character)],
                 str_trim)
        
        if(length(col_n) > 0)
          obs_dt <- obs_dt %>% mutate_each_(funs(as.numeric(as.character(.))), col_n)
      }
      main_table$data <- obs_dt
      output$obsDT <- DT::renderDataTable(main_table$data, 
                                          options = list(paging=T), 
                                          rownames=F, 
                                          filter = "top")
  })

  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('observations', "_",year(ymd_hms(Sys.time())),
            "_",month(ymd_hms(Sys.time())),
            "_",day(ymd_hms(Sys.time())),
            "_",hour(ymd_hms(Sys.time())),
            "_",minute(ymd_hms(Sys.time())),
            "_",second(ymd_hms(Sys.time())),
            ".csv",sep='') 
    },
    content = function(file) {
      write.csv(main_table$data, file)
    }
  )
  observeEvent(input$inApply, {
    updateCheckboxGroupInput(session, 
                             "inColumnNames",
                             choices = names(main_table$data),
                             selected = names(main_table$data)
                             )
    if(length(names(main_table$data)) > 0){
      updateCheckboxInput(session,
                          "incheckbox",
                          value = T)
    }
    else{
      updateCheckboxInput(session,
                          "incheckbox",
                          value = F)
    }
    updateNumericInput(session, "inStartRange",value = "")
    updateNumericInput(session, "inEndRange",value = "")
    updateSelectizeInput(session, "inCatLevels", choices = c(""))
    updateTextInput(session,"inLevelName",value ="")
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
  #Add Categorical Columns
  observeEvent(input$inCollapseAddCols,{
    if(input$inCollapseAddCols == "Add Columns"){
      numeric_columns <- names(main_table$data)[main_table$data %>% 
                                             map_lgl(is.numeric)]
      updateSelectizeInput(session,"inNumericCols",choices = numeric_columns,selected = NULL)
    }
  })
  catColumns <- reactiveValues(data = list())
  #Addlevel to a column
  observeEvent(input$inAddLevel,{
    newlevel <- list("Name" = input$inLevelName,
                    "Range" = c("From"= as.numeric(input$inStartRange),"To"= as.numeric(input$inEndRange))
                    )
    catColumns$data[[length(catColumns$data) + 1]] = newlevel
    levelnames <- catColumns$data %>% map_chr("Name")
    updateSelectizeInput(session, "inCatLevels", choices = levelnames)
    updateNumericInput(session, "inStartRange",value = "")
    updateNumericInput(session, "inEndRange",value = "")
    updateTextInput(session,"inLevelName",value ="")
  })
  #Mutate to add new categorical column to dataframe
  observeEvent(input$inApplyColumn,{
    colNameToBeGrouped <- input$inNumericCols
    newColName <- paste(colNameToBeGrouped,"Group",sep=".")
    mutate_call <- lazyeval::interp(~a , a = as.name(colNameToBeGrouped))
    main_table$data <- main_table$data %>% mutate_(.dots = setNames(list(mutate_call), newColName))

    levelnames <- catColumns$data %>% map_chr("Name")
    ranges <- catColumns$data %>% map("Range")
    df_list <- levelnames %>% map2(.y=ranges, function(x, y, df){
      mutate_call_ip <- lazyeval::interp(~ifelse(a >= y[[1]] & a <=y[[2]],x[[1]],NA) , 
                                         a = as.name(newColName))
      df <- df %>% mutate_(.dots = setNames(list(mutate_call_ip), newColName))
    }, df = main_table$data) %>% map(function(x){
      filter_criteria <- lazyeval::interp(~!is.na(a), a = as.name(newColName))
      x %>% filter_(.dots = filter_criteria)
    })
    main_table$data <- bind_rows(df_list)
    
    mutate_call <- lazyeval::interp(~as.factor(a) , a = as.name(newColName))
    main_table$data <- main_table$data %>% mutate_(.dots = setNames(list(mutate_call), newColName))
    
    output$obsDT <- DT::renderDataTable(main_table$data, 
                                        options = list(paging=T), 
                                        rownames=F, 
                                        filter = "top")
    updateSelectizeInput(session, "inCatLevels", choices = c(""))
    catColumns$data <- list()
  })
}

shinyApp(ui = ui, server = server)