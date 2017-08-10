plugin <- function(input, output, session, dataSourceFile){
  mainTable <- reactiveValues(data = NULL)
  mainPlot <- reactiveValues(data = NULL)
  tableData <- reactiveValues(data = NULL)
  
  callModule(pluginSearchTab, "search", mainTable, dataSourceFile)
  callModule(barChartTab, "barChart", mainTable, tableData, mainPlot)
  selectChoices <-
    reactiveValues(data = list(
      "Class" = 1,
      "Question" = 2,
      "Answer" = 3
    )
  )
  observeEvent(input$inTabPanel, {
    if (input$inTabPanel == "Bar and Charts") {
      updateCheckboxGroupInput(
        session,
        "barChart-inDimensions",
        choices = names(mainTable$data),
        selected = NULL
      )
      updateSelectInput(
        session,
        "barChart-inCharts",
        choices = list(
          "Table" = 1,
          "Bar Chart" = 2,
          "Histogram" = 3,
          "Scatter Plot" = 4,
          "Map Plot" = 5,
          "Line Chart" = 6,
          "Box Plot" = 7
        )
      )
      
    }
  })
}

pluginSearchTab <- function(input, output, session, mainTable, dataSourceFile) {
  observeEvent(input$inApply, {
    dateRange <- as.character(input$inDateRange)
    envir <- new.env()
    mysqlPool <- getMysqlConnectionPool()
    psqlPool <- getPsqlConnectionPool()
    source(dataSourceFile,local=envir)
    mainTable$data <- envir$fetchData(mysqlPool, psqlPool, ymd(dateRange[1]), ymd(as.Date(dateRange[2])+1))
    disconnectFromDb(mysqlPool)
    disconnectFromDb(psqlPool)
    envir <- NULL
    output$obsDT <- DT::renderDataTable(
      mainTable$data,
      options = list(paging = T),
      rownames = F,
      filter = "top"
    )
  })
  
  observeEvent(input$inApply, {
    updateCheckboxGroupInput(
      session,
      "inColumnNames",
      choices = names(mainTable$data),
      selected = names(mainTable$data)
    )
    if (length(names(mainTable$data)) > 0) {
      updateCheckboxInput(session,
                          "incheckbox",
                          value = T)
    }
    else{
      updateCheckboxInput(session,
                          "incheckbox",
                          value = F)
    }
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateSelectizeInput(session, "inCatLevels", choices = c(""))
    updateTextInput(session, "inLevelName", value = "")
  })
  
  observeEvent(input$inColumnNames, {
    obs <- mainTable$data
    obs <- obs[, input$inColumnNames, drop = F]
    output$obsDT <- DT::renderDataTable(
      obs,
      options = list(paging = T),
      rownames = F,
      filter = "top"
    )
  })
  
  observeEvent(input$inShowColumns, {
    if (input$inShowColumns) {
      shinyjs::show(id = "inColumnNamePanel")
      updateButton(session, "inShowColumns", "Hide Column Selection")
    } else{
      shinyjs::hide(id = "inColumnNamePanel")
      updateButton(session, "inShowColumns", "Show Column Selection")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(
        'observations',
        "_",
        year(ymd_hms(Sys.time())),
        "_",
        month(ymd_hms(Sys.time())),
        "_",
        day(ymd_hms(Sys.time())),
        "_",
        hour(ymd_hms(Sys.time())),
        "_",
        minute(ymd_hms(Sys.time())),
        "_",
        second(ymd_hms(Sys.time())),
        ".csv",
        sep = ''
      )
    },
    content = function(file) {
      write.csv(mainTable$data, file)
    }
  )
  #Add Categorical Columns
  observeEvent(input$inCollapseAddCols, {
    if (!is.null(mainTable$data)) {
      isDate <- function(mydate){
        tryCatch({
            is.Date(as.Date(mydate))},
             error = function(err){FALSE}
        )
      } 
      isNumericColumn <- mainTable$data %>% map_lgl(is.numeric)
      numericColumns <- names(mainTable$data)[isNumericColumn]
      dateColumns <- names(mainTable$data)[mainTable$data %>%
                                                map_lgl(isDate)]
      
      updateSelectizeInput(session,
                           "inNumericCols",
                           choices = numericColumns,
                           selected = NULL)
      updateSelectizeInput(session,
                           "inNumericColsOther",
                           choices = numericColumns,
                           selected = NULL)
      updateSelectizeInput(session,
                           "inDateCols",
                           choices = dateColumns,
                           selected = NULL)
    }
  })
  catColumns <- reactiveValues(data = list())
  #Addlevel to a column
  observeEvent(input$inAddLevel, {
    newlevel <- list(
      "Name" = input$inLevelName,
      "Range" = c(
        "From" = as.numeric(input$inStartRange),
        "To" = as.numeric(input$inEndRange),
        "FromOther" = as.numeric(input$inStartRangeOther),
        "ToOther" = as.numeric(input$inEndRangeOther)
      ),
      "AfterDate" = input$inAfterDate
    )
    catColumns$data[[length(catColumns$data) + 1]] = newlevel
    levelnames <- catColumns$data %>% map_chr("Name")
    updateSelectizeInput(session, "inCatLevels", choices = levelnames)
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateTextInput(session, "inLevelName", value = "")
  })
  
  #Mutate to add new categorical column to dataframe
  observeEvent(input$inApplyColumn, {
    datatype <- input$inDatatype
    columnName  <- input$inGroupName
    levelnames <- catColumns$data %>% map_chr("Name")
    
    if(datatype == 1){
      ranges <- catColumns$data %>% map("Range")
      if(input$inTwoVariables){
        df_list <- deriveWithTwoVarsNumeric(input, mainTable, levelnames, ranges)
      }else{
        df_list <- deriveWithOneVarNumeric(input, mainTable, levelnames, ranges)
      }      
    }else{
      dates <- catColumns$data %>% map("AfterDate")
      df_list <- deriveWithDateVariable(input, mainTable, levelnames, dates)
    }

    df_list <- df_list %>% map(function(x) {
      filter_criteria <-
        lazyeval::interp( ~ !is.na(a), a = as.name(columnName))
      x %>% filter_(.dots = filter_criteria)
    })
    mainTable$data <- bind_rows(df_list)
    
    mutate_call <-
      lazyeval::interp( ~ as.factor(a) , a = as.name(columnName))
    mainTable$data <-
      mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), columnName))
    
    output$obsDT <- DT::renderDataTable(
      mainTable$data,
      options = list(paging = T),
      rownames = F,
      filter = "top"
    )
    updateSelectizeInput(session, "inCatLevels", choices = c(""))
    catColumns$data <- list()
  })
}

deriveWithDateVariable <- function(input, mainTable, levelnames, dates) {
  columnName  <- input$inGroupName
  dateColumn <- input$inDateCols
  mutate_call <- lazyeval::interp( ~ a , a = as.name(dateColumn))
  mainTable$data <- mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), dateColumn))

  levelnames %>% map2(.y = dates, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(as.Date(a) >= y[[1]] & as.Date(a) <= Sys.Date()+1, x[[1]], NA) ,
                        a = as.name(dateColumn))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}

deriveWithOneVarNumeric <- function(input, mainTable, levelnames, ranges) {
  columnName  <- input$inGroupName
  firstColName <- input$inNumericCols
  mutate_call <- lazyeval::interp( ~ a , a = as.name(firstColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), firstColName))

  levelnames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] , x[[1]], NA) ,
                        a = as.name(firstColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}

deriveWithTwoVarsNumeric <- function(input, mainTable, levelnames, ranges) {
  columnName  <- input$inGroupName
  firstColName <- input$inNumericCols
  secondColName <- input$inNumericColsOther
  
  mutate_call <- lazyeval::interp( ~ a , a = as.name(firstColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), firstColName))

  mutate_call <- lazyeval::interp( ~ a , a = as.name(secondColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), secondColName))

  levelnames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] & b >= y[[3]] & b <= y[[4]] , x[[1]], NA) ,
                        a = as.name(firstColName), b = as.name(secondColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}