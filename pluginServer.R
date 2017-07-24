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
  
  dateTimeConcepts <- eventReactive(input$inTabPanel,{
      if(input$inTabPanel=="Search"){
        getDateTimeConcepts()
      }
  })

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
          "Scatter Plot" = 4
        )
      )
      
    }
    else{
      # Can also set the label and select items
      updateSelectInput(
        session,
        "search-inSelect",
        label = "",
        choices = selectChoices$data,
        selected = 2
      )
      updateSelectInput(session,
                        "search-inDateBy",
                        label = "Date Filter",
                        choices = dateTimeConcepts())
      
    }
  })
}

pluginSearchTab <- function(input, output, session, mainTable, dataSourceFile) {
  observeEvent(input$inApply, {
    dateRange <- as.character(input$inDateRange)
    envir <- new.env()
    source(dataSourceFile,local=envir)
    mainTable$data <- envir$fetchData(pool, ymd(dateRange[1]), ymd(as.Date(dateRange[2])+1))
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
    if (input$inCollapseAddCols == "Add Columns") {
      numericColumns <- names(mainTable$data)[mainTable$data %>%
                                                map_lgl(is.numeric)]
      updateSelectizeInput(session,
                           "inNumericCols",
                           choices = numericColumns,
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
        "To" = as.numeric(input$inEndRange)
      )
    )
    catColumns$data[[length(catColumns$data) + 1]] = newlevel
    levelnames <- catColumns$data %>% map_chr("Name")
    updateSelectizeInput(session, "inCatLevels", choices = levelnames)
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateTextInput(session, "inLevelName", value = "")
  })
  
  #Mutate to add new categorical column to dataframe
  observeEvent(input$inApplyColumn, {
    colNameToBeGrouped <- input$inNumericCols
    newColName <- paste(colNameToBeGrouped, "Group", sep = ".")
    mutate_call <-
      lazyeval::interp( ~ a , a = as.name(colNameToBeGrouped))
    mainTable$data <-
      mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), newColName))
    
    levelnames <- catColumns$data %>% map_chr("Name")
    ranges <- catColumns$data %>% map("Range")
    df_list <- levelnames %>% map2(.y = ranges, function(x, y, df) {
      mutate_call_ip <-
        lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]], x[[1]], NA) ,
                          a = as.name(newColName))
      df <-
        df %>% mutate_(.dots = setNames(list(mutate_call_ip), newColName))
    }, df = mainTable$data) %>% map(function(x) {
      filter_criteria <-
        lazyeval::interp( ~ !is.na(a), a = as.name(newColName))
      x %>% filter_(.dots = filter_criteria)
    })
    mainTable$data <- bind_rows(df_list)
    
    mutate_call <-
      lazyeval::interp( ~ as.factor(a) , a = as.name(newColName))
    mainTable$data <-
      mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), newColName))
    
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