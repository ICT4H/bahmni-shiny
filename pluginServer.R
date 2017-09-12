plugin <- function(input, output, session, dataSourceFile, pluginName, preferencesFolderPath){
  source('dashboardTabServer.R', local = TRUE)

  mainTable <- reactiveValues(data = NULL)
  mainPlot <- reactiveValues(data = NULL)
  tableData <- reactiveValues(data = NULL)
  plotsForDashboard <- reactiveValues(data = NULL)
  
  colDefFilePath = paste(preferencesFolderPath,"/",pluginName,"-columns.json",sep="")
  dashboardFilePath = paste(preferencesFolderPath,"/",pluginName,"-dashboard.json",sep="")
  geocodesFilePath = paste(preferencesFolderPath,"/","geocodes.json", sep="")

  existingColumnDefs <- reactiveValues(data = NULL)
  if(file.exists(colDefFilePath)){
    existingColumnDefs$data <- fromJSON(file=colDefFilePath) 
  }else{
    existingColumnDefs$data <- list()
  }

  callModule(pluginSearchTab, "search", mainTable, dataSourceFile, colDefFilePath, existingColumnDefs)
  callModule(barChartTab, "barChart", mainTable, tableData, mainPlot, plotsForDashboard, dashboardFilePath, geocodesFilePath)
  callModule(dashboardTab, "dashboard", dataSourceFile, plotsForDashboard, dashboardFilePath, geocodesFilePath, existingColumnDefs)

  observeEvent(input$inTabPanel, {
    if (input$inTabPanel == "Bar and Charts") {
      updateSelectInput(
        session,
        "barChart-inFactor1",
        choices = c("",names(mainTable$data)),
        selected = NULL
      )
      updateSelectInput(
        session,
        "barChart-inFactor2",
        choices = c("",names(mainTable$data)),
        selected = NULL
      )
    }
  })
}

fetchDataForPlugin <- function(dateRange, shouldFetchAll, dataSourceFile){
  envir <- new.env()
  showModal(modalDialog(
    withSpinner(p("")),title = "Fetching Data",
    footer = NULL, size = "s")
  )
  mysqlPool <- getMysqlConnectionPool()
  psqlPool <- getPsqlConnectionPool()
  source(dataSourceFile,local=envir)
  data <- envir$fetchData(mysqlPool, psqlPool, shouldFetchAll, ymd(dateRange[1]), ymd(as.Date(dateRange[2])+1))
  removeModal()
  disconnectFromDb(mysqlPool)
  disconnectFromDb(psqlPool)
  envir <- NULL
  data
}

pluginSearchTab <- function(input, output, session, mainTable, dataSourceFile, colDefFilePath, existingColumnDefs) {
  observe({
    updateSelectInput(session,
      "inColumnDefs",
      choices = names(existingColumnDefs$data)
    )
  })

  observeEvent(input$inApply, {
    shouldFetchAll <- input$inFetchAll
    dateRange <- as.character(input$inDateRange)
    mainTable$data <- fetchDataForPlugin(dateRange, shouldFetchAll, dataSourceFile)
    output$obsDT <- DT::renderDataTable(
      mainTable$data,
      options = list(paging = T),
      rownames = F,
      filter = "top"
    )

    if (length(names(mainTable$data)) > 0) {
      updateCheckboxInput(session,"incheckbox", value = T)
    }
    else{
      message <- "There is no data available for selected data range!"
      showModal(modalDialog(message))
      updateCheckboxInput(session, "incheckbox", value = F)
    }
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateTextInput(session, "inCategoryName", value = "")
    output$newColumnCategories <- renderTable(do.call("rbind", list()))
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
      isNumericColumn <- mainTable$data %>% map_lgl(is.numeric)
      numericColumns <- names(mainTable$data)[isNumericColumn]
      
      updateSelectizeInput(session,
                           "inNumericCols",
                           choices = numericColumns,
                           selected = 1)
      updateSelectizeInput(session,
                           "inNumericColsOther",
                           choices = numericColumns,
                           selected = 1)
    }
  })
  catColumns <- reactiveValues(data = list())

  convertCategoriesToNamedList <- function(categories, usingTwoVars){
    outputCategories <- lapply(categories, FUN=function(category){
      categoryOutput <- list()
      categoryOutput["Name"] <- category$Name
      if(usingTwoVars){
        categoryOutput["Range For First Variable"] <- paste(category[['Range']][['From']],category[['Range']][['To']], sep=" - ")
        categoryOutput["Range For Second Variable"] <- paste(category[['Range']][['FromOther']],category[['Range']][['ToOther']], sep=" - ")
      }else{
        categoryOutput["Range For Variable"] <- paste(category[['Range']][['From']],category[['Range']][['To']], sep=" - ")
      }
      categoryOutput
    })
  }
  #AddCategory to a column
  observeEvent(input$inAddCategory, {
    if(identical(input$inCategoryName, "")){
      showModal(modalDialog("Please Enter Category Name!"))
      return()
    }
    if(identical(input$inNumericCols,"")){
      showModal(modalDialog("Please select a column!"))
      return()
    }
    if(input$inTwoVariables && identical(input$inNumericColsOther,"")){
      showModal(modalDialog("Please select a column!"))
      return()
    }
    if(is.na(input$inStartRange) || is.na(input$inEndRange)){
      showModal(modalDialog("Please add start and end range!"))
      return()
    }
    if(input$inTwoVariables && (is.na(input$inStartRangeOther) || is.na(input$inEndRangeOther))){
      showModal(modalDialog("Please add start and end range!"))
      return()
    }
    newCategory <- list(
      "Name" = input$inCategoryName,
      "Range" = c(
        "From" = as.numeric(input$inStartRange),
        "To" = as.numeric(input$inEndRange),
        "FromOther" = as.numeric(input$inStartRangeOther),
        "ToOther" = as.numeric(input$inEndRangeOther)
      )
    )
    catColumns$data[[length(catColumns$data) + 1]] = newCategory
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateTextInput(session, "inCategoryName", value = "")

    outputCategories <- convertCategoriesToNamedList(catColumns$data, input$inTwoVariables)
    output$newColumnCategories <- renderTable(
      do.call("rbind", outputCategories), bordered = T, caption = "Categories",
       caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
    )
  })

  observeEvent(input$inSaveColDef, {
    if(identical(input$inDerivedColumnName, "")){
      showModal(modalDialog("Please Enter the New Column Name!"))
      return()
    }
    if(length(catColumns$data) < 1){
      showModal(modalDialog("Please add atleast one category!"))
      return()
    }
    result <- list()
    columnName <- input$inDerivedColumnName
    result$datatype <- input$inDatatype
    result$usingTwoVars <- input$inTwoVariables
    result$firstColName <- input$inNumericCols
    result$secondColName <- input$inNumericColsOther
    result$dateColName <- input$inDateCols
    result$categories <- catColumns$data
    
    existingColumnDefs$data[[columnName]] <- result
    
    write_lines(toJSON(existingColumnDefs$data), colDefFilePath)
    output$newColumnCategories <- renderTable(do.call("rbind", list()))
    catColumns$data <- list()
    updateTextInput(session,"inDerivedColumnName",value = "")
    updateCheckboxInput(session,"inTwoVariables", value = F)
    updateSelectizeInput(session, "inNumericCols", selected = 1)
    updateSelectizeInput(session, "inNumericColsOther", selected =1 )

    showNotification(
      paste("Column Definition for", columnName, "is saved successfully."),
      type = "message"
    )
  })

  observeEvent(input$inDeleteColumn,{
    columnName <- input$inColumnDefs
    existingColumnDefs$data[[columnName]] <- NULL
    write_lines(toJSON(existingColumnDefs$data), colDefFilePath)
    showNotification(
      paste("Column Definition for", columnName, "is deleted successfully."),
      type = "message"
    )
  })

  observeEvent(input$inResetColDef,{
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateTextInput(session, "inCategoryName", value = "")

    catColumns$data <- list()
    updateTextInput(session,"inDerivedColumnName",value = "")
    updateCheckboxInput(session,"inTwoVariables", value = F)
    updateSelectizeInput(session, "inNumericCols", selected = 1)
    updateSelectizeInput(session, "inNumericColsOther", selected = 1)
    output$newColumnCategories <- renderTable(do.call("rbind", list()))
  })

  observeEvent(input$inColumnDefs, {
    columnName <- input$inColumnDefs
    if(identical(input$inColumnDefs, "")){
        output$savedColumnDef <- renderTable(
        as.matrix(list()), rownames = T, colnames = F, bordered = T
      )
      output$savedColumnCategories <- renderTable(
        do.call("rbind", list())
      )
      return ()
    }
    colDef <- existingColumnDefs$data[[columnName]]

    outputColDef <- c()
    outputColDef['Datatype'] <- colDef$datatype
    outputColDef['Uses Two Variables'] <- ifelse(colDef$usingTwoVars, "YES", "NO")
    if(colDef$usingTwoVars){
      outputColDef['First Variable'] <- colDef$firstColName
      outputColDef['Second Variable'] <- colDef$secondColName
    }else{
      outputColDef['Variable'] <- colDef$firstColName
    }
    
    outputCategories <- convertCategoriesToNamedList(colDef$categories, colDef$usingTwoVars)
    output$savedColumnDef <- renderTable(
      as.matrix(outputColDef), rownames = T, colnames = F, bordered = T
    )
    output$savedColumnCategories <- renderTable(
      do.call("rbind", outputCategories), bordered = T, caption = "Categories",
       caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
    )
  })
  
  #Mutate to add new categorical column to dataframe
  observeEvent(input$inApplyColumn, {
    columnName <- input$inColumnDefs
    if(identical(columnName, "")){
      showModal(modalDialog("Please Select a columnName!"))
      return()
    }
    
    mainTable$data <- addDerivedColumn(existingColumnDefs, columnName, mainTable$data)

    output$obsDT <- DT::renderDataTable(
      mainTable$data,
      options = list(paging = T),
      rownames = F,
      filter = "top"
    )
    catColumns$data <- list()
    showNotification(
      paste("Column", columnName, "has been added successfully."),
      type = "message"
    )
  })
}

addDerivedColumn <- function(existingColumnDefs, columnName, data){
  colDef <- existingColumnDefs$data[[columnName]]
  datatype <- colDef$datatype
  usingTwoVars <- colDef$usingTwoVars
  if(usingTwoVars){
    df_list <- deriveWithTwoVarsNumeric(colDef,columnName, data)
  }else{
    df_list <- deriveWithOneVarNumeric(colDef, columnName, data)
  }      
  df_list <- df_list %>% map(function(x) {
    filter_criteria <-
      lazyeval::interp( ~ !is.na(a), a = as.name(columnName))
    x %>% filter_(.dots = filter_criteria)
  })
  data <- bind_rows(df_list)
  
  mutate_call <-
    lazyeval::interp( ~ as.factor(a) , a = as.name(columnName))
  data <-
    data %>% mutate_(.dots = setNames(list(mutate_call), columnName))
  data
}

deriveWithOneVarNumeric <- function(colDef, columnName, data) {
  firstColName <- colDef$firstColName
  ranges <- colDef$categories %>% map("Range")
  categoryNames <- colDef$categories %>% map_chr("Name")

  categoryNames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] , x[[1]], NA) ,
                        a = as.name(firstColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = data)
}

deriveWithTwoVarsNumeric <- function(colDef, columnName, data) {
  firstColName <- colDef$firstColName
  secondColName <- colDef$secondColName
  ranges <- colDef$categories %>% map("Range")
  categoryNames <- colDef$categories %>% map_chr("Name")
  
  categoryNames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] & b >= y[[3]] & b <= y[[4]] , x[[1]], NA) ,
                        a = as.name(firstColName), b = as.name(secondColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = data)
}

renderCustomToolbar <- function(output,session, chartOption){
  ns <- session$ns
  output$customToolBar <- renderUI({   
    if(chartOption == "Table"){
      downloadButton(ns("downloadTable"), 'Download')
    }
    else if(chartOption == "Map Plot"){
      fluidRow( 
        column(4,textInput(ns("inPlotName"), "Unique Title", value="")),
        column(4,
          tags$div(class = "custom-top-spacing",
              tags$p("text")
          ),
          actionButton(ns("inAddtoDB"), "Add to Dashboard", class = 'btnbottomAlign btn-primary'))
      ) 
    }
    else if(chartOption != "Map Plot"){
      tagList(    
        actionButton(ns("inFullScreen"), "View Full Screen", class = 'btnbottomAlign btn-primary'),
        bsModal(ns("plotModal"), "", ns("inFullScreen"), size = "large", plotlyOutput(ns("fullScreenPlot"), height="90vh")),
        fluidRow(
          column(4,textInput(ns("inPlotName"), "Unique Title", value="")),
          column(4,
            tags$div(class = "custom-top-spacing",
              tags$p("text")
            ),
            actionButton(ns("inAddtoDB"), "Add to Dashboard", class = 'btnbottomAlign btn-primary')))
      )
    }
  })
}

barChartTab <- function(input, output, session, mainTable, tableData, mainPlot, plotsForDashboard, dashboardFilePath, geocodesFilePath) {
  #Charts and Graphs
  observeEvent(input$inShow, {
    chartOption <- input$inCharts
    if(identical(input$inFactor1, "")){
      showModal(modalDialog(
        "Please select Factor 1!"
      ))
      return()
    }
    selected_cols <- c(input$inFactor1)
    if(!identical(input$inFactor2, "")){
      selected_cols <- c(selected_cols, input$inFactor2)  
    }
    
    obs <- mainTable$data

    if(chartOption == "Table"){
      tableop <- ftable(droplevels(obs[selected_cols]))
      if(input$inProportional){
        tableop <- prop.table(tableop)
      }
      tableData$data <- tableop
      output$tableDF <- renderTable(as.matrix(tableop), rownames = T)
    }else if(chartOption == "Bar Chart"){
      output$barPlot <- renderPlotly({
        mainPlot$data <- showBarChart(obs, input$inTimeInterval, input$inProportional, selected_cols)
        mainPlot$data
      })
    }else if(chartOption == "Histogram"){
      output$histPlot <- renderPlotly({  
        mainPlot$data <- showHistogram(obs, input$inHistInput, selected_cols)
        mainPlot$data
      })
    }else if(chartOption == "Scatter Plot"){
      output$scatterPlot <- renderPlotly({
        mainPlot$data <- showScatterPlot(obs, selected_cols)
        mainPlot$data
      })
    }else if(chartOption == "Map Plot"){
      output$mapPlot <- renderLeaflet({
        mainPlot$data <- showMapPlot(obs, selected_cols, geocodesFilePath)
        mainPlot$data
      })
    }else if(chartOption == "Line Chart"){
      output$lineChart <- renderPlotly({
        mainPlot$data <- showLineChart(obs,input$inTimeInterval,input$inProportional,input$inFunction,selected_cols)
        mainPlot$data
      })
    }else if(chartOption == "Box Plot"){
      output$boxPlot <- renderPlotly({
        mainPlot$data <- showBoxPlot(obs,input$inTimeInterval,selected_cols)
        mainPlot$data
      })
    }
    renderCustomToolbar(output, session, chartOption)
    updateNavbarPage(session, "inChartMenu", selected = chartOption)
  })

  observeEvent(input$inAddtoDB, {
    if(identical(input$inPlotName, "")){
      showModal(modalDialog("Please Enter a unique title for plot!"))
      return()
    }
    plot <- list()
    plot$factor1 <- input$inFactor1
    if(!identical(input$inFactor2, "")){
      plot$factor2 <- input$inFactor2
    }
    plot$timeInterval <- input$inTimeInterval
    plot$isProportional <- input$inProportional
    plot$type <- input$inCharts

    plotsForDashboard$data[[input$inPlotName]] <- plot
    write_lines(toJSON(plotsForDashboard$data), dashboardFilePath)
    updateTextInput(session, "inPlotName", value = "")
    showNotification(
      paste(input$inPlotName, " is added to dashboard."),
      type = "message"
    )
  })

  observeEvent(input$inFullScreen, {
    if(input$inCharts != "Map Plot"){
      output$fullScreenPlot <- renderPlotly({mainPlot$data})  
    }
  })
  
  #Download Table
  output$downloadTable <- downloadHandler(
    filename = function() {
      chartOption <- input$inCharts
      fextn <- ifelse(chartOption == 1, ".csv", ".png")
      fprefix <- ifelse(chartOption == 1, "table", "plot")
      paste(
        fprefix,
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
        fextn,
        sep = ''
      )
    },
    content = function(file) {
      chartOption <- input$inCharts
      if (chartOption == "Table") {
        op_df <- as.matrix(tableData$data)
        write.csv(op_df, file)
      }
    }
  )
}