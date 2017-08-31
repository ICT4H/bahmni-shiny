plugin <- function(input, output, session, dataSourceFile, pluginName, preferencesFolderPath){
  mainTable <- reactiveValues(data = NULL)
  mainPlot <- reactiveValues(data = NULL)
  tableData <- reactiveValues(data = NULL)
  plotsForDashboard <- reactiveValues(data = NULL)
  
  colDefFilePath = paste(preferencesFolderPath,"/",pluginName,"-columns.json",sep="")
  dashboardFilePath = paste(preferencesFolderPath,"/",pluginName,"-dashboard.json",sep="")
  callModule(pluginSearchTab, "search", mainTable, dataSourceFile, colDefFilePath)
  callModule(barChartTab, "barChart", mainTable, tableData, mainPlot)
  callModule(dashboardTab, "dashboard", dataSourceFile, plotsForDashboard, dashboardFilePath)
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

fetchDataForPlugin <- function(input, dataSourceFile){
  shouldFetchAll <- input$inFetchAll
  dateRange <- as.character(input$inDateRange)
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

pluginSearchTab <- function(input, output, session, mainTable, dataSourceFile, colDefFilePath) {
  existingColumnDefs <- reactiveValues(data = NULL)
  if(file.exists(colDefFilePath)){
    existingColumnDefs$data <- fromJSON(file=colDefFilePath) 
  }else{
    existingColumnDefs$data <- list()
  }

  observe({
    updateSelectInput(session,
      "inColumnDefs",
      choices = names(existingColumnDefs$data)
    )
  })

  observeEvent(input$inApply, {
    mainTable$data <- fetchDataForPlugin(input, dataSourceFile)
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
    colDef <- existingColumnDefs$data[[columnName]]
    datatype <- colDef$datatype
    usingTwoVars <- colDef$usingTwoVars
    if(usingTwoVars){
      df_list <- deriveWithTwoVarsNumeric(colDef,columnName, mainTable)
    }else{
      df_list <- deriveWithOneVarNumeric(colDef, columnName, mainTable)
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
    catColumns$data <- list()
    showNotification(
      paste("Column", columnName, "has been added successfully."),
      type = "message"
    )
  })
}

deriveWithOneVarNumeric <- function(colDef, columnName, mainTable) {
  firstColName <- colDef$firstColName
  ranges <- colDef$categories %>% map("Range")
  categoryNames <- colDef$categories %>% map_chr("Name")

  categoryNames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] , x[[1]], NA) ,
                        a = as.name(firstColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}

deriveWithTwoVarsNumeric <- function(colDef, columnName, mainTable) {
  firstColName <- colDef$firstColName
  secondColName <- colDef$secondColName
  ranges <- colDef$categories %>% map("Range")
  categoryNames <- colDef$categories %>% map_chr("Name")
  
  categoryNames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] & b >= y[[3]] & b <= y[[4]] , x[[1]], NA) ,
                        a = as.name(firstColName), b = as.name(secondColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}

barChartTab <- function(input, output, session, mainTable, tableData, mainPlot) {
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
    if (chartOption == 1) {
      showTable(obs, input, output, tableData, selected_cols)
      selectedValue <- "Table"
    } else if (chartOption == 2) {
      showBarChart(input,output, selected_cols, obs, mainPlot)
      selectedValue <- "Bar Chart"
    } else if (chartOption == 3) {
      showHistogram(obs, input, output, mainPlot, selected_cols)
      selectedValue <- "Histogram"
    } else if (chartOption == 4) {
      showScatterPlot(obs, output, selected_cols, mainPlot)
      selectedValue <- "Scatter Plot"
    } else if(chartOption == 5){
      showMapPlot(input,output,selected_cols,obs)
      selectedValue <- "Map Plot"
    } else if(chartOption == 6){
      showLineChart(input,output,selected_cols,obs, mainPlot)
      selectedValue <- "Line Chart"
    } else if(chartOption == 7){
      showBoxPlot(input,output,selected_cols,obs, mainPlot)
      selectedValue <- "Box Plot"
    }
    updateNavbarPage(session, "inChartMenu", selected = selectedValue)
    ns <- session$ns
    output$customToolBar <- renderUI({   
      if(chartOption == 1){
        downloadButton(ns("downloadTable"), 'Download')
      }else if(chartOption != 5){
        tagList(    
           actionButton(ns("inFullScreen"), "View Full Screen"),
           bsModal(ns("plotModal"), "", ns("inFullScreen"), size = "large", plotlyOutput(ns("fullScreenPlot"), height="90vh")),
           actionButton(ns("inAddtoDB"), "Add to Dashboard")   
          )
      }
    })
  })

  observeEvent(input$inFullScreen, {
    if(input$inCharts != 1){
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
      if (chartOption == 1) {
        op_df <- as.matrix(tableData$data)
        write.csv(op_df, file)
      }
    }
  )
}

showTable <- function(obs, input, output, tableData, selected_cols){
  tableop <- ftable(droplevels(obs[selected_cols]))
  if(input$inProportional){
    tableop <- prop.table(tableop)
  }
  tableData$data <- tableop
  output$tableDF <- renderTable(as.matrix(tableop), rownames = T)
}

showHistogram <- function(obs, input, output, mainPlot, selected_cols){
  output$histPlot <- renderPlotly({
    hist_1 <- obs %>% ggplot(aes_string(as.name(selected_cols[1])))
    if(length(selected_cols) == 2){
      hist_1 <- obs %>% ggplot(aes_string(as.name(selected_cols[1]), fill = as.name(selected_cols[2])))
    }
    plot <-
      hist_1 +  geom_histogram(binwidth = input$inHistInput)
    mainPlot$data <- ggplotly(plot)
    mainPlot$data
  })
}

showScatterPlot <- function(obs, output, selected_cols, mainPlot){
  output$scatterPlot <- renderPlotly({
    scatter_plot <-
      obs %>% ggplot(aes_string(
        x = as.name(selected_cols[1]),
        y = as.name(selected_cols[2]),
        col = "Gender"
      ))
    plot <- scatter_plot + geom_point()
    mainPlot$data <- ggplotly(plot)
    mainPlot$data
  })
}

showBoxPlot <- function(input,output,selected_cols,obs, mainPlot){
  interval <- input$inTimeInterval
  timeSeriesData <- formatTimeSeries(obs, interval)    
  obs <- timeSeriesData[[1]]
  scale_X <- timeSeriesData[[2]]
  uiText <- timeSeriesData[[3]]

  if(length(selected_cols) == 2){
    p <- ggplot(obs, aes_string(x=interval, y=as.name(selected_cols[[1]]), fill=as.name(selected_cols[[2]]), text = uiText))
  }else{
    p <- ggplot(obs, aes_string(x=interval, y=as.name(selected_cols[[1]]), text = uiText))
  }
  p <- p + geom_boxplot() + scale_X
  output$boxPlot <- renderPlotly({
    mainPlot$data <- ggplotly(p, tooltip = c("text", "y", "fill")) %>% layout(boxmode = "group")
    mainPlot$data
  })
}

showLineChart <- function(input,output,selected_cols,obs, mapPlot){
  interval <- input$inTimeInterval
  timeSeriesData <- formatTimeSeries(obs, interval)
  obs <- timeSeriesData[[1]]
  scale_X <- timeSeriesData[[2]]
  uiText <- timeSeriesData[[3]]
  
  chartData <- obs %>% group_by_(.dots = c(lapply(selected_cols,as.name), interval)) %>% summarise(total = n())
  prapotionalChartData <- chartData %>%
      group_by_(.dots = c(interval)) %>%
      mutate(countT= sum(total)) %>%
      group_by_(.dots = c(lapply(selected_cols,as.name))) %>%
      mutate(percentage=round(100*total/countT,2))

  output$lineChart <- renderPlotly({
    if(input$inProportional){
      chartData <- prapotionalChartData
      outputVar <- "percentage"
    }else{
      outputVar <- "total"
    }
    plot <- ggplot(chartData, aes_string(y = outputVar, x = interval, colour = as.name(selected_cols[1]), group = as.name(selected_cols[1]), text = uiText))
    plot <- plot + geom_line(data = chartData, stat="identity", size = 1.5) + geom_point() 
    plot <- plot + scale_X
    if(length(selected_cols) == 2){
      facetFactor = paste("`",as.name(selected_cols[2]), "`", "~ .", sep = "")
      plot <- plot + facet_grid(facetFactor)
    }
    if(input$inFunction != "none"){
      plot <- plot + stat_summary(fun.y = input$inFunction, na.rm = TRUE, group = 3, color = 'black', geom ='line')
    }
    mapPlot$data <- ggplotly(plot, tooltip = c("text","group", "y"))
    mapPlot$data
  })
}

fetchGeoCode <- function(addresses){
  lat <- c()
  lon <- c()
  localGeoCodes <- fromJSON(file='geocodes.json')
  for (i in 1:length(addresses)) {
    localGeoCode <- localGeoCodes[[addresses[i]]]
    if(is.null(localGeoCode)){
      print("Fetch From Remote")
      geocode <- geocode(paste("India", addresses[i]))
      lat <- c(lat, geocode$lat)
      lon <- c(lon, geocode$lon)
      localGeoCodes[[addresses[i]]]$lat <- geocode$lat
      localGeoCodes[[addresses[i]]]$lon <- geocode$lon
    }
    else{
      lat <- c(lat, localGeoCode$lat)
      lon <- c(lon, localGeoCode$lon)
    }
  }
  write_lines(toJSON(localGeoCodes),'geocodes.json')
  data.frame(lat,lon)
}

showMapPlot <- function(input,output,selected_cols,obs){
  if(!identical(selected_cols[1], "State") && !identical(selected_cols[1], "District")){
    showModal(modalDialog(
      "Map plot can only work with State or District!"
    ))
    return()
  }
  if(length(selected_cols) > 1){
    showNotification(
      "Map Plot works for just one Factor, We will consider Factor 1!",
      type = "warning",
      duration = NULL
    )
  }
  chartData <- obs %>% group_by_(.dots = c(as.name(selected_cols[1]))) %>% summarise(total = n())
  chartData <- subset(chartData, !is.na(chartData[[selected_cols[1]]])) 
  locs_geo <- fetchGeoCode(chartData[[selected_cols[1]]])
  chartData <- cbind(chartData, locs_geo)
  maxRow <- chartData[which.max(chartData$total), ]

  output$mapPlot <- renderLeaflet({
    leaflet(maxRow, data = chartData) %>%
    setView(maxRow$lon ,maxRow$lat, zoom = 9) %>%
    addTiles() %>%
          addCircleMarkers(~lon, ~lat,
           popup = ~as.character(chartData[[selected_cols[1]]]),
           label = ~as.character(total),
           labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
             radius = 20
          )
    })
}

showBarChart <- function(input,output,selected_cols,obs, mainPlot){
  output$barPlot <- renderPlotly({
    interval <- input$inTimeInterval
    timeSeriesData <- formatTimeSeries(obs, interval)    
    obs <- timeSeriesData[[1]]
    scale_X <- timeSeriesData[[2]]
    uiText <- timeSeriesData[[3]]

    chartData <- obs %>% group_by_(.dots = c(lapply(selected_cols,as.name), interval)) %>% summarise(total = n())
    prapotionalChartData <- chartData %>%
      group_by_(.dots = c(interval)) %>%
      mutate(countT= sum(total)) %>%
      group_by_(.dots = c(lapply(selected_cols,as.name))) %>%
      mutate(percentage=round(100*total/countT,2))

    if(input$inProportional){
      plot <- ggplot(prapotionalChartData, aes_string(interval, "percentage", fill = as.name(selected_cols[1]), text = uiText)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + scale_X
    }else{
      plot <- ggplot(chartData, aes_string(interval, "total", fill = as.name(selected_cols[1]), text = uiText)) +
        geom_bar(stat="identity", position = "dodge") + scale_X
    }
    if(length(selected_cols) == 2){
      facetFactor = paste("`",as.name(selected_cols[2]), "`", "~ .", sep = "")
      plot <- plot + facet_grid(facetFactor)
    }
    
    plot <-ggplotly(plot, tooltip = c("text","fill", "y"))
    for (i in 1:length(plot$x$data)){
        plot$x$data[[i]]$hovertext <- NULL
    }
    mainPlot$data <- plot
    mainPlot$data
  })
}

formatTimeSeries <- function(obs, interval){
  if(interval == "Years"){
    obs[interval] <- floor_date(ymd_hms(obs[["Visit Date"]]), unit = 'year')
    scale_X <- scale_x_datetime(breaks = date_breaks("1 years"), labels = date_format("%Y"))
    uiText <- paste("format.Date(",interval,", '%Y')")
  }else if(interval == "Months"){
    obs[interval] <- floor_date(ymd_hms(obs[["Visit Date"]]), unit = 'month')
    scale_X <- scale_x_datetime(breaks = date_breaks("1 months"), labels = date_format("%b-%Y"))
    uiText <- paste("format.Date(",interval,", '%b-%Y')")
  }else if(interval == "Quarters"){
    obs[interval] <- floor_date(ymd_hms(obs[["Visit Date"]]), unit = 'quarter')
    start <- floor_date(min(obs[[interval]]), unit = 'year')
    end <- ceiling_date(max(obs[[interval]]), unit = 'year')
    scale_X <- scale_x_datetime(breaks = seq(start, end, by="3 month"), labels = date_format('%b-%Y'))
    uiText <- paste("format.Date(",interval,", '%b-%Y')")
  }
  return (list(obs, scale_X, uiText))
}

dashboardTab <- function(input, output, session, dataSourceFile, plotsForDashboard, dashboardFilePath){
  if(file.exists(dashboardFilePath)){
    plotsForDashboard$data <- fromJSON(file=dashboardFilePath) 
  }else{
    plotsForDashboard$data <- list()
    output$dashboardPlots <- renderUI({
      h3("There are no plots added to dashboard")
    })
  }
  observeEvent(input$apply, {
    data <- fetchDataForPlugin(input, dataSourceFile)
    if(nrow(data) <= 0){
      message <- "There is no data available for selected data range!"
      showModal(modalDialog(message))
      return()
    }
    output$dashboardPlots <- renderUI({
      h3("There are no plots added to dashboard")
    })
  })
}