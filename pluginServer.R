plugin <- function(input, output, session, dataSourceFile, pluginName){
  mainTable <- reactiveValues(data = NULL)
  mainPlot <- reactiveValues(data = NULL)
  tableData <- reactiveValues(data = NULL)
  
  callModule(pluginSearchTab, "search", mainTable, dataSourceFile, pluginName)
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

pluginSearchTab <- function(input, output, session, mainTable, dataSourceFile, pluginName) {
  existingColumnDefs <- reactiveValues(data = NULL)
  colDefFileName <- paste("derivedColumns/",pluginName,".json",sep="")
  if(file.exists(colDefFileName)){
    existingColumnDefs$data <- fromJSON(file=colDefFileName) 
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
    shouldFetchAll <- input$inFetchAll
    dateRange <- as.character(input$inDateRange)
    envir <- new.env()
    mysqlPool <- getMysqlConnectionPool()
    psqlPool <- getPsqlConnectionPool()
    source(dataSourceFile,local=envir)
    showModal(modalDialog(
      withSpinner(p("")),
      title = "Fetching Data",
      footer = NULL, size = "s"))
    mainTable$data <- envir$fetchData(mysqlPool, psqlPool, shouldFetchAll, ymd(dateRange[1]), ymd(as.Date(dateRange[2])+1))
    removeModal()
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
      showModal(modalDialog(
        "There is no data available for selected data range!"
      ))
      updateCheckboxInput(session,
                          "incheckbox",
                          value = F)
    }
    updateNumericInput(session, "inStartRange", value = "")
    updateNumericInput(session, "inEndRange", value = "")
    updateNumericInput(session, "inStartRangeOther", value = "")
    updateNumericInput(session, "inEndRangeOther", value = "")
    updateTextInput(session, "inCategoryName", value = "")
    output$newColumnCategories <- renderTable(do.call("rbind", list()))
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
      
      updateSelectizeInput(session,
                           "inNumericCols",
                           choices = numericColumns,
                           selected = NULL)
      updateSelectizeInput(session,
                           "inNumericColsOther",
                           choices = numericColumns,
                           selected = NULL)
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
    categoryNames <- catColumns$data %>% map_chr("Name")
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
    
    write_lines(toJSON(existingColumnDefs$data), colDefFileName)
    output$newColumnCategories <- renderTable(do.call("rbind", list()))
    catColumns$data <- list()
    updateTextInput(session,"inDerivedColumnName",value = "")
    updateCheckboxInput(session,"inTwoVariables", value = F)
    updateSelectizeInput(session, "inNumericCols")
    updateSelectizeInput(session, "inNumericColsOther")
  })

  observeEvent(input$inColumnDefs, {
    columnName <- input$inColumnDefs
    req(input$inColumnDefs)
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
  })
}

deriveWithOneVarNumeric <- function(colDef, columnName, mainTable) {
  firstColName <- colDef$firstColName
  ranges <- colDef$categories %>% map("Range")
  categoryNames <- colDef$categories %>% map_chr("Name")

  mutate_call <- lazyeval::interp( ~ a , a = as.name(firstColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), firstColName))

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
  
  mutate_call <- lazyeval::interp( ~ a , a = as.name(firstColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), firstColName))

  mutate_call <- lazyeval::interp( ~ a , a = as.name(secondColName))
  mainTable$data <-
    mainTable$data %>% mutate_(.dots = setNames(list(mutate_call), secondColName))

  categoryNames %>% map2(.y = ranges, function(x, y, df) {
    mutate_call_ip <- lazyeval::interp( ~ ifelse(a >= y[[1]] & a <= y[[2]] & b >= y[[3]] & b <= y[[4]] , x[[1]], NA) ,
                        a = as.name(firstColName), b = as.name(secondColName))
    df <-
      df %>% mutate_(.dots = setNames(list(mutate_call_ip), columnName))
  }, df = mainTable$data)
}

barChartTab <- function(input, output, session, mainTable, tableData, mainPlot) {
  scatter_ranges <- reactiveValues(x = NULL, y = NULL)
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
    if(identical(input$inFactor1, "")){
      showModal(modalDialog(
        "Please select Factor 1!"
      ))
      return()
    }
    grp_cols <- c(input$inFactor1)
    if(!identical(input$inFactor2, "")){
      grp_cols <- c(grp_cols, input$inFactor2)  
    }
    
    obs <- mainTable$data
    dots <- lapply(grp_cols, as.symbol)
    if (chartOption == 1) {
      tableop <- ftable(droplevels(obs[grp_cols]))
      if(input$inProportional){
        tableop <- prop.table(tableop)
      }
      tableData$data <- tableop
      output$tableDF <- renderTable(as.matrix(tableop), rownames = T)
      selectedValue <- "Table"
    } else if (chartOption == 2) {
      showBarChart(input,output, grp_cols, obs)
      selectedValue <- "Bar Chart"
    } else if (chartOption == 3) {
      #histogram
      output$histPlot <- renderPlot({
        hist_1 <- obs %>% ggplot(aes_string(grp_cols[1]))
        mainPlot$data <-
          hist_1 +  geom_histogram(binwidth = input$inHistInput)
        mainPlot$data
      })
      selectedValue <- "Histogram"
    } else if (chartOption == 4) {
      #scatter plot
      output$scatterPlot <- renderPlot({
        scatter_plot <-
          obs %>% ggplot(aes_string(
            x = grp_cols[1],
            y = grp_cols[2],
            col = "Gender"
          ))
        scatter_plot <- scatter_plot + geom_point()
        mainPlot$data <-
          scatter_plot + coord_cartesian(xlim = scatter_ranges$x, ylim = scatter_ranges$y)
        mainPlot$data
      })
      selectedValue <- "Scatter Plot"
    } else if(chartOption == 5){
      showMapPlot(input,output,grp_cols,obs)
      selectedValue <- "Map Plot"
    } else if(chartOption == 6){
      showLineChart(input,output,grp_cols,obs)
      selectedValue <- "Line Chart"
    } else if(chartOption == 7){
      showBoxPlot(input,output,grp_cols,obs)
      selectedValue <- "Box Plot"
    }
    updateNavbarPage(session, "inChartMenu", selected = selectedValue)
    ns <- session$ns
    output$tableDownload <- renderUI({
      tagList(
        downloadButton(ns("downloadTable"), 'Download'),
        actionButton(ns("inAddtoDB"), "Add to Dashboard")
      )
    })
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
        op_df <- stats:::format.ftable(tableData$data, quote = FALSE)
        write.csv(op_df, file)
      } else{
        ggsave(file, plot = mainPlot$data, device = "png")
      }
    }
  )
}

showBoxPlot <- function(input,output,grp_cols,obs){
  interval <- input$inTimeInterval
  if(interval == "Years"){
    obs[interval] <- strftime(obs[["Visit Date"]], format="%Y")  
  }else if(input$inTimeInterval == "Months"){
    obs[interval] <- strftime(obs[["Visit Date"]], format="%m-%Y")  
  }
  output$boxPlot <- renderPlotly({
    p <- plot_ly(obs,x = obs[[interval]], y = obs[[grp_cols[[2]]]]
      , color = obs[[grp_cols[[1]]]], type = "box")
    p %>% layout(boxmode = "group", xaxis = list(title = interval,showgrid = T))
  })
}

showLineChart <- function(input,output,grp_cols,obs){
  if(length(grp_cols) > 1){
    showNotification(
      "Line Chart works for just one Factor, We will consider Factor 1!",
      type = "warning",
      duration = NULL
    )
  }
  interval <- input$inTimeInterval
  if(interval == "Years"){
    obs[interval] <- strftime(obs[["Visit Date"]], format="%Y")  
  }else if(input$inTimeInterval == "Months"){
    obs[interval] <- strftime(obs[["Visit Date"]], format="%m-%Y")  
  }
  
  chartData <- obs %>% group_by_(.dots = c(lapply(grp_cols[1],as.name), interval)) %>% summarise(total = n())
  prapotionalChartData <- chartData %>%
      group_by_(.dots = c(interval)) %>%
      mutate(countT= sum(total)) %>%
      group_by_(.dots = c(lapply(grp_cols[1],as.name))) %>%
      mutate(percentage=round(100*total/countT,2))
  output$lineChart <- renderPlotly({
    if(input$inProportional){
      chartData <- prapotionalChartData
      outputVar <- "percentage"
    }else{
      outputVar <- "total"
    }
    plot <- ggplot(chartData, aes_string(y = outputVar, x = interval, colour = as.name(grp_cols[1]), group = as.name(grp_cols[1])))
    plot <- plot + geom_line(data = chartData, stat="identity", size = 1.5) + geom_point()
    if(input$inFunction != "none"){
      plot <- plot + stat_summary(fun.y = input$inFunction, na.rm = TRUE, group = 3, color = 'black', geom ='line')
    }
    ggplotly(plot, tooltip = c("x", "group", "y"))
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

showMapPlot <- function(input,output,grp_cols,obs){
  if(!identical(input$grp_cols[1], "State") || !identical(input$grp_cols[1], "District")){
    showModal(modalDialog(
      "Map plot can only work with State or District!"
    ))
    return()
  }
  if(length(grp_cols) > 1){
    showNotification(
      "Map Plot works for just one Factor, We will consider Factor 1!",
      type = "warning",
      duration = NULL
    )
  }
  chartData <- obs %>% group_by_(.dots = c(as.name(grp_cols[1]))) %>% summarise(total = n())
  chartData <- subset(chartData, !is.na(chartData[[grp_cols[1]]])) 
  locs_geo <- fetchGeoCode(chartData[[grp_cols[1]]])
  chartData <- cbind(chartData, locs_geo)
  maxRow <- chartData[which.max(chartData$total), ]

  output$mapPlot <- renderLeaflet({
    leaflet(maxRow, data = chartData) %>%
    setView(maxRow$lon ,maxRow$lat, zoom = 9) %>%
    addTiles() %>%
          addCircleMarkers(~lon, ~lat,
           popup = ~as.character(chartData[[grp_cols[1]]]),
           label = ~as.character(total),
           labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
             radius = 20
          )
    })
}

showBarChart <- function(input,output,grp_cols,obs){
  output$barPlot <- renderPlotly({
    interval <- input$inTimeInterval
    if(interval == "Years"){
      obs[interval] <- strftime(obs[["Visit Date"]], format="%Y")  
    }else if(input$inTimeInterval == "Months"){
      obs[interval] <- strftime(obs[["Visit Date"]], format="%m-%Y")  
    }
    chartData <- obs %>% group_by_(.dots = c(lapply(grp_cols,as.name), interval)) %>% summarise(total = n())
    prapotionalChartData <- chartData %>%
      group_by_(.dots = c(interval)) %>%
      mutate(countT= sum(total)) %>%
      group_by_(.dots = c(lapply(grp_cols,as.name))) %>%
      mutate(percentage=round(100*total/countT,2))
    if(length(grp_cols) == 2){
      if(input$inFlipBar){
        grp_cols = rev(grp_cols)
      }
      if(input$inProportional){
        plot <- ggplot(prapotionalChartData, aes_string(as.name(grp_cols[2]), "percentage", fill = as.name(grp_cols[1]))) + 
          geom_bar(stat="identity", position = position_stack(vjust = 0.5), width=0.4) +
          geom_text(data=prapotionalChartData, aes (label = paste(percentage,"%",sep="")), size = 3, position = position_stack(vjust = 0.5)) +
          scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) + 
          facet_grid(as.formula(paste("~", interval))) 
      }else{
        prapotionalChartData$group <- prapotionalChartData[[interval]]
        plot <- ggplot(chartData, aes_string(as.name(grp_cols[2]), "total", fill = as.name(grp_cols[1]))) +
          geom_bar(stat="identity", position = position_stack(vjust = 0.5), width=0.4) +
          geom_text(data=chartData, aes (label = total), size = 3, position = position_stack(vjust = 0.5)) +
          facet_grid(as.formula(paste("~", interval))) 
      }
    }
    else{
      if(input$inFlipBar){
        barType <- position_stack(vjust = 0.5)
      }else{
        barType <- position_dodge(width = 0.2)
      }
      if(input$inProportional){         
        plot <- ggplot(prapotionalChartData, aes_string(interval, "percentage", fill = as.name(grp_cols[1]))) +
          geom_bar(stat="identity", position = barType, width=0.4) +
          geom_text(data=prapotionalChartData, aes (label = paste(percentage,"%",sep="")), size = 3, position = barType) +
          scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))  
      }else{
        plot <- ggplot(chartData, aes_string(interval, "total", fill = as.name(grp_cols[1]))) +
          geom_bar(stat="identity", position = barType, width=0.4) +
          geom_text(data=chartData, aes (label = total), size = 3, position = barType) 
      }
    } 
    
    p <-ggplotly(plot)
    for (i in 1:length(p$x$data)){
        p$x$data[[i]]$hovertext <- NULL
    }
    p
  })
}
