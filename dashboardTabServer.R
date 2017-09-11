dashboardTab <- function(input, output, session, dataSourceFile, plotsForDashboard, dashboardFilePath){
  #todo:- Defining this counter to avoid conflict of element observers.
  counter <- 1
  dashboardTabData <- reactiveValues(data = NULL)
  if(file.exists(dashboardFilePath)){
    plotsForDashboard$data <- fromJSON(file=dashboardFilePath) 
  }else{
    plotsForDashboard$data <- list()
  }

  observe({
    dashboardCharts <- names(plotsForDashboard$data)
    ns <- session$ns
    output$dashboardPlots <- renderUI({
      if(length(dashboardCharts) < 1){
        output$dashboardPlots <- renderUI({
          h3("There are no plots added to dashboard")
        })
        return ()
      }
      panels <- lapply(dashboardCharts, function(name){
        dateRangeInputID <- paste("inDateRange-",counter,sep="")
        applyButtonID <- paste("inApply-",counter,sep="")
        removeButtonId <- paste("inRemoveFromDB-",counter,sep="")
        plotID <- paste("plot-",counter,sep="")

        plot <- plotsForDashboard$data[[name]]
        panel <- panelForDashboardPlot(name,plot,ns,dateRangeInputID,applyButtonID, plotID, removeButtonId)
        observerForRemoveFromDashboard(input, plotID, removeButtonId, plotsForDashboard, dashboardFilePath)
        observerForDashboardFetchData(input, output,plot, dataSourceFile, applyButtonID, dateRangeInputID, plotID)
        counter <<- counter + 1
        panel
      })
      do.call(bsCollapse, panels)
    })
  })
}

panelForDashboardPlot <- function(title,plot,ns,dateRangeInputID, applyButtonID, plotID, removeButtonId){
  bsCollapsePanel(title, 
    tagList(
      fluidRow(
        column(4, 
          dateRangeInput(ns(dateRangeInputID),
            label = 'Range',
            start = Sys.Date() - 365,
            end = Sys.Date()
          )
        ),
        column(4,
          tags$div(class = "custom-top-spacing",
              tags$p("text")
          ),
          actionButton(ns(applyButtonID), "Apply", class = 'btnbottomAlign btn-primary'))
      ) 
    ),
    (if(plot$type == "Map Plot"){
      fluidRow(leafletOutput(ns(plotID)))
    }else{
      fluidRow(plotlyOutput(ns(plotID)))
    }),
    fluidRow(
      div(class = "hidden-input", textInput(ns(paste("plot-", plotID, sep="")), value = title, label = "Plot Name")),
      actionButton(ns(removeButtonId), "Remove From Dashboard", class = 'btnbottomAlign btn-primary')
    ) 
  )
}

renderPlot <- function(data, plot){
  chartOption <- plot$type
  selected_cols <- c(plot$factor1, plot$factor2)
  if(chartOption == "Bar Chart"){
      showBarChart(data, plot$timeInterval, plot$isProportional, selected_cols)
  }else if(chartOption == "Histogram"){
    showHistogram(data,NULL, selected_cols)
  }else if(chartOption == "Scatter Plot"){
    showScatterPlot(data, selected_cols)
  }else if(chartOption == "Map Plot"){
    showMapPlot(data, selected_cols)
  }else if(chartOption == "Line Chart"){
    showLineChart(data,plot$timeInterval,plot$isProportional,"none",selected_cols)
  }else if(chartOption == "Box Plot"){
    showBoxPlot(data,plot$timeInterval,selected_cols)
  }
}

observerForDashboardFetchData <- function(input, output,plot,dataSourceFile, applyButtonID, dateRangeInputID, plotID){
  observeEvent(input[[applyButtonID]], {
    dateRange <- as.character(input[[dateRangeInputID]])
    data <- fetchDataForPlugin(dateRange, FALSE, dataSourceFile)
    if(plot$type == "Map Plot"){
      output[[plotID]] <- renderLeaflet({
        renderPlot(data, plot)
      })
    }else {
      output[[plotID]] <- renderPlotly({
        plotToShow <- renderPlot(data, plot)
        plotToShow
      })
    }
  })
}

observerForRemoveFromDashboard <- function(input, plotID, removeButtonId, plotsForDashboard, dashboardFilePath){
  observeEvent(input[[removeButtonId]], {
    textBoxId <- paste("plot-", plotID, sep="")
    plotName <- input[[textBoxId]]
    plotsForDashboard$data[[plotName]] <- NULL
    write_lines(toJSON(plotsForDashboard$data), dashboardFilePath)
    showNotification(
      paste(plotName, "is removed from dashboard."),
      type = "message"
    )
  })
}
