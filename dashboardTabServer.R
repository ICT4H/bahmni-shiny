dashboardTab <- function(input, output, session, dataSourceFile, plotsForDashboard, dashboardFilePath){
  observers <- list()
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
      i <- 1
      panels <- lapply(dashboardCharts, function(name){
        dateRangeInputID <- paste("inDateRange-",i,sep="")
        applyButtonID <- paste("inApply-",i,sep="")
        plotID <- paste("plot-",i,sep="")

        plot <- plotsForDashboard$data[[name]]
        panel <- panelForDashboardPlot(name,plot,ns,dateRangeInputID,applyButtonID, plotID)
        if(is.null(observers[[applyButtonID]])){
          observer <- observerForDashboardPlots(input, output,plot, dataSourceFile, applyButtonID, dateRangeInputID, plotID)
          observers[[applyButtonID]] <<- observer
        }
        i <<- i + 1
        panel
      })
      do.call(bsCollapse, panels)
    })
  })
}

panelForDashboardPlot <- function(title,plot,ns,dateRangeInputID, applyButtonID, plotID){
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
    if(plot$type == "Map Plot"){
      fluidRow(leafletOutput(ns(plotID)))
    }else{
      fluidRow(plotlyOutput(ns(plotID)))
    }
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

observerForDashboardPlots <- function(input, output,plot,dataSourceFile, applyButtonID, dateRangeInputID, plotID){
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
