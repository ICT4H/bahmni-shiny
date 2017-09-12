formatTimeSeries <- function(data, interval){
  if(interval == "Years"){
    data[interval] <- floor_date(ymd_hms(data[["Visit Date"]]), unit = 'year')
    scale_X <- scale_x_datetime(breaks = date_breaks("1 years"), labels = date_format("%Y"))
    uiText <- paste("format.Date(",interval,", '%Y')")
  }else if(interval == "Months"){
    data[interval] <- floor_date(ymd_hms(data[["Visit Date"]]), unit = 'month')
    scale_X <- scale_x_datetime(breaks = date_breaks("1 months"), labels = date_format("%b-%Y"))
    uiText <- paste("format.Date(",interval,", '%b-%Y')")
  }else if(interval == "Quarters"){
    data[interval] <- floor_date(ymd_hms(data[["Visit Date"]]), unit = 'quarter')
    start <- floor_date(min(data[[interval]]), unit = 'year')
    end <- ceiling_date(max(data[[interval]]), unit = 'year')
    scale_X <- scale_x_datetime(breaks = seq(start, end, by="3 month"), labels = date_format('%b-%Y'))
    uiText <- paste("format.Date(",interval,", '%b-%Y')")
  }
  return (list(data, scale_X, uiText))
}

showBarChart <- function(data,interval, isProportional, selected_cols){
  timeSeriesData <- formatTimeSeries(data, interval)    
  data <- timeSeriesData[[1]]
  scale_X <- timeSeriesData[[2]]
  uiText <- timeSeriesData[[3]]

  chartData <- data %>% 
    group_by_(.dots = c(lapply(selected_cols,as.name), interval)) %>%
    summarise(total = n())

  prapotionalChartData <- chartData %>%
    group_by_(.dots = c(interval)) %>%
    mutate(countT= sum(total)) %>%
    group_by_(.dots = c(lapply(selected_cols,as.name))) %>%
    mutate(percentage=round(100*total/countT,2))

  if(isProportional){
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
  plot
}

fetchGeoCode <- function(addresses, geocodesFilePath){
  lat <- c()
  lon <- c()
  if(file.exists(geocodesFilePath)){
    localGeoCodes <- fromJSON(file=geocodesFilePath) 
  }else{
    localGeoCodes <- list()
  }
  for (i in 1:length(addresses)) {
    localGeoCode <- localGeoCodes[[addresses[i]]]
    if(is.null(localGeoCode)){
      print(paste("Fetch From Remote for", addresses[i]))
      geocode <- geocode(paste("India", addresses[i]))
      if(is.na(geocode$lat) || is.na(geocode$lon)){
        showModal(modalDialog(paste("Problem while fetching data for!", addresses[i])))
        return()
      }else{
        lat <- c(lat, geocode$lat)
        lon <- c(lon, geocode$lon)
        localGeoCodes[[addresses[i]]]$lat <- geocode$lat
        localGeoCodes[[addresses[i]]]$lon <- geocode$lon  
      }
      
    }
    else{
      lat <- c(lat, localGeoCode$lat)
      lon <- c(lon, localGeoCode$lon)
    }
  }
  write_lines(toJSON(localGeoCodes),geocodesFilePath)
  data.frame(lat,lon)
}

showMapPlot <- function(data, selected_cols, geocodesFilePath){
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
  chartData <- data %>%
   group_by_(.dots = c(as.name(selected_cols[1]))) %>% 
   summarise(total = n())

  chartData <- subset(chartData, !is.na(chartData[[selected_cols[1]]])) 
  locs_geo <- fetchGeoCode(chartData[[selected_cols[1]]], geocodesFilePath)
  chartData <- cbind(chartData, locs_geo)
  maxRow <- chartData[which.max(chartData$total), ]
  leaflet(maxRow, data = chartData) %>%
    setView(maxRow$lon ,maxRow$lat, zoom = 9) %>%
    addTiles() %>%
    addCircleMarkers(~lon, ~lat,
      popup = ~as.character(chartData[[selected_cols[1]]]),
      label = ~as.character(total),
      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T),
      radius = 20
    )
}

showLineChart <- function(data,interval, isProportional, aggFunc ,selected_cols){
  timeSeriesData <- formatTimeSeries(data, interval)
  data <- timeSeriesData[[1]]
  scale_X <- timeSeriesData[[2]]
  uiText <- timeSeriesData[[3]]
  
  chartData <- data %>% group_by_(.dots = c(lapply(selected_cols,as.name), interval)) %>% summarise(total = n())
  prapotionalChartData <- chartData %>%
      group_by_(.dots = c(interval)) %>%
      mutate(countT= sum(total)) %>%
      group_by_(.dots = c(lapply(selected_cols,as.name))) %>%
      mutate(percentage=round(100*total/countT,2))

  if(isProportional){
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
  if(aggFunc != "none"){
    plot <- plot + stat_summary(fun.y = aggFunc, na.rm = TRUE, group = 3, color = 'black', geom ='line')
  }
  ggplotly(plot, tooltip = c("text","group", "y"))
}

showBoxPlot <- function(data,interval,selected_cols){
  timeSeriesData <- formatTimeSeries(data, interval)    
  data <- timeSeriesData[[1]]
  scale_X <- timeSeriesData[[2]]
  uiText <- timeSeriesData[[3]]

  if(length(selected_cols) == 2){
    p <- ggplot(data, aes_string(x=interval, y=as.name(selected_cols[[1]]), fill=as.name(selected_cols[[2]]), text = uiText))
  }else{
    p <- ggplot(data, aes_string(x=interval, y=as.name(selected_cols[[1]]), text = uiText))
  }
  p <- p + geom_boxplot() + scale_X
  ggplotly(p, tooltip = c("text", "y", "fill")) %>% layout(boxmode = "group")
}

showScatterPlot <- function(data, selected_cols){
  scatter_plot <- data %>% 
      ggplot(
        aes_string(
          x = as.name(selected_cols[1]),
          y = as.name(selected_cols[2]),
          col = "Gender"
        )
      )
  plot <- scatter_plot + geom_point()
  ggplotly(plot)
}

showHistogram <- function(data, binwidth, selected_cols){
  hist_1 <- data %>% ggplot(aes_string(as.name(selected_cols[1])))
  if(length(selected_cols) == 2){
    hist_1 <- data %>% ggplot(aes_string(as.name(selected_cols[1]), fill = as.name(selected_cols[2])))
  }
  plot <- hist_1 +  geom_histogram(binwidth = binwidth)
  ggplotly(plot)
}

