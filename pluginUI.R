pluginUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Welcome!"),
    tabsetPanel(
      id=ns("inTabPanel"),
      tabPanel("Search", pluginSearchTabUI(ns("search"))),
      tabPanel("Bar and Charts", barChartTabUI(ns("barChart"))),
      tabPanel("Dashboard")
    )
  )
}

pluginSearchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    p(""),
    dateRangeInput(ns("inDateRange"),
      label = 'Range',
      start = Sys.Date() - 365,
      end = Sys.Date()
    ),
    actionButton(ns("inApply"), "Apply", class="btn-primary"),
    conditionalPanel(condition = "1==0",
                     checkboxInput(
                       ns("incheckbox"), label = "Choice A", value = F
                     )),
    p(""),
    conditionalPanel(
      condition = paste("input[['", id, "-incheckbox']]==1", sep = ""),
      bsButton(
        ns("inShowColumns"),
        label = "Hide Column Selection",
        block = F,
        type = "toggle",
        value = TRUE,
        class="btn-primary"
      ),
      p(""),
      sidebarLayout(
        div(
          id = ns("inColumnNamePanel"),
          sidebarPanel(width = 3,
                       checkboxGroupInput(
                         ns("inColumnNames"), 'Columns:',
                         choices = c("")
                       ))
        ),
        mainPanel(
          conditionalPanel(
            condition = paste("input[['", id, "-incheckbox']]==1", sep = "") ,
            bsCollapse(
              id = ns("inCollapseAddCols"),
              uiForDerivedColumns(id,ns)
            ),
            DT::dataTableOutput(ns("obsDT"))
            ,
            downloadButton(ns("downloadData"), 'Download', class="btn-primary")
          )
        )
      )
    )
  )
}

uiForDerivedColumns <- function(id, ns){
  bsCollapsePanel(
    "Add Columns",
    tabsetPanel(
      tabPanel("Saved Column Definitions",
        selectInput(ns("inColumnDefs"), "Saved Columns Definitions:", choices =
            c("")),
        
        tableOutput(ns("savedColumnDef")),
        tableOutput(ns("columnLevels")),
        
        actionButton(
          ns("inApplyColumn"), label = "Apply Column", class = 'btnbottomAlign btn-primary'
        )
      ),
      tabPanel("New Column Definition",
        p(""),
        fluidRow(
          column(3,
            selectInput(ns("inDatatype"), "DataType", choices=c(
              "Numeric"
              ), selected = 1
            )
          ),
          column(3,
            textInput(ns("inDerColumnName"), "Column Name", value="")
          ),
          column(3,
            tags$div(class = "custom-top-spacing",
              tags$p("text")
            ),
            checkboxInput(ns("inTwoVariables"), "Using Two Variables", value = F)
          )
        ),
        uiForNumericVariables(id, ns),
        fluidRow(
          column(2,
            textInput(ns("inLevelName"), "Name", value =
              "")
          ),
          column(2,
            tags$div(class = "custom-top-spacing",
              tags$p("text")
            ),
            actionButton(
              ns("inAddLevel"), label = "Add Level", class = 'btnbottomAlign btn-primary'
            ))
        ),
        fluidRow(
          column(3,
            selectizeInput(ns("inCatLevels"), "Levels:", choices =
                           c(""))
          ),
          column(3,
            tags$div(class = "custom-top-spacing",
              tags$p("text")
            ),
            actionButton(ns("inSaveColDef"), "Save Column Definition", class = 'btnbottomAlign btn-primary')
          )
        )
      )
    ),
    style = "info"
  )
}

uiForNumericVariables <- function(id, ns){
  tagList(
    fluidRow(
      column(4,
             selectizeInput(
               ns("inNumericCols"), "Numeric Columns:", choices = c("")
             )),
      column(2,
             numericInput(ns("inStartRange"), "Start", value =
                            "")),
      column(2,
             numericInput(ns("inEndRange"), "End", value =
                            ""))
    ),
    conditionalPanel(
        condition = paste("input[['", id, "-inTwoVariables']]==1", sep = ""),
        fluidRow(
          column(4,
             selectizeInput(
               ns("inNumericColsOther"), "Numeric Columns:", choices = c("")
             )),
          column(2,
             numericInput(ns("inStartRangeOther"), "Start", value =
                            "")),
      column(2,
             numericInput(ns("inEndRangeOther"), "End", value =
                            ""))
          )
    )
  )
}

barChartTabUI <- function(id) {
  ns <- NS(id)
  tagList(p(""),
  pageWithSidebar(
    # Application title
    headerPanel(""),
    
    # Sidebar with a slider input
    sidebarPanel(
      selectInput(ns("inFactor1"), 'Choose Factor 1:',
                         choices = c("")),
      selectInput(ns("inFactor2"), 'Choose Factor 2:',
                         choices = c("")),
      selectInput(
        ns("inCharts"),
        'Charts:',
        choices = c("Table", "Bar Chart", "Histogram",
         "Scatter Plot", "Map Plot", "Line Chart", "Box Plot"),
        multiple = F
      ),
      checkboxInput(ns("inProportional"), "Proportional", value = F),
      selectInput(ns("inTimeInterval"), "Time Interval"
        ,choices = c("Years","Months"),
        selected = 1,
        multiple = F),
      actionButton(ns("inShow"), "Show", class="btn-primary")
    ),
    # Show a plot of the generated distribution
    mainPanel(navbarPage(
      "",
      id = ns("inChartMenu"),
      navbarMenu(
        "Output",
        conditionalPanel(condition="F",tabPanel("None",
                 uiOutput(""))),
        tabPanel("Table",
                 withSpinner(tableOutput(ns("tableDF")))),
        tabPanel("Bar Chart",
                fluidRow(column(
                   12,
                   checkboxInput(ns("inFlipBar"), "Flip:", value = F)
                 )),
                 fluidRow(column(
                   12,
                   withSpinner(plotlyOutput(ns("barPlot")))
                 ))),
        tabPanel("Histogram",
                 fluidRow(column(
                   12,
                   sliderInput(ns("inHistInput"), "Choose a bin width:", 1, 100, 10)
                 )),
                 fluidRow(column(
                   12,
                   withSpinner(plotOutput(ns("histPlot")))
                 ))),
        tabPanel("Scatter Plot",
                 fluidRow(column(
                   12,
                   withSpinner(plotOutput(
                     ns("scatterPlot"),
                     dblclick = ns("scatter_dblclick"),
                     brush = brushOpts(id = ns("scatter_brush"),
                                       resetOnNew = TRUE)
                   ))
                 ))),
        tabPanel("Map Plot", 
                  fluidRow(column(
                   12,
                   withSpinner(leafletOutput(ns("mapPlot")))
                 ))),
        tabPanel("Line Chart",
               fluidRow(column(
                 12,
                 radioButtons(ns("inFunction"), "Function",
                  inline = T,
                  choices = c("none","sum", "mean", "sd", "median"))
               )),
               fluidRow(column(
                 12,
                 withSpinner(plotlyOutput(ns("lineChart")))
               ))),
        tabPanel("Box Plot",
               fluidRow(column(
                 12,
                 withSpinner(plotlyOutput(ns("boxPlot")))
               )))
        )
    ),
    uiOutput(ns("tableDownload")))
  ))
}
