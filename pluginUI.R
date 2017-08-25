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
    checkboxInput(ns("inFetchAll"), label="Fetch All Data", value = F),
    conditionalPanel(
      condition = paste("input[['", id, "-inFetchAll']]==0", sep = ""),
      dateRangeInput(ns("inDateRange"),
      label = 'Range',
      start = Sys.Date() - 365,
      end = Sys.Date()
    )),
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
        tableOutput(ns("savedColumnCategories")),
        
        actionButton(
          ns("inApplyColumn"), label = "Apply Column", class = 'btnbottomAlign btn-primary'
        ),
        actionButton(
          ns("inDeleteColumn"), label = "Delete Column Definition", class = 'btnbottomAlign btn-primary'
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
            textInput(ns("inDerivedColumnName"), "Column Name", value="")
          ),
          column(3,
            tags$div(class = "custom-top-spacing",
              tags$p("text")
            ),
            checkboxInput(ns("inTwoVariables"), "Using Two Variables", value = F)
          )
        ),
        uiForNumericVariables(id, ns),
        tableOutput(ns("newColumnCategories")),
        actionButton(ns("inSaveColDef"), "Save Column Definition", class = 'btnbottomAlign btn-primary'),
        actionButton(ns("inResetColDef"), "Reset", class = 'btnbottomAlign btn-primary')
      )
    ),
    style = "info"
  )
}

uiForNumericVariables <- function(id, ns){
  tags$fieldset(class = "inAddCategory",
    tags$legend("Add Category"),
    textInput(ns("inCategoryName"), "Name", value = ""),
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
    ),
    actionButton(
      ns("inAddCategory"), label = "Add Category", class = 'btnbottomAlign btn-primary'
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
                 withSpinner(tableOutput(ns("tableDF"))),
                 downloadButton(ns("downloadTable"), 'Download')
                 ),
        tabPanel("Bar Chart",
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
                   withSpinner(plotlyOutput(ns("histPlot")))
                 ))),
        tabPanel("Scatter Plot",
                 fluidRow(column(
                   12,
                   withSpinner(plotlyOutput(ns("scatterPlot")))
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
    uiOutput(ns("customToolBar")))
  ))
}
