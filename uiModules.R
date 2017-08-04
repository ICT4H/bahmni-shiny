contentPanelUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id=ns("inTabPanel"),
      tabPanel("Search", searchTabUI(ns("search"))),
      tabPanel("Bar and Charts", barChartTabUI(ns("barChart"))),
      tabPanel("Dashboard")
    )
  )
}

searchTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    p(""),
    fluidRow(
      column(2, selectInput(ns("inSelect"), "",
        c(""), selected = 2)),
      column(4, selectInput(ns("inCheckboxGroup"), "",
        c(""), multiple = T)),
      conditionalPanel(
        condition = paste("input[['", id, "-inSelect']]==2", sep = ""),
        column(4, selectInput(ns("inAnswers"), "",
                              c(""), multiple = T))
      )
    ),
    fluidRow(
      column(4, selectInput(ns("inDateBy"), "Date Filter",
        c(""))
      ),
      column(4,
        dateRangeInput(ns("inDateRange"),
          label = 'Range',
          start = Sys.Date() - 30,
          end = Sys.Date()
        )
      )
    ),
    actionButton(ns("inApply"), "Apply"),
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
        value = TRUE
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
              bsCollapsePanel(
                "Add Columns",
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
                                        "")),
                  column(2,
                         textInput(ns("inLevelName"), "Name", value =
                                     "")),
                  column(1,
                         actionButton(
                           ns("inAddLevel"), label = "+", class = 'btnbottomAlign'
                         ))
                ),
                fluidRow(column(
                  3,
                  selectizeInput(ns("inCatLevels"), "Levels:", choices =
                                   c(""))
                ),
                column(
                  3,
                  actionButton(ns("inApplyColumn"), "Apply", class = 'btnbottomAlign')
                )),
                style = "info"
              )
            )
          ),
          DT::dataTableOutput(ns("obsDT"))
          ,
          downloadButton(ns("downloadData"), 'Download')
        )
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
              checkboxGroupInput(ns("inDimensions"), 'Columns:',
                                 choices = c("")),
              selectInput(
                ns("inCharts"),
                'Charts:',
                choices = c("Table", "Bar Chart", "Histogram", "Scatter Plot", "Map Plot", "Line Chart"),
                multiple = F
              ),
              checkboxInput(ns("inProportional"), "Proportional", value = F),
              selectInput(ns("inTimeInterval"), "Time Interval"
                ,choices = c("Years","Months"),
                selected = 1,
                multiple = F),
              actionButton(ns("inShow"), "Show")
            ),
            # Show a plot of the generated distribution
            mainPanel(navbarPage(
              "",
              id = ns("inChartMenu"),
              navbarMenu(
                "Output",
                tabPanel("Table",
                         tableOutput(ns("tableDF"))),
                tabPanel("Bar Chart",
                        fluidRow(column(
                           12,
                           checkboxInput(ns("inFlip"), "Flip:", value = F)
                         )),
                         fluidRow(column(
                           12,
                           plotlyOutput(ns("barPlot"))
                         ))),
                tabPanel("Histogram",
                         fluidRow(column(
                           12,
                           sliderInput(ns("inHistInput"), "Choose a bin width:", 1, 100, 10)
                         )),
                         fluidRow(column(
                           12,
                           plotOutput(ns("histPlot"))
                         ))),
                tabPanel("Scatter Plot",
                         fluidRow(column(
                           12,
                           plotOutput(
                             ns("scatterPlot"),
                             dblclick = ns("scatter_dblclick"),
                             brush = brushOpts(id = ns("scatter_brush"),
                                               resetOnNew = TRUE)
                           )
                         ))),
                tabPanel("Map Plot", 
                          fluidRow(column(
                           12,
                           leafletOutput(ns("mapPlot"))
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
                         plotlyOutput(ns("lineChart"))
                       )))
                )
            ),
            uiOutput(ns("tableDownload")))
          ))
}