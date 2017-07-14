sideBarUI <- function(id) {
  ns <- NS(id)
  div(
    id = ns("inSaveSideBar"),
    sidebarPanel(
      width = 3,
      actionButton(ns("inSave"), "Save"),
      p(""),
      checkboxGroupInput(ns('inDocuments'), 'Files:',
                         choices = c(""))
    )
  )
}

observationTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    p(""),
    fluidRow(
      column(2, selectInput(ns("inSelect"), "",
                            c(""), selected = 2))
      ,
      column(4, selectInput(
        ns("inCheckboxGroup"), "",
        c(""), multiple = T
      ))
      ,
      conditionalPanel(
        condition = paste("input[['", id, "-inSelect']]==2", sep = ""),
        column(4, selectInput(ns("inAnswers"), "",
                              c(""), multiple = T))
      )
    ),
    fluidRow(column(4, selectInput(
      ns("inDateBy"), "Date Filter",
      c("")
    ))
    ,
    column(
      4,
      dateRangeInput(
        ns("inDateRange"),
        label = 'Range',
        start = Sys.Date() - 30,
        end = Sys.Date()
      )
    )),
    actionButton(ns("inApply"), "Apply"),
    #what is this part doing?
    conditionalPanel(condition = "1==0",
                     checkboxInput(
                       ns("incheckbox"), label = "Choice A", value = F
                     )),
    #what is this part doing?
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
                choices = c("Table", "Bar Chart", "Histogram", "Scatter Plot"),
                multiple = F
              ),
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
                           plotOutput(ns("barPlot"))
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
                         )))
              )
            ),
            uiOutput(ns("tableDownload")))
          ))
}