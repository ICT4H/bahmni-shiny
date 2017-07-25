pluginUI <- function(id) {
  ns <- NS(id)
  tagList(
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
      start = Sys.Date() - 30,
      end = Sys.Date()
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
                  column(3,
                    selectInput(ns("inDatatype"), "DataType", choices=c(
                      Numeric = 1,
                      Datetime = 2
                      ), selected = 1
                    )
                  ),
                  column(3,
                    textInput(ns("inGroupName"), "New Column Name", value="")
                  ),
                  conditionalPanel(
                    condition = paste("input[['", id, "-inDatatype']]==1", sep = ""),
                    column(3,
                      checkboxInput(ns("inTwoVariables"), "Using Two Variables", value = F)
                    )
                  )
                ),
                uiForNumericVariables(id, ns),
                uiForDateTimeVariables(id, ns),
                fluidRow(
                  column(2,
                         textInput(ns("inLevelName"), "Name", value =
                                     "")
                  ),
                  column(2,
                         actionButton(
                           ns("inAddLevel"), label = "Add Level", class = 'btnbottomAlign'
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

uiForDateTimeVariables <- function(id, ns){
  conditionalPanel(
    condition = paste("input[['", id, "-inDatatype']]==2", sep = ""),
    fluidRow(
      column(4, selectizeInput(ns("inDateCols"), "DateTime Columns:", choices=c(""))
      ),
      column(3, dateInput(ns("inAfterDate"), "After"))
    )
  )
}

uiForNumericVariables <- function(id, ns){
    conditionalPanel(
    condition = paste("input[['", id, "-inDatatype']]==1", sep = ""),
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
