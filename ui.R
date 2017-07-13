sideBarUI <- function(id) {
	ns <- NS(id)
	div(id =ns("inSaveSideBar"),sidebarPanel(
	  width = 3,
	  actionButton(ns("inSave"),"Save"),
	  p(""),
	  checkboxGroupInput(ns('inDocuments'), 'Files:',
	                     choices=c(""))
	))	
}

observationTabUI <- function(id){
  ns <- NS(id)
	tagList(
         p(""),
         fluidRow(
           column(2,selectInput(ns("inSelect"), "",
                                c(""), selected = 2))
           ,
           column(4,selectInput(ns("inCheckboxGroup"), "",
                                c(""), multiple = T))
           ,
           conditionalPanel(
             condition = paste("input[['",id,"-inSelect']]==2", sep=""),
             column(4,selectInput(ns("inAnswers"), "",
                                  c(""), multiple = T))
           )
         ),
         fluidRow(
           column(4,selectInput("inDateBy", "Date Filter",
                                c("")))
           ,
           column(4,dateRangeInput('inDateRange',
                                   label = 'Range',
                                   start = Sys.Date() -30, end = Sys.Date() 
           ))
         ),
         actionButton("inApply","Apply"),
         #what is this part doing?
         conditionalPanel(
           condition = "1==0",
           checkboxInput("incheckbox", label = "Choice A", value = F)
         ),
         #what is this part doing?
         p(""),
         conditionalPanel(
           condition = "input.incheckbox==1",
           bsButton("inShowColumns", label = "Hide Column Selection",
                    block = F, type = "toggle", value = TRUE),
           p(""),
           sidebarLayout(
             div(id ="inColumnNamePanel",sidebarPanel(
               width = 3,
               checkboxGroupInput('inColumnNames', 'Columns:',
                                  choices=c(""))
             )),
             mainPanel(
               conditionalPanel(
                 condition = "input.incheckbox==1",
                     bsCollapse(id = "inCollapseAddCols",
                                bsCollapsePanel("Add Columns", fluidRow(
                                  column(4,
                                         selectizeInput("inNumericCols","Numeric Columns:", choices=c(""))
                                         ),
                                  column(2,
                                         numericInput("inStartRange","Start", value="")
                                        ),
                                  column(2,
                                         numericInput("inEndRange","End", value="")
                                        ),
                                  column(2,
                                         textInput("inLevelName","Name", value="")
                                        ),
                                  column(1,
                                         actionButton("inAddLevel", label = "+", class = 'btnbottomAlign')
                                        ) 
                                ),
                                fluidRow(
                                  column(3,
                                         selectizeInput("inCatLevels","Levels:", choices=c(""))
                                        ),
                                  column(3,
                                         actionButton("inApplyColumn","Apply", class = 'btnbottomAlign')
                                        )
                                ),style = "info")
                     )
               ),
               DT::dataTableOutput("obsDT")
               ,
               downloadButton('downloadData', 'Download')
             )
           )
         )
		)
}

barChartTabUI <- function() {
	tagList(
	 p(""),
	 pageWithSidebar(
	   
	   # Application title
	   headerPanel(""),
	   
	   # Sidebar with a slider input
	   sidebarPanel(
	     checkboxGroupInput('inDimensions', 'Columns:',
	                        choices=c("")),
	     selectInput('inCharts', 'Charts:',
	                        choices=c("Table", "Bar Chart", "Histogram", "Scatter Plot"), multiple = F),
	     actionButton("inShow","Show")
	   ),
	   # Show a plot of the generated distribution
	   mainPanel(
	     navbarPage("", id = "inChartMenu",
	                navbarMenu("Output",
	                           tabPanel("Table",
	                                    tableOutput("tableDF")
	                           ),
	                           tabPanel("Bar Chart",
	                                    fluidRow(
	                                      column(12,
	                                             plotOutput("barPlot")
	                                      )
	                                    )
	                           ),
	                           tabPanel("Histogram",
	                                    fluidRow(
	                                      column(12,
	                                             sliderInput("inHistInput", "Choose a bin width:", 1, 100, 10)
	                                      )
	                                    ),
	                                    fluidRow(
	                                      column(12,
	                                             plotOutput("histPlot")
	                                      )
	                                    )
	                           ),
	                           tabPanel("Scatter Plot",
	                                    fluidRow(
	                                      column(12,
	                                             plotOutput("scatterPlot",
	                                                        dblclick = "scatter_dblclick",
	                                                        brush = brushOpts(
	                                                          id = "scatter_brush",
	                                                          resetOnNew = TRUE
	                                                        ))
	                                      )
	                                    )
	                           )
	                )
	     ),
	     uiOutput("tableDownload")
	   )
	 )
	)
}