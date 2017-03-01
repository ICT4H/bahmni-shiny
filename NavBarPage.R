
library(markdown)
library(shiny)

ui <- navbarPage("Navbar!",
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        DT::dataTableOutput("table1")#includeMarkdown("about.md")
                                 )
                               )
                      )
           )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(mtcars)
  })
  
  output$summary <- renderPrint({
    summary(mtcars)
  })

  output$table <- DT::renderDataTable({
    cat(nrow(mtcars))
    DT::datatable(mtcars)
  })
}
shinyApp(ui = ui, server = server)

#  conditionalPanel(
#    condition = "input$inCharts=='Table'",
#    tableOutput("tableDF")
#  ),
#  conditionalPanel(
#    condition = "input$inCharts=='Histogram'",
#    plotOutput("histPlot")
#  ),
#  conditionalPanel(
#    condition = "input$inCharts=='Bar Chart'",
#    plotOutput("barPlot")
#  ),
#  conditionalPanel(
#    "input$inCharts=='Scatter Plot'",
#    plotOutput("scatterPlot")
# ),
#  uiOutput("tableDownload")