
library(markdown)
library(shiny)
library(htmlTable)

ui <- navbarPage("Navbar!",
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        htmlOutput("summary")
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
    #summary(mtcars)
    op <- matrix(paste("Content", LETTERS[1:16]), 
             ncol=4, byrow = TRUE)
    htmlTable(op,
              header =  paste(c("1st", "2nd",
                                "3rd", "4th"), "header"),
              rnames = paste(c("1st", "2nd",
                               "3rd", "4th"), "row"),
              rgroup = c("Group A",
                         "Group B"),
              n.rgroup = c(2,2),
              cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
              n.cgroup = c(2,2), 
              caption="Basic table with both column spanners (groups) and row groups",
              tfoot="&dagger; A table footer commment")
    
  })

  output$table <- DT::renderDataTable({
    cat(nrow(mtcars))
    DT::datatable(mtcars)
  })
}
shinyApp(ui = ui, server = server)
