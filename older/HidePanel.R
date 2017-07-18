library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  pageWithSidebar(headerPanel(""),
            
                  
                      div(id ="inSaveSideBar",sidebarPanel(
                        width = 3,
                        actionButton("inSave","Save"),
                        p(""),
                        checkboxGroupInput('inDocuments', 'Files:',
                                           choices=c(""))
                      )),
                      
                      mainPanel(actionLink("inExplorer","Files"),
                                actionButton("showSidebar", "Show sidebar"),
                                actionButton("hideSidebar", "Hide sidebar")
                      )
             
  )
)

server <-function(input, output, session) {
  observeEvent(input$showSidebar, {
    shinyjs::show(id = "inSaveSideBar")
  })
  observeEvent(input$hideSidebar, {
    shinyjs::hide(id = "inSaveSideBar")
  })
  observeEvent(input$inExplorer, {
    shinyjs::hide(id = "inSaveSideBar")
  })
}

shinyApp(ui, server) 