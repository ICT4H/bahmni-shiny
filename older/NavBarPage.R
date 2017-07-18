
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
    # obs_tod_pres <- read_csv("observations_fp_md.csv")
    # obs_tod_pres <- obs_tod_pres %>% mutate(Gender = as.factor(ifelse(Gender==F,"F","M")))
    # tableop <- ftable(obs_tod_pres[c("Gender","Delivery.Note..Method.of.Delivery","Delivery.Note..Fetal.Presentation")])
    # 
    # 
    # htmlTable(tableop, rnames = rep(unlist(attr(tableop, "row.vars")[2]), 3), 
    #           rgroup=unlist(attr(tableop, "row.vars")[1]), n.rgroup = c(1,1),
    #           cgroup = unlist(attr(tableop, "col.vars")[[1]]),n.cgroup = c(1,1), caption =  names(attr(tableop, "col.vars")) )
    fasting_sugar <- read_csv("obs_fast_sugar_1.csv")
    fasting_sugar <- fasting_sugar %>% mutate(Gender = as.factor(Gender))
    fast_table <- ftable(Gender  ~ Blood.Sugar.Fasting.Group + Age.Group, data = fasting_sugar)
    htmlTable(fast_table, rnames = unlist(attr(fast_table, "row.vars")[1]), 
              header = rep(unlist(attr(fast_table, "col.vars")[[1]]), 2), 
              cgroup = unlist(attr(fast_table, "col.vars")[[2]]), 
              n.cgroup = c(2,2),caption =  "BloodSugarTable" )
    
  })

  output$table <- DT::renderDataTable({
    cat(nrow(mtcars))
    DT::datatable(mtcars)
  })
}
shinyApp(ui = ui, server = server)
