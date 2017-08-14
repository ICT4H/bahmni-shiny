#### Log in module ###
PASSWORD <- data.frame(username = "withr", password = "25d55ad283aa400af464c76d713c07ad")
USER <- reactiveValues(Logged = FALSE)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value="")
  )
}

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "User Name:"),
      passwdInput("password", "Pass word:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
   if (input$Login > 0) {
      Username <- isolate(input$userName)
      Password <- isolate(input$password)
      print(Username)
      print(Password)
      Id.username <- which(PASSWORD$username == Username)
      Id.password <- which(PASSWORD$password == Password)
      if (length(Id.username) > 0 & length(Id.password) > 0) {
        if (Id.username == Id.password) {
          USER$Logged <- TRUE
        } 
      } else  {
        "User name or password failed!"
      }
    } 
    }
  }
})
