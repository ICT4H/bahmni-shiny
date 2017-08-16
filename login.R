#### Log in module ###
PASSWORD <- data.frame(username = "admin", password = "$2a$12$hF7youFlKdG2BE9rAVPSNu1mPnOcMEUM/x1y7oUv.qbSC0Xx2ckMe", stringsAsFactors=F)
USER <- reactiveValues(Logged = FALSE)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "Username:"),
      passwordInput("password", "Password:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

observeEvent(input$Login, {
  if (USER$Logged == FALSE) {
    Username <- isolate(input$userName)
    Password <- isolate(input$password)
    rowNum <- which(PASSWORD$username == Username)
    if(length(rowNum) > 0){
      salt <- PASSWORD[rowNum,]$password
      if(checkpw(Password,salt)){
        USER$Logged <- TRUE
        return ()        
      }
    }
    USER$Logged <- FALSE
    showNotification("Username or Password incorrect!", type = "error")  
  }
})