#### Log in module ###
con = dbConnect(SQLite(), dbname="shiny.sqlite")
myQuery <- dbSendQuery(con, "SELECT username,password FROM users")
users <- dbFetch(myQuery, n = Inf)
USER <- reactiveValues(Logged = F)
DBI::dbDisconnect(con)

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
    username <- isolate(input$userName)
    password <- isolate(input$password)
    rowNum <- which(users$username == username)
    if(length(rowNum) > 0){
      salt <- users[rowNum,]$password
      if(checkpw(password,salt)){
        USER$Logged <- TRUE
        return ()        
      }
    }
    USER$Logged <- FALSE
    showNotification("Username or Password incorrect!", type = "error")  
  }
})

output$userLogged <- eventReactive(input$Login,{
  USER$Logged    
})

outputOptions(output,"userLogged",suspendWhenHidden=FALSE)