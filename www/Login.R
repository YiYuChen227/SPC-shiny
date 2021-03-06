#### Log in module ###

PASSWORD <- data.frame(
  Brukernavn = c("cathy"), 
  Passord = c("0000")
  )

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(width=8,
      textInput("userName", "User Name:"),
      passwordInput("passwd", "Pass word:"),
      br(),
      actionButton("Login", "Log in",style="color: #fff;align:center; background-color: #337ab7; border-color: #2e6da4")
    )
  }
})



output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      column(2,
             "User: ", USER$name
      ),
      column(1, actionButton("logout", "Logout",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
    )
  }  
})

# control login
observeEvent(input$Login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  Id.username <- which(PASSWORD$Brukernavn == Username)
  Id.password <- which(PASSWORD$Passord    == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      USER$Logged <- TRUE
      USER$name <- Username      
    } 
  } else {
    USER$pass <- "User name or password failed!"
  }
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
})

