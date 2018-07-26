white_list <- brapi::ba_db()
sp <- white_list$sweetpotato
con <- sp

observe({
  if(USER$logged){
    
    session$userData$logged <- TRUE
    session$userData$userId <- USER$id
    session$userData$token <- USER$token
    session$userData$username <- USER$username
  
    # print("logging in")
    
    output$fbregistry_login_message <- renderText("")
    
    output$menuUser <- renderMenu({
      sidebarMenu(id ="userSessionMenu",
                  fluidRow(
                    column(width = 2),
                    column(width = 3,
                           div(icon("user-circle", "fa-2x"), style="text-align: right;padding-top: 7px;")
                    ),
                    column(width = 1),
                    column(width = 6,
                           div(uiOutput("userLoggedText"), style="text-align: left;"),
                           div(icon("circle"), "Connected", style="text-align: left;")
                    )),
                  
                  #br(),
                  div(style="padding-left: 90px",actionButton("btLogOut", "Log Out", icon=icon("sign-out"), style='padding:4px;font-size:70%'))
      )
    })
    
    # updateFBRegistryTable()
    fb_registry$table <- data.table(userFilesFromDb(session$userData$userId))
    # fb_registry$table <- userFilesFromDb(session$userData$userId)
    
  }
  
  else{
    USER$id <- NULL
    USER$name <- NULL
    USER$token <- NULL
    USER$username <- NULL
    
    session$userData$logged <- F
    session$userData$userId <- NULL
    session$userData$usename <- NULL
    session$userData$token <- NULL
    session$userData$conn <- NULL
    
    hideTab(inputId = "tabs", target = "Foo")
    
    output$fbregistry_login_message <- renderText(HTML("<center><font color='red'><h2> You must log in to see your files</h2></font></center>"))
    
    output$menuUser <- renderMenu({
      sidebarMenu(id ="networkMenu",
                  fluidRow(
                    column(width = 12,
                           div("Hello, Guest", style="text-align: center;"),
                           div(icon("circle-o"), "Not connected", style="text-align: center;")
                    )),
                  div(style="padding-left: 90px",actionButton("btLogIn", "Log in", icon=icon("sign-in"), style='padding:4px;font-size:70%'))
      )
    })
    fb_registry$table <- setNames(data.table(matrix(nrow = 0, ncol = 10)), col_names)
    # fb_registry$table <- setNames(data.frame(matrix(nrow = 0, ncol = 10)), col_names)
  }
})


observeEvent(input$btLogIn, {
  showModal(loginModal())
})

loginModal <- function(message = ""){
  modalDialog(
    title = HTML("<center><font color='#f7941d'><h2>Log in to SweetPotatoBase</h2></font></center>"),
    div(
      textInput("userName", "Username:", value = USER$id),
      passwordInput("passwd", "Password:"),
      HTML( paste0("<center><h4><font color='red'> ", message, " <font/><h4/><center/>"))
    ),
    a( "Forgot your password?", href="https://sweetpotatobase.org/user/reset_password", target='_blank'),
    br(),
    a( "Not a user yet? Create an account.", href="https://sweetpotatobase.org/user/new", target='_blank'),
    
    easyClose = FALSE,
    footer = tagList(
      div(style="display:inline-block",
          actionButton("checkLogin", "Log in", style="color: #fff;font-size:150%;", class = "btn-primary", width = 150),
          actionButton("closeModal", "Cancel", style=" font-size:150%;", width = 150),
          style="float:right;text-align:center"))
  )
}


observeEvent(input$btLogOut, {
  
  callpath <- "token"
  omc <- con$multicrop
  con$multicrop <- FALSE
  callurl <- paste0(get_brapi(con = con), callpath)
  con$multicrop <- omc
  dat <- list(access_token = USER$token)
  resp <- httr::DELETE(url = callurl,
                       body = dat,
                       encode = ifelse(con$bms == TRUE, "json", "form"))
  
  x2 <- httr::content(x = resp)

  
  if(x2$metadata$status[[3]]$message == "User Logged Out")
      USER$logged <- FALSE
  
})

observeEvent(input$closeModal, {
  removeModal()
})

observeEvent(input$checkLogin, {
  
  mssg = "User or password is incorrect"
  
  con$user<- trimws(input$userName)
  con$password<- trimws(input$passwd)
  
  dat<- data.frame(username = con$user, password = con$password, grant_type = "password", client_id = "", stringsAsFactors = FALSE)
  
  jsondat <- RJSONIO::toJSON(dat)
  callurl <- "https://sweetpotatobase.org/brapi/v1/token"
  resp <- httr::POST(url = callurl,
                     body = dat,
                     encode = ifelse(con$bms == TRUE, "json", "form"))
  
  xout <- httr::content(x = resp)
  token <- xout$access_token
  
  if (USER$logged == FALSE && !is.null(token)) {
        USER$logged <- TRUE
        USER$id <- getUserId(con$user)
        USER$username <- con$user
        USER$name <- xout$userDisplayName
        
        removeModal()
        output$userLoggedText <- renderText(paste("Hello, ",USER$name))
        
  }
  
  if(USER$logged == FALSE){
    showModal(loginModal(mssg));
  }
  
})


get_brapi <- function(con = NULL) {
  if (is.null(con)) return(NULL)
  if (!is.null(con$apipath)) {
    con$apipath <- paste0("/", con$apipath)
  }
  if (con$secure) {
    con$protocol <- "https://"
  }
  port <- ifelse(con$port == 80, "", paste0(":", con$port))
  if (con$multicrop) {
    url <- paste0(con$protocol, con$db, port, con$apipath, "/",
                  con$crop, "/brapi/v1/")
  } else {
    url <- paste0(con$protocol, con$db, port, con$apipath,
                  "/brapi/v1/")
  }
  return(url)
}

getUserId <- function(username){
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  qryUser = dbSendQuery(mydb, paste0("select id from users where username = '", username ,"'"))
  res = fetch(qryUser, n=-1)
  
  ans <- NULL
  
  
  if(nrow(res) == 1) {
    dbDisconnect(mydb)
    return(res["id"]$id)
  }
  
  if(nrow(res) > 1 ) {
    dbDisconnect(mydb)
    return(ans)
  }
  
  ## the user is not in the database yet
  strQry <- paste0("insert into users (username) values('",username,"')")
  qryUser = dbSendQuery(mydb, strQry)
  qryUser = dbSendQuery(mydb, paste0("select id from users where username = '", username ,"'"))
  res = fetch(qryUser, n=-1)
  if(nrow(res) == 1) ans <- res["id"]$id
  
  dbDisconnect(mydb)
  return(ans)
  

}