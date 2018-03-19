allowedCharacters  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-,()?!*@%&[]{}+=$# "
allowedCharactersPass  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-#@?!%&,*;"
allowedCharactersMail  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-@+"
listCountries <- c('Aruba','Afghanistan','Angola','Anguilla','Albania','Andorra','United Arab Emirates','Argentina','Armenia','American Samoa','Antarctica','French Southern Territories','Antigua and Barbuda','Australia','Austria','Azerbaijan','Burundi','Belgium','Benin','Bonaire','Burkina Faso','Bangladesh','Bulgaria','Bahrain','Bahamas','Bosnia and Herzegowina','Belarus','Belize','Bermuda','Bolivia','Brazil','Barbados','Brunei','Bhutan','Burma','Bouvet Island','Botswana','Byelorusian SSR (Former)','Central African Republic','Canada','Cocos (Keeling) Islands','Switzerland','Chile','China','CIPHQ','Cote dIvoire','Cameroon','Congo','Congo','Cook Islands','Colombia','Comoros','Cape Verde','Costa Rica','Czechoslovakia (Former)','Cuba','Curacao','Christmas Island (Australia)','Cayman Islands','Cyprus','Czech Republic','German Democratic Republic','Germany','Djibouti','Dominica','Denmark','Dominican Republic','Algeria','Ecuador','Egypt','Eritrea','Western Sahara','Spain','Estonia','Ethiopia','Finland','Fiji','Falkland Islands (Malvinas)','France','Faroe Islands','Micronesia','Gabon','United Kingdom','Georgia','Ghana','Gibraltar','Guinea','Guadeloupe','Gambia','Guinea-Bissau','Equatorial Guinea','Greece','Grenada','Greenland','Guatemala','French Guiana','Guam','Guyana','Hong Kong','Heard and Mc Donald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','India','British Indian Ocean Territory','Ireland','Iran','Iraq','Iceland','Israel','Italy','Jamaica','Jordan','Japan','Kazakhstan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Saint Kitts and Nevis','Korea','Kuwait','Lao People s Democratic Republic','Lebanon','Liberia','Libyan Arab Jamahiriya','Saint Lucia','Liechtenstein','Sri Lanka','Lesotho','Lithuania','Luxemburg','Latvia','Macau','Saint Martin (French part)','Macedonia','Morocco','Monaco','Moldova','Madagascar','Maldives','Mexico','Marshall Islands','Mali','Malta','Myanmar','Mongolia','Northern Mariana Islands','Mozambique','Mauritania','Montserrat','Martinique','Mauritius','Malawi','Malaysia','Mayotte','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Niue','Netherlands','Norway','Nepal','Nauru','Neutral Zone (Former)','New Zealand','Oman','Pakistan','Palestine','Panama','Pitcairn Islands','Peru','Philippines','Palau','Papua New Guinea','Poland','Puerto Rico','Korea','Portugal','Paraguay','French Polynesia','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Serbia and Montenegro','Scotland','Sudan','Senegal','Singapore','Saint Helena','Svalbard and Jan Mayen Islands','Solomon Islands','Sierra Leone','El Salvador','San Marino','Somalia','Saint Pierre and Miquelon','Serbia','Sao Tome e Principe','Union of Soviet Socialist Republics (Former)','Surinam','Slovakia','Slovenia','Sweden','Swaziland','Seychelles','Syrian Arab Republic','Turks and Caicos Islands','Chad','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','East Timor','Tonga','Trinidad and Tobago','Tunisia','Turkey','Tuvalu','Taiwan','Tanzania','Uganda','Ukraine','United States Misc. Pacific Islands','unknown','Uruguay','United States of America','Uzbekistan','Vatican City State','Saint Vincent and the Grenadines','Venezuela','British Virgin Islands','Virgin Islands (US)','Viet Nam','Vanuatu','Wallis and Fortuna Islands_','Samoa','Yemen','Yugoslavia (Former)','South Africa','Zaire','Zambia','Zimbabwe'
)

# modal to show when app is launched
showModal(modalDialog(
  title = HTML(" <center>  Welcome to HiDAP-AGROFIMS <center/>"),
  HTML("You can use the open version or log in and use all characteristics of HiDAP-AGROFIMS. <br> Registration is free"),
  easyClose = FALSE,
  footer = tagList(
    modalButton("Continue with Open Version"),
    actionButton("btLoginModal", "Log in or Register")
  )
))

# modal to show to user to login or register
observeEvent(input$btLoginModal, {
  showModal(loginModal())
})


# when user wants to go to welcome modal (modal when app is launched)
observeEvent(input$goBackModal, {
  showModal(modalDialog(
    title = HTML(" <center>  Welcome to HiDAP-AGROFIMS <center/>"),
    HTML("You can use the open version or log in and use all characteristics of HiDAP-AGROFIMS. <br> Registration is free"),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Continue with Open Version"),
      actionButton("btLoginModal", "Log in or Register")
    )
  ))
})

# validate user email
validateEmail <- function(mail){
  res <- c(TRUE, "")
  if (!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",mail, ignore.case=TRUE)){
    res <- c(FALSE, "Not a valid Email")
    return(res)
  }

  mail_split <- strsplit(mail, "")[[1]]
  for (letter in mail_split) {
    if (!grepl(letter, allowedCharactersMail, fixed=TRUE)){
      res <- c(FALSE, "Email contains not valid characters")
      return(res)
    }
  }

  return (res)
}

# to show login modal when called from welcome modal
loginModal <- function(message = ""){
  modalDialog(
    title = HTML("<center> Log in to HiDAP-AGROFIMS <center/>"),
    div(
      textInput("userName", "Username:"),
      passwordInput("passwd", "Password:"),
      HTML( paste0("<center><h4><font color='red'> ", message, " <font/><h4/><center/>"))
    ),

    # actionLink("ForgotPass", "Forgot your password?"), br(),
    # actionLink("CreateAccount", "Not a user yet? Create an account."),
    a( "Forgot your password?", href="#shiny-tab-forgotPass","data-toggle"="tab"),
    br(),
    a( "Not a user yet? Create an account.", href="#shiny-tab-register","data-toggle"="tab"),

    easyClose = FALSE,
    footer = tagList(
      actionButton("goBackModal", "Go back"),
      actionButton("checkLogin", "Log in ")
    )
  )
}

# when the modal is called from sidebar menu
loginModalMenu <- function(message = ""){
  modalDialog(
    title = HTML("<center> Log in to HiDAP-AGROFIMS <center/>"),
    div(
      textInput("userName", "Username:"),
      passwordInput("passwd", "Password:"),
      HTML( paste0("<center><h4><font color='red'> ", message, " <font/><h4/><center/>"))
    ),

    # actionLink("ForgotPass", "Forgot your password?"), br(),
    # actionLink("CreateAccount", "Not a user yet? Create an account."),
    a( "Forgot your password?", href="#shiny-tab-forgotPass","data-toggle"="tab"),
    br(),
    a( "Not a user yet? Create an account.", href="#shiny-tab-register","data-toggle"="tab"),

    easyClose = FALSE,
    footer = tagList(
      modalButton("Close"),
      actionButton("checkLogin", "Log in ")
    )
  )
}

# check credentials when user logs in
observeEvent(input$checkLogin, {
  mssg = "User or password is incorrect"
  val  <- validateEmail(trimws(input$userName))
  inputPass <- trimws(input$passwd)
  if (USER$Logged == FALSE && as.logical(val[1]) && nchar(inputPass) > 0) {
    Username <- isolate(trimws(input$userName))
    Password <- digest(isolate(inputPass), "sha256", serialize = FALSE)

    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    userc = dbSendQuery(mydb, "select id, username, password, fname, lname from users where available = 1")
    data1 = fetch(userc, n=-1)
    dbDisconnect(mydb)
    PASSWORD <- data.frame(Brukernavn = data1[,2], Passord = data1[,3])

    Id.username <- which(PASSWORD$Brukernavn == Username)

    if (length(Id.username) == 1) {
      if (PASSWORD[Id.username, 2] == Password) {
        USER$Logged <- TRUE
        # USER$list <- paste(data1[,4], data1[,5], paste0("<", data1[,2], ">"))
        USER$id <- data1[Id.username, "id"]
        USER$username <- data1[Id.username, "username"]
        USER$fname <- data1[Id.username, "fname"]
        USER$lname <- data1[Id.username, "lname"]
        USER$org <- data1[Id.username, "organization"]
        USER$country <- data1[Id.username, "country"]
        removeModal()
        output$userLoggedText <- renderText(paste("Hello,", USER$fname, sep=" "))
      }
    }
  }

  if(USER$Logged == FALSE){
    showModal(loginModalMenu(mssg));
  }

})


###########################################################################################################
# to perform when a user logs in
###########################################################################################################
observe({
  if(USER$Logged == TRUE) {

    # menu to be shown with hidap network options when the users logs in
    output$menuUser <- renderMenu({
      sidebarMenu(id ="networkMenu",
                  div(img(src="user.png", width = "75px"), style="text-align: center;"),
                  div(uiOutput("userLoggedText"), style="text-align: center;"),
                  br(),
                  menuSubItem("My Profile", tabName = "userProfile", icon = icon("user")),
                  menuSubItem("Change Password", tabName = "changePass", icon = icon("lock")),
                  # menuSubItem("Log Out", tabName = "logoutTab", icon = icon("sign-out"))
                  actionLink("btLogOut", "Log Out", icon=icon("sign-out"))
      )
    })


  }
  else {
    USER$id <- NULL
    USER$username <- NULL
    USER$fname <- NULL
    USER$lname <- NULL
    USER$org <- NULL
    USER$country <- NULL
    output$menuUser <- renderMenu({
      sidebarMenu(id ="networkMenu",
                  # menuItem("Log in", tabName = "login", icon = icon("lock"))
                  actionLink("btLogIn", " Log in", icon=icon("lock"))
      )
    })

  }
})

###########################################################################################################
# creating a new user
###########################################################################################################
observeEvent(input$btCreateUser, {

  # print("hola")
  if (!validateUserCreateForm()) return()

  strMail  <- trimws(input$newUsrMail)
  strPass  <- digest(trimws(input$newUsrPass), "sha256", serialize = FALSE)
  strFName <- trimws(input$newUsrFName)
  strLName <- trimws(input$newUsrLName)
  strOrg   <- trimws(input$newUsrOrg)
  strCounS <- trimws(input$countrySelection)

  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  strQry <- paste("insert into users (username, password, fname, lname, organization, country) values('",strMail,"','",strPass,"','", strFName,"','",strLName,"','",strOrg, "','",strCounS, "')", sep ="")
  qryUpdate = dbSendQuery(mydb, strQry)


  params <- list(
    dataRequest = "createUser",
    username = strMail
  )

  var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/agrofims/createNewUser.php", body=params)
  code <- content(var, "text")
  if (code == "500"){
    strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
    qryDel = dbSendQuery(mydb, strQry)
    showModal(modalDialog(title = "Hidap Network", HTML("Problems creating account, please try again.")))
  }
  else if (code == "200") {
    showModal(modalDialog(title = "Hidap Network", HTML("<h4>Yout account was successfully created, a confirmation message will be sent soon. Check your email to activate your account.</h4> <br> <h5>If you haven't received a message, please check your spam and add us to your contacts.</h5>")))
    output$uiLogin <- renderUI({

      if (USER$Logged == FALSE) {
        wellPanel(
          h3("Start a new session!"),
          textInput("userName", "Username:"),
          passwordInput("passwd", "Password:"),
          br(),
          actionButton("Login", "Log in"),
          br(),
          br(),
          actionLink("ForgotPass", "Forgot your password?"),br(),
          "Not a user yet? ", actionLink("btCreate", "Create a new account.")
        )
      }
    })
  }
  else{
    strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
    qryDel = dbSendQuery(mydb, strQry)
    showModal(modalDialog(title = "Hidap Network", HTML("Problems creating account, please try later.")))
  }
  dbDisconnect(mydb)

})

validateUserCreateForm <- function(){
  mail <- trimws(input$newUsrMail)
  fName <- trimws(input$newUsrFName)
  lName <- trimws(input$newUsrLName)
  org   <- trimws(input$newUsrOrg)
  pass <- trimws(input$newUsrPass)

  validEmail <- validateEmail(mail)

  if(!as.logical(validEmail[1])){
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): INVALID EMAIL ")
    output$pass <- renderText(paste("<font color='red'><h4>", validEmail[2],"</h4></font>", sep = ""))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): ")
  }

  if(usernameIsInDb(mail)){
    output$pass <- renderText("<font color=red><h4>User is already registered</h4></font>")
    return(FALSE)
  }

  lnMail  <- nchar(mail)
  lnPass  <- nchar(pass)
  lnPassR <- nchar(trimws(input$newUsrPassRepeat))
  lnFName <- nchar(fName)
  lnLName <- nchar(lName)
  lnOrg   <- nchar(org)
  lnCounS <- nchar(trimws(input$countrySelection))

  lenghtValid <- lnMail * lnPass * lnPassR * lnFName * lnLName * lnOrg * lnCounS
  passwMatch  <- lnPass == lnPassR && pass == trimws(input$newUsrPassRepeat)

  if(lenghtValid == 0){
    output$pass <- renderText("<font color=red><h4>Must complete all fields with (*)</h4></font>")
    return(FALSE)
  }

  valPass <- validatePassword(pass)

  if(!as.logical(valPass[1])){
    output$pass <- renderText(paste("<font color=red><h4>", valPass[2],"</h4></font>", sep=""))
    return(FALSE)
  }

  if(!passwMatch ){
    output$pass <- renderText("<font color=red><h4>Passwords don't match</h4></font>")
    return(FALSE)
  }

  validFname <- validateInput(fName)
  if(!as.logical(validFname[1])){
    updateTextInput(session, "newUsrFName",
                    label ="* Name: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validFname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrFName",
                    label ="* Name: ")
  }

  validLname <- validateInput(lName)
  if(!as.logical(validLname[1])){
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validLname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: ")
  }

  validOrg <- validateInput(org)
  if(!as.logical(validOrg[1])){
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validOrg[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: ")
  }

  output$pass <- renderText("")
  return(TRUE)

}


observeEvent(input$btLogIn, {
  showModal(loginModalMenu())
})


observeEvent(input$btLogOut, {
  # updateTabItems(session, "tabs", "dashboard")
  USER$Logged <- FALSE
})


validateUserCreateForm <- function(){
  mail <- trimws(input$newUsrMail)
  fName <- trimws(input$newUsrFName)
  lName <- trimws(input$newUsrLName)
  org   <- trimws(input$newUsrOrg)
  pass <- trimws(input$newUsrPass)

  validEmail <- validateEmail(mail)

  if(!as.logical(validEmail[1])){
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): INVALID EMAIL ")
    output$pass <- renderText(paste("<font color=red><h4>", validEmail[2],"</h4></font>", sep = ""))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrMail",
                    label ="* Email Address (username): ")
  }

  if(usernameIsInDb(mail)){
    output$pass <- renderText("<font color=red><h4>User is already registered</h4></font>")
    return(FALSE)
  }

  lnMail  <- nchar(mail)
  lnPass  <- nchar(pass)
  lnPassR <- nchar(trimws(input$newUsrPassRepeat))
  lnFName <- nchar(fName)
  lnLName <- nchar(lName)
  lnOrg   <- nchar(org)
  lnCounS <- nchar(trimws(input$countrySelection))

  lenghtValid <- lnMail * lnPass * lnPassR * lnFName * lnLName * lnOrg * lnCounS
  passwMatch  <- lnPass == lnPassR && pass == trimws(input$newUsrPassRepeat)

  if(lenghtValid == 0){
    output$pass <- renderText("<font color=red><h4>Must complete all fields with (*)</h4></font>")
    return(FALSE)
  }

  valPass <- validatePassword(pass)

  if(!as.logical(valPass[1])){
    output$pass <- renderText(paste("<font color=red><h4>", valPass[2],"</h4></font>", sep=""))
    return(FALSE)
  }

  if(!passwMatch ){
    output$pass <- renderText("<font color=red><h4>Passwords don't match</h4></font>")
    return(FALSE)
  }

  validFname <- validateInput(fName)
  if(!as.logical(validFname[1])){
    updateTextInput(session, "newUsrFName",
                    label ="* Name: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validFname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrFName",
                    label ="* Name: ")
  }

  validLname <- validateInput(lName)
  if(!as.logical(validLname[1])){
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validLname[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrLName",
                    label ="* Lastname: ")
  }

  validOrg <- validateInput(org)
  if(!as.logical(validOrg[1])){
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: INVALID STRING")
    output$pass <- renderText(paste0("<font color=red><h4>", validOrg[2], "</h4></font>"))
    return(FALSE)
  }
  else{
    updateTextInput(session, "newUsrOrg",
                    label ="* Organization: ")
  }

  output$pass <- renderText("")
  return(TRUE)

}

validateChangePassForm <- function(){
  curPass <- trimws(input$chngPassCurrent)
  newPass <- trimws(input$chngPassNew)

  lnCurPass   <- nchar(curPass)
  lnNewPass   <- nchar(newPass)
  lnNewPRep   <- nchar(trimws(input$chngPassNewRep))

  lenghtValid <- lnCurPass * lnNewPass * lnNewPRep
  passwMatch  <- lnNewPass == lnNewPRep && trimws(input$chngPassNew) == trimws(input$chngPassNewRep)

  if(lenghtValid == 0){
    showModal(modalDialog(title = "Hidap Network", HTML("Must complete all fields")))
    return(FALSE)
  }

  validPass <- validatePassword(curPass)
  if(!as.logical(validPass[1])){
    showModal(modalDialog(title = "Hidap Network", HTML(paste("Current Password:", validPass[2]))))
    return(FALSE)
  }

  validPass <- validatePassword(newPass)
  if(!as.logical(validPass[1])){
    showModal(modalDialog(title = "Hidap Network", HTML(paste("New Password:", validPass[2]))))
    return(FALSE)
  }

  if(!passwMatch ){
    showModal(modalDialog(title = "Hidap Network", HTML("New Passwords don't match")))
    return(FALSE)
  }

  return(TRUE)
}


validatePassword <-function(pass){
  lnPass  <- nchar(pass)

  if(lnPass < 8 || lnPass > 12 ){
    res <- c(FALSE,"Your password must contain at least 8 and at most 12 characters" )
    return(res)
  }

  pass_split <- strsplit(pass, "")[[1]]
  # verify that pass contains only allowed characters
  for (letter in pass_split) {
    if (!grepl(letter, allowedCharactersPass, fixed=TRUE)){
      res <- c(FALSE,"Your password contains invalid characters")
      return(res)
    }
  }

  res <- c(TRUE,"")
  return(res)
}

usernameIsInDb <- function(username){
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  qryUser = dbSendQuery(mydb, paste("select count(*) as cont from users where username = '",username ,"'", sep=""))
  res = fetch(qryUser, n=-1)
  num <- (res["cont"])
  dbDisconnect(mydb)
  return(num == 1)

}

validateInput <- function(input){
  input_split <- strsplit(input, "")[[1]]

  if (nchar(input) > 100){
    res <- c(FALSE, "Input is too long")
    return(res)
  }

  if (nchar(input) < 1){
    res <- c(FALSE, "Input is missing")
    return(res)
  }

  # verify that input contains only allowed characters
  for (letter in input_split) {
    if (!grepl(letter, allowedCharacters, fixed=TRUE)){
      res <- c(FALSE, "Input contains not valid characters")
      return(res)
    }
  }
  res <- c(TRUE, "")
  return(res)
}
