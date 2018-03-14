library(d3heatmap)
library(shinysky)
library(data.table)
library(shinyTree)

library(doBy)
library(tidyr)
library(DT)
library(brapi)
library(brapps)
library(agricolae)
library(dplyr)
library(openxlsx)
#library(fbmet)
# library(fbhelp)
library(fbdesign)
library(rhandsontable)
library(shinydashboard)
library(date)

library(purrr)
# library(shinyURL)
library(qtlcharts)
library(leaflet)
library(withr)
library(dplyr)
library(st4gi)
library(tibble)
library(knitr)
library(readxl)
library(countrycode)
library(fbsites)
# library(fbmlist)
#library(fbmet)

# library(fbcheck)
#library(fbmlist)
library(shinyjs)
library(DBI)
library(RMySQL)
library(spsurvey)
library(foreign)
library(tools)
library(stringr)
library(shinyBS)
# library(fbopenbooks)
# library(fbanalysis)
# library(traittools)
library(sbformula)
library(pepa)
library(shinyFiles)
library(rlist)
library(rprojroot)
library(factoextra)
library(ggrepel)
library(countrycode)

# library(fbdocs)
# library(geneticdsg)

#package fbupdate
#library(remotes)
#library(fbupdate)
library(tibble)
#library(shinyjs)

# library(shinyalert)


# packages for HiDAP network
# library(stringi)
# library(digest)
# library(DT)
# library(shiny)
# library(shinyjs)
# library(shinydashboard)
# library(datasets)
# library(RMySQL)
# library(httr) #library for http requests, used to make POST request to the server
# library(bsplus) #hidapnetwork
# library(htmltools) #hidapnetwork
#library(hidapNetwork)

# init default data: TODO make a function with better logic checking whats new
# from fbglobal get_base_dir

#dd = system.file("xdata/Default", package = "fbglobal")
#file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)

# remove dependency on RTools by pointing to a zip.exe. NOTE: needs to be installed
# into HiDAP working dir by installer
#Sys.setenv("R_ZIPCMD" = file.path(Sys.getenv("HIDAP_HOME"), "zip.exe"))


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "HiDAP", titleWidth = "250px"
                  #tags$script(HTML("$('body').addClass('sidebar-mini');"))
  ),#end Header
  dashboardSidebar(width = "250px",

                   #div(style="margin-right: auto;",img(src = "Logo1.png", width = "250")),
                   br(),
                   div(img(src="hidapicon.png", width = "150px"), style="text-align: center;"),

                   #sidebarSearchForm(label = "Enter a word", "searchText", "searchButton"),
                   sidebarMenuOutput("menu"),#from hidap network

                   sidebarMenu(

                     id = "tabs",
                     # menuItem("Phenotype tool", icon = icon("th-list"),

                      menuItem("Phenotype", icon = icon("th-list"),

                              menuItem("Material Management",
                                       menuSubItem("Manage list", tabName = "manageList", icon = icon("table")),
                                       menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                                       menuSubItem("Family list", tabName = "createList", icon = icon("list-alt")) ,
                                       menuSubItem("Parental list", tabName = "parentList", icon = icon("list-alt")),
                                       #menuSubItem("Network List", tabName = "networkList", icon = icon("plane")),
                                       menuSubItem("Distribution Data", tabName = "distributionDB", icon = icon("database"))
                              ),

                              menuItem("Fieldbook management",
                                       menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file")),
                                       menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                       menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              ),

                              menuItem("Single Trial Analysis",
                                       #menuSubItem("Single trial graph",tabName = "SingleChart", icon = icon("calculator")),
                                       menuSubItem("Single report", tabName = "singleAnalysisReport", icon = icon("file-text-o")),
                                       menuSubItem("Genetic report", tabName = "geneticAnalysisReport", icon = icon("file-text-o"))

                                       #menuSubItem("Data Transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              ),

                              menuItem("PVS Trial Analysis",
                                       menuSubItem("PVS report", tabName = "singlePVS", icon = icon("calculator"))#,
                                       #menuSubItem("PVS anova report",tabName = "singlePVS", icon = icon("calculator"))
                              ),

                              menuItem("MET Analysis",
                                       #menuSubItem("MET analytical graph",tabName = "metAnalysisGraphs", icon = icon("calculator")),
                                       menuSubItem("MET report", tabName = "metAnalysisReport",icon = icon("file-text-o"))#,
                              ),

                              menuItem("Index Selection",
                                       menuSubItem("Elston index",tabName = "elstonIndex",icon = icon("file-text-o")),
                                       #menuSubItem("Pesek-Baker index", tabName = "pesekIndex",icon = icon("indent")),
                                       menuSubItem("Selection response", tabName = "selResponse",icon = icon("indent")),
                                       menuSubItem("Drought indexes", tabName = "droughtIndex",icon = icon("indent"))


                              )#,

                     ),



                     menuItem("Geographic Information", icon = icon("globe"),
                              menuSubItem("Locations table",tabName = "trialSitesTable",icon = icon("file-text-o")),
                              menuSubItem("Add trial sites",tabName = "trialSites", icon = icon("location-arrow"))
                              ),

                     menuItem("Documentation",  icon = icon("book"),
                              menuSubItem("HiDAP documents", tabName = "docHidap",icon = icon("file-text-o"))#,
                     ),

                     menuItem("Help",  icon = icon("refresh"),
                              menuSubItem("Check updates", tabName = "updateHidap",icon = icon("refresh"))#,
                     ),


                     menuItem("About", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)#,

                   )
                   # )
  ),

  dashboardBody(
    #
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    # ),

    includeCSS("www/custom.css"),

    tabItems(

      ##### begin UI HIDAP NETOWORK ####

      tabItem(tabName = "hnetwork",
              h2("Login or create HiDAP Network Account")),
      # tabItem(tabName = "others",
      #         h2("Panel to simulate other parts of Hidap"), h4("Other stuff")),
      # tabItem(tabName = "userProfile",
      #         h1("My profile"),
      #         br()),
      tabItem(tabName = "sharedWithMe",
              h1("Files shared with me"),
              br(),
              DT::dataTableOutput("tabSharedWithMe"),
              actionButton("btUpdWithMe", "Update table"),
              actionButton("btDownload", "Add to My Files")),
      tabItem(tabName = "sharedFromMe",
              h1("Files shared"),
              DT::dataTableOutput("tabSharedFromMe"),
              actionButton("btUpdFromMe", "Update Table")),
      tabItem(tabName = "shareFile",
              h1("Share a file"),
              selectizeInput("userSelection" ,  width="500px", multiple = TRUE, choices = NULL, label="Select users to share with", options = list(maxOptions = 5 ,  placeholder = 'Select users')),
              DT::dataTableOutput("tabFilesToShare"),
              uiOutput('obsInput'),
              actionButton("btUpload", "Share")),
      tabItem(tabName = "login",  div( uiOutput("uiLogin"), uiOutput("pass"), style = 'max-width:700px;' )),
      tabItem(tabName = "logoutTab",div(uiOutput("uiLogout"))),
      tabItem(tabName = "changePass",div(uiOutput("uiChangePass"), uiOutput("mssgChngPass"),style = 'max-width:700px;')),
      tabItem(tabName = "userProfile",div(uiOutput("uiUserProfile"), style = 'max-width:700px;')),

      ##### end UI HIDAP NETOWORK ####


      ###
      #Codigo Ivan Perez
      tabItem(tabName = "dashboard",

              #br(h2("Highly Interactive Data Analysis Platform")),
              #br( p(class = "text-muted", style="text-align:right", "Highly Interactive Data Analysis Platform")),

              # br(),
              # br(),
              #img(src="potato.jpg", width = "100%"),-
              img(src="about.jpg", width = "100%"),

              br(),
              br(),

              h3("HiDAP v.1.0.3"),
              p(class = "text-muted", style="text-align:justify",
                #paste("HiDAP is a Highly Interactive Data Analysis Platform originally meant to support clonal crop breeders at the <a href='http://www.cipotato.org' target='_new'>International Potato Center</a>. It is part of a continuous institutional effort to improve data collection, data quality, data analysis and open access publication. The recent iteration simultaneously also represents efforts to unify best practices from experiences in breeding data management of over 10 years, specifically with DataCollector and CloneSelector for potato and sweetpotato breeding, to address new demands for open access publishing and continue to improve integration with both corporate and community databases (such as biomart and sweetpotatobase) and platforms such as the <a href='https://research.cip.cgiar.org/gtdms/' target='_new'> Global Trial Data Management System (GTDMS)</a> at CIP. </br> One of the main new characteristics of the current software development platform established over the last two years is the web-based interface which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops. Key features include support for data capture, creation of field books, upload field books from and to accudatalogger, data access from breeding databases (e.g., <a href = 'http://germplasmdb.cip.cgiar.org/' target='_new'>CIP BioMart</a>, <a href='http://www.sweetpotatobase.org' target='_new'>sweetpotatobase</a> via <a href='http://docs.brapi.apiary.io/' target='_new'>breeding API</a>), data quality checks, single and multi-environmental data analysis, selection indices, and report generations. For users of DataCollector or CloneSelector many of the features are known but have been improved upon. Novel features include list management of breeding families, connection with the institutional pedigree database, interactive and linked graphs as well as reproducible reports. With the first full release by end of November 2016 we will include all characteristics from both DataCollector and CloneSelector. HIDAP, with additional support from <a href='https://sweetpotatogenomics.cals.ncsu.edu/' target='_new'>GT4SP</a>, <a href='http://www.rtb.cgiar.org/' target='_new'>RTB</a>, USAID, and <a href='http://cipotato.org/research/partnerships-and-special-projects/sasha-program/' target='_new'>SASHA</a>, is aimed to support the broader research community working on all aspects with primary focus on breeding, genetics, biotechnology, physiology and agronomy.")
                shiny::includeHTML("www/about_hidap.txt")
              ),


              br(),
              br(),

              fluidRow(
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="CIPlogo_RGB.png", width = "150px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  div(img(src="gt4sp.png", height = "108px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="usaid.png", width = "150px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  div(img(src="sasha.png"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  div(img(src="rtb.png", width = "150px"), style="text-align: center;")
                )
              ),

              br(),
              br(),
              br()
      ),

      # Design Experiments Module ----------------------------------------------------
      fbdesign::ui_fieldbook_agrofims(name = "newFieldbook")#,

      # # Data Quality and Check Fieldbook Module  ----------------------------------------------------
      # fbcheck::fbcheck_ui(name= "checkFieldbook"),
      #
      # # Fieldbook Manager Module ----------------------------------------------------
      # fbopenbooks::fbopenbooks_ui(name="openFieldbook"),
      #
      # # Data Transformation
      # fbanalysis::dtr_ui(name = "singleAnalysisTrans"),


      # # Material List Module ----------------------------------------------------
      #
      # fbmlist::generate_ui(name = "generateList"),
      # fbmlist::managerlist_ui(name = "manageList"),
      # fbmlist::createlist_ui(name = "createList"),
      # fbmlist::parent_ui(name = "parentList"),
      # fbmlist::distribution_ui(name = "distributionDB"),

      # #brapps::fbasingle_ui("SingleChart"),
      #
      # fbanalysis::single_ui(name="singleAnalysisReport"),
      # fbanalysis::genetic_ui(name="geneticAnalysisReport"),
      #
      #
      # fbanalysis::met_ui(name="metAnalysisReport"),
      # #fbmet::met_ui("metAnalysisGraphs"),
      #
      #
      # fbsites::addsite_ui(name = "trialSites"),
      # fbsites::ui_site(name ="trialSitesTable"),
      #
      #
      # fbanalysis::elston_ui(name="elstonIndex"),
      # fbanalysis::droindex_ui(name = "droughtIndex"),
      #
      # fbanalysis::ui_pvs(name = "singlePVS"),
      #
      # fbdocs::fbdocs_ui(name = "docHidap") ,
      #
      # #Hidap Update Module
      # fbupdate::fbupdate_ui(name = "updateHidap")#,


    ) , #end of TabSetPanel

    tags$div(
      fluidRow(
        tags$footer(
          a(
            list(
              tags$div(id = "test", img(src="cc_by.png"), "2018 International Potato Center. Av La Molina 1895, La Molina - Peru.")
            ),
            href="#"
          ),
          tags$style("footer {background-color: #222d32;height: 40px;position: absolute;bottom: 0;width: 100%;}"),
          tags$style("#test {color: #fff;padding-top: 5px;}")
        )
      )
    )

  )
)




############################################################

sv <- function(input, output, session) ({

  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")



  withProgress(message = 'Loading HiDAP', value = 0, {

  incProgress(1/25, detail = paste("..."))

  # fbcheck::fbcheck_server(input, output, session, values)

  #fbmlist::server_managerlist(input, output, session, values)
  #fbmlist::server_generate(input, output, session, values)
  #fbmlist::server_createlist(input, output, session, values)
  #fbmlist::server_parentlist(input, output, session, values)
  #fbmlist::server_distribution(input,output,session, values)

  incProgress(2/25, detail = paste("..."))


  fbdesign::server_design_agrofims(input, output, session, values)
  # fbdesign::server_design_big(input, output, session, values)
  # fbopenbooks::fbopenbooks_server(input, output, session, values)
  # fbanalysis::single_server(input, output, session, values)
  # fbanalysis::dtr_server(input, output, session, values)
  #
  # fbanalysis::met_server(input, output, session, values)
  #
  # incProgress(3/25, detail = paste("..."))
  #
  # fbanalysis::elston_server(input, output, session, values)
  #
  # incProgress(4/25, detail = paste("..."))
  #
  # fbanalysis::pbaker_server(input, output, session, values)
  #
  # incProgress(5/25, detail = paste("..."))
  #
  # fbanalysis::droindex_server(input, output, session, values = values)
  #
  # fbanalysis::pvs_server(input, output, session, values)
  #
  # incProgress(6/25, detail = paste("..."))
  #
  # fbanalysis::genetic_server(input, output, session, values)
  #
  # incProgress(8/25, detail = paste("..."))
  #
  # fbanalysis::pvs_anova_server(input, output, session, values)
  #
  #
  # incProgress(9/25, detail = paste("..."))
  #
  # fbdocs::fbdocs_server(input, output, session, values)
  #
  # incProgress(10/25, detail = paste("..."))
  #
  # fbsites::server_addsite(input, output, session, values = values)
  # fbsites::server_site(input, output, session, values = values)
  #
  # incProgress(12/25, detail = paste("..."))
  #
  # fbupdate::fbupdate_server(input, output, session, values = values)
  #
  # incProgress(15/25, detail = paste("..."))
  #
  #
  #     incProgress(16/25, detail = paste("..."))

  #  ################## NETWORK #####################################################
  # constUserDB <- "dspotatotrials"
  # constPassDB <- "ca7H=j~$V+p2G0715"
  # constDBName <- "hidap_network"
  # constDBHost <- "176.34.248.121"
  #
  # source("www/Login.R",  local = TRUE)
  #
  # ## begin login.R #############################
  #
  #   incProgress(17/25, detail = paste("..."))
  # #########end login .R #####################
  #
  # #myLocalFilesDir <- "./xdata/" # path to folder containing files to share
  #
  # myLocalFilesDir <- fbglobal::get_base_dir()
  # #myLocalFilesDir <- file.path(myLocalFilesDir, dbf_file)
  # out_list <- c("potato_db_distribution.rds", "hot_fieldbook.rds" ,"dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds", "sweetpotato_pedigree.rds", "table_sites.rds")
  # myLocalFiles_list <- list.files(list.files(myLocalFilesDir, pattern = "\\.rds$"))
  # myLocalFilesDir <- setdiff(myLocalFiles_list, out_list)
  #
  # localFiles <- data.frame(myLocalFilesDir)
  # names(localFiles)  <- c("File Name")
  # allowedCharacters  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-,()?!*@%&[]{}+=$# "
  # allowedCharactersPass  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-#@?!%&,*;"
  # allowedCharactersMail  <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_.-@+"
  # tabSharedWithMe <- NULL
  # tabMyLocalFiles <- reactive({localFiles})
  #
  # output$tabFilesToShare = DT::renderDataTable({
  #   server = TRUE
  #   isolate({
  #     datatable(
  #       tabMyLocalFiles(),
  #       class = 'cell-border stripe',
  #       # extensions = 'FixedColumns',
  #       options = list(
  #         scrollX = TRUE,
  #         pageLength = 5
  #       )
  #     )
  #   })
  # })
  #
  #       incProgress(18/25, detail = paste("..."))
  # ###########################################################################################################
  # # changing user password
  # ###########################################################################################################
  # observeEvent(input$btChangePass, {
  #   if (!validateChangePassForm()) return()
  #
  #   #mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #   m <- dbDriver("MySQL");
  #   mydb <-  DBI::dbConnect(m, user=constUserDB, password=constPassDB, host=constDBHost , dbname=constDBName)
  #
  #
  #
  #   strQry = paste("select username, password  from users where username = '", USER$username, "' and available = 1", sep = "")
  #   res <- data.frame(fetch(dbSendQuery(mydb,strQry)))
  #   userDb <- res[,1]
  #   if(length(userDb) == 1){
  #     newPass <- input$chngPassCurrent
  #     Password <- digest(isolate(newPass), "sha256", serialize = FALSE)
  #     curPass <- res[1,"password"]
  #
  #     if(curPass == Password){
  #       newPass = digest(isolate(input$chngPassNew), "sha256", serialize = FALSE)
  #       strQry = paste("update users set password = '",newPass,"' where username ='",USER$username, "' and password = '",curPass,"' and available = 1", sep="")
  #
  #       updQry = dbSendQuery(mydb,strQry)
  #
  #
  #       params <- list(
  #         dataRequest = "passwordChanged",
  #         username = USER$username,
  #         fname = USER$fname,
  #         lname = USER$lname
  #       )
  #
  #       var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidapNetwork/emailPasswordChanged.php", body=params)
  #       code <- content(var, "text")
  #       output$mssgChngPass <- renderText("<font color='blue'><h3>Your password was successfully changed</h3></font>")
  #
  #       output$uiChangePass <- renderUI({
  #         if (USER$Logged == TRUE) {
  #           wellPanel(
  #             h3("Password Change"),
  #             passwordInput("chngPassCurrent", "Current password: "),
  #             passwordInput("chngPassNew", "New password (at least 8 and at most 12 characters) "),
  #             passwordInput("chngPassNewRep", "Re-enter new password: "),
  #             actionButton("btChangePass", "Update")
  #           )
  #         }
  #       })
  #
  #     }
  #     else{
  #       output$mssgChngPass <- renderText("<font color='red'><h3>Incorrect Password</h3></font>")
  #     }
  #   }
  #   else{
  #     output$mssgChngPass <- renderText(" <font color='red'><h3>Error while changing password. Please try again</h3></font>")
  #   }
  #
  #   dbDisconnect(mydb)
  # })
  #
  #     incProgress(19/25, detail = paste("..."))
  #
  # ###########################################################################################################
  # # reseting user password
  # ###########################################################################################################
  # observeEvent(input$ResetPass,{
  #   usermail <- trimws(input$userMailReset)
  #
  #   validEmail <- validateEmail(usermail)
  #   if(!as.logical(validEmail[1])){
  #     output$pass <- renderText(paste("<font color='red'> <h4><b>", usermail, ": </b> ", validEmail[2], "</h4> </font>", sep=""))
  #     return()
  #   }
  #
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #
  #   strQry = paste("select count(*) as cant from users where username = '", usermail, "' and available = 1", sep = "")
  #   res <- fetch(dbSendQuery(mydb,strQry))
  #   num <- (res["cant"])
  #   dbDisconnect(mydb)
  #   if (num == 1 ){
  #
  #     params <- list(
  #       dataRequest = "resetPassword",
  #       username = usermail
  #     )
  #
  #     var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidapNetwork/resetPasswordHidap.php", body=params)
  #     code <- content(var, "text")
  #     if (code == "200"){
  #       # showModal(modalDialog(title = "Hidap Network", HTML("Succesfully reset")))
  #       output$pass <- renderText("<h4>Password reset success. An email has been sent with a new password </h4>")
  #     }
  #     else{
  #       # showModal(modalDialog(title = "Hidap Network", HTML("Problems reseting password")))
  #       output$pass <- renderText("<font color='red'> <h5>Problems reseting password</h5> </font>")
  #     }
  #   }
  #   else{
  #     output$pass <- renderText(paste("<font color='red'> <h4>User <b>", usermail, "</b> is not registered</h4> </font>", sep=""))
  #   }
  # } )
  #
  #
  #     incProgress(20/25, detail = paste("..."))
  # ###########################################################################################################
  # # creating a new user
  # ###########################################################################################################
  # observeEvent(input$btCreateUser, {
  #   if (!validateUserCreateForm()) return()
  #
  #   strMail  <- trimws(input$newUsrMail)
  #   strPass  <- digest(trimws(input$newUsrPass), "sha256", serialize = FALSE)
  #   strFName <- trimws(input$newUsrFName)
  #   strLName <- trimws(input$newUsrLName)
  #   strOrg   <- trimws(input$newUsrOrg)
  #   strCounS <- trimws(input$countrySelection)
  #
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #   strQry <- paste("insert into users (username, password, fname, lname, organization, country) values('",strMail,"','",strPass,"','", strFName,"','",strLName,"','",strOrg, "','",strCounS, "')", sep ="")
  #   qryUpdate = dbSendQuery(mydb, strQry)
  #
  #
  #   params <- list(
  #     dataRequest = "createUser",
  #     username = strMail
  #   )
  #
  #   var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidapNetwork/createNewUser.php", body=params)
  #   code <- content(var, "text")
  #   if (code == "500"){
  #     strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
  #     qryDel = dbSendQuery(mydb, strQry)
  #     showModal(modalDialog(title = "HiDAP Network", HTML("Problems creating account, please try again.")))
  #   }
  #   else if (code == "200") {
  #     showModal(modalDialog(title = "HiDAP Network", HTML("<h4>Your account was successfully created, a confirmation message will be sent soon. Check your email to activate your account.</h4> <br> <h5>If you haven't received the message, please check your spam and add <a href='cip-riu@cgiar.org'>cip-riu@cgiar.org</a> to your contacts.</h5>")))
  #     output$uiLogin <- renderUI({
  #
  #       if (USER$Logged == FALSE) {
  #         wellPanel(
  #           h3("Start a new session!"),
  #           textInput("userName", "Username:"),
  #           passwordInput("passwd", "Password:"),
  #           br(),
  #           actionButton("Login", "Log in"),
  #           br(),
  #           br(),
  #           actionLink("ForgotPass", "Forgot your password?"),br(),
  #           "Not a user yet? ", actionLink("btCreate", "Create a new account.")
  #         )
  #       }
  #     })
  #   }
  #   else{
  #     strQry <- paste("delete from users where username = '", strMail, "'", sep ="")
  #     qryDel = dbSendQuery(mydb, strQry)
  #     showModal(modalDialog(title = "HiDAP Network", HTML("Problems creating account, please try later.")))
  #   }
  #   dbDisconnect(mydb)
  #
  # })
  #
  # ###########################################################################################################
  # # downloading the file from server and saving to a specific destination folder
  # ###########################################################################################################
  # observeEvent(input$btDownload, {
  #
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #   serverRoute = "https://research.cip.cgiar.org/gtdms/hidap/script/filesFromHidapNetwork/"
  #   #homeRoute = "./downloadedFiles/"
  #   homeRoute = fbglobal::get_base_dir()
  #
  #   filesToDownload  <- input$tabSharedWithMe_rows_selected
  #   qryUpd = paste("update shared_files set downloaded = 1, times_downloaded = times_downloaded + 1,  date_downloaded = '", as.character(Sys.time(), "%Y-%m-%d %H:%M:%S"), "' where id = ", sep ="")
  #
  #   for (id in filesToDownload) {
  #     qryFileName = dbSendQuery(mydb, paste("select system_file_name, times_downloaded, username from shared_files a, users b where a.owner_id = b.id and a.id = ", tabSharedWithMe()[id,1]))
  #     encodedFileName = fetch(qryFileName, n=-1)
  #     fileName = paste(stri_sub(tabSharedWithMe()[id,2], 1, -5),"_", encodedFileName["username"], "_v", encodedFileName["times_downloaded"] + 1, stri_sub(tabSharedWithMe()[id,2], -4, -1), sep = "")
  #     #fileDestination = paste(homeRoute, fileName, sep="")
  #     fileDestination <-  file.path(homeRoute, fileName)
  #     fileRoute = paste(serverRoute, encodedFileName["system_file_name"], sep="")
  #     #download.file(fileRoute, fileDestination, method = "curl") for Linux
  #     download.file(fileRoute, fileDestination)
  #     qryUpdate = dbSendQuery(mydb, paste(qryUpd, tabSharedWithMe()[id,1]))
  #   }
  #
  #   dbDisconnect(mydb)
  #   showModal(modalDialog(title = "HiDAP Network",HTML("The files were succesfully downloaded")))
  #   updSharedWithMe()
  #
  # })
  #
  # output$obsInput <- renderUI({
  #   textInput("remarks", "Remarks")
  # })
  #
  #     incProgress(21/25, detail = paste("..."))
  # ###########################################################################################################
  # # uploading files
  # ###########################################################################################################
  # observeEvent(input$btUpload, {
  #
  #   numUser <- length(input$userSelection)
  #   usersToShareWith <- input$userSelection
  #   observation = trimws(input$remarks)
  #
  #   validObs <- validateInput(observation)
  #
  #   if(!as.logical(validObs[1])){
  #     showModal(modalDialog(title = "HiDAP Network", HTML(validObs[2])))
  #     return()
  #   }
  #
  #   filesToShare  <- input$tabFilesToShare_rows_selected
  #
  #   if (filesToShare < 1 || numUser < 1){
  #     showModal(modalDialog(title = "HiDAP Network", HTML("Users or files are not selected.")))
  #     return()
  #   }
  #
  #   date  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
  #   sharedDate = as.character(Sys.time(), "%Y-%m-%d %H:%M:%S")
  #   # observation = input$remarks
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #
  #   usersStr <- ""
  #   ownerId = USER$id
  #
  #   code <- "200"
  #   message <- ""
  #
  #   for (id in filesToShare) {
  #     fileName <- tabMyLocalFiles()[id,1]
  #     ranStr <-  stri_rand_strings(1, 15,  '[a-zA-Z0-9]')
  #     servName = paste(date, ranStr, sep="-")
  #
  #
  #     usrIns <- 0
  #     for (user in usersToShareWith){
  #       aux_idx <- stri_locate_last(user, pattern="<", fixed=TRUE)
  #       rl_usrnm <- stri_sub(user, aux_idx[[1]] +1, -2)
  #       usrIdQry <- paste( "select id from users where username = '", rl_usrnm, "' and available = 1", sep="")
  #       usrId = dbSendQuery(mydb, usrIdQry)
  #       receiverId = fetch(usrId, n=-1)
  #       if(length(receiverId[["id"]])==1){
  #         insQry= "insert into shared_files (shared_file_name, system_file_name, date_shared, observation, owner_id, receiver_id) values('"
  #         insQry= paste(insQry, fileName, sep="")
  #         insQry= paste(insQry, servName, sharedDate, observation,  sep="','")
  #         insQry= paste(insQry, "'", sep="")
  #         insQry= paste(insQry, ownerId, receiverId ,  sep=",")
  #         insQry= paste(insQry, ")",  sep="")
  #         qryUsers = dbSendQuery(mydb, insQry)
  #         usrIns <- usrIns + 1
  #       }
  #
  #     }
  #
  #     if(usrIns >  0){
  #
  #       print(myLocalFilesDir)
  #       print(fileName)
  #       upd_file <- file.path(myLocalFilesDir, fileName)
  #
  #
  #       params <- list(
  #         dataRequest = "uploadFile",
  #         fileServerName = servName,
  #
  #         #filedata= upload_file(paste(myLocalFilesDir, fileName, sep ="" ), "text/csv")
  #         filedata= upload_file(upd_file, "text/csv")
  #       )
  #
  #       var <- POST("https://research.cip.cgiar.org/gtdms/hidap/script/hidapNetwork/getFileUpload.php", body=params)
  #       code <- content(var, "text")
  #
  #       if (code == "200")
  #         message <- paste0(message, fileName, " was successfully shared <br>")
  #       else{
  #         message = paste0( message, "Error while sharing ", fileName , ". Please Try again. <br>")
  #         delQry = paste0("delete from shared_files where system_file_name = '", servName, "'")
  #         qrydDel = dbSendQuery(mydb, delQry)
  #       }
  #     }
  #   }
  #
  #   updateTextInput(session, "remarks", value = "")
  #
  #   showModal(modalDialog(title = "HiDAP Network", HTML(message)))
  #
  #   dbDisconnect(mydb)
  #   updSharedWithMe()
  #   updSharedFromMe()
  # })
  #
  # observeEvent(input$btUpdFromMe, {
  #   updSharedFromMe()
  # })
  #
  # observeEvent(input$btUpdWithMe, {
  #   updSharedWithMe()
  # })
  #
  #
  #     incProgress(22/25, detail = paste("..."))
  # ###########################################################################################################
  # # to perform when a user logs in
  # ###########################################################################################################
  # observe({
  #   if (USER$Logged == TRUE) {
  #
  #     # menu to be shown with hidap network options when the users logs in
  #     output$menu <- renderMenu({
  #       sidebarMenu(id ="networkMenu",
  #                   #menuItem("Others", tabName = "others", icon = icon("dashboard")),
  #                   menuItem("HiDAP Network", tabName = "hnetwork", icon = icon("users"),
  #                            menuSubItem("My Profile", tabName = "userProfile", icon = icon("user")),
  #                            menuSubItem("Shared with me", tabName = "sharedWithMe", icon = icon("inbox")),
  #                            menuSubItem("Files shared", tabName = "sharedFromMe", icon = icon("share")),
  #                            menuSubItem("Share a file", tabName = "shareFile", selected=T, icon = icon("upload")),
  #                            menuSubItem("Change Password", tabName = "changePass", icon = icon("lock")),
  #                            menuSubItem("Log Out", tabName = "logoutTab", icon = icon("sign-out"))
  #                   )
  #       )
  #     })
  #
  #     updSharedWithMe() #update table of files shared with me
  #     updSharedFromMe() #update table of files shared from me
  #
  #
  #     users = USER$list
  #     l_user = paste(USER$fname, USER$lname, paste0("<",USER$username,">"))
  #     users = users[users != l_user]
  #     updateSelectizeInput(session, 'userSelection', choices = unlist(users, use.names=FALSE), server = TRUE)
  #     updateTabItems(session, "shareFile", selected = TRUE)
  #
  #   } else {
  #     output$menu <- renderMenu({
  #       sidebarMenu(id ="networkMenu",
  #                   #menuItem("Others", tabName = "others", icon = icon("dashboard")),
  #                   menuItem("HiDAP Network", tabName = "hnetwork", icon = icon("users"),
  #                            menuSubItem("Log in", tabName = "login", icon = icon("user"))
  #                   )
  #
  #       )
  #     })
  #
  #   }
  # })
  #
  #     incProgress(23/25, detail = paste("..."))
  # ###########################################################################################################
  # # auxiliar functions
  # ###########################################################################################################
  # updSharedWithMe <- function() {
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #
  #   # query to get all files that have been shared with current logged user
  #   strQry = paste("SELECT a.id as fileID,
  #                  a.shared_file_name as fileName,
  #                  b.username as owner,
  #                  b.fname as name,
  #                  b.lname as lastname,
  #                  a.observation as observation,
  #                  a.date_shared as date_shared,
  #                  a.downloaded as downloaded,
  #                  a.date_downloaded as date_downloaded,
  #                  a.times_downloaded as times_donwloaded,
  #                  a.available as available
  #                  FROM shared_files a, users b
  #                  WHERE a.receiver_id = ",USER$id, " and a.owner_id = b.id ", " order by date_shared desc", sep="")
  #
  #   qrySharedWithMe = dbSendQuery(mydb,strQry)
  #   filesSharedWithMe = fetch(qrySharedWithMe, n=-1)
  #   df_withMe <<- data.frame(filesSharedWithMe)
  #   df_withMe$downloaded[df_withMe$downloaded == 0 ] <- "No"
  #   df_withMe$downloaded[df_withMe$downloaded == 1 ] <- "Yes"
  #   df_withMe$available[df_withMe$available == 0 ] <- "No"
  #   df_withMe$available[df_withMe$available == 1 ] <- "Yes"
  #   names(df_withMe)  <- c("fileID", "File Name", "Owner" ,"Name", "Last Name","Remarks", "Date Shared", "Downloaded", "Date Downloaded","Times Downloaded", "Available")
  #   tabSharedWithMe <<- reactive({df_withMe})
  #   # showing table with files with from me
  #   output$tabSharedWithMe = DT::renderDataTable({
  #     server = TRUE
  #     isolate({
  #       # data <- df_withMe
  #       datatable(
  #         # data,
  #         tabSharedWithMe(),
  #         class = 'cell-border stripe',
  #         extensions = 'FixedColumns',
  #         options = list(
  #           scrollX = TRUE,
  #           pageLength = 10,
  #           columnDefs = list(list(visible=FALSE, targets=1))
  #         )
  #       )
  #     })
  #   })
  #
  #   dbDisconnect(mydb)
  # }
  #
  # updSharedFromMe <- function(){
  #
  #   mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #   qryUsers = dbSendQuery(mydb, "select id,  username from users where available = 1")
  #   dataUsers = fetch(qryUsers, n=-1)
  #
  #   #query to get all files that have been shared from current user
  #   strQry = paste("SELECT a.id as fileID,
  #                  a.shared_file_name as fileName,
  #                  b.username as receiver,
  #                  b.fname as name,
  #                  b.lname as lastname,
  #                  a.observation as observation,
  #                  a.date_shared as date_shared,
  #                  a.downloaded as downloaded,
  #                  a.date_downloaded as date_downloaded,
  #                  a.times_downloaded as times_donwloaded,
  #                  a.available as available
  #                  FROM shared_files a, users b
  #                  WHERE a.owner_id = ",USER$id, " and  a.receiver_id = b.id ", " order by date_shared desc", sep="")
  #
  #   qrySharedFromMe = dbSendQuery(mydb,strQry)
  #   filesSharedFromMe <<- fetch(qrySharedFromMe, n=-1)
  #   df_fromMe = data.frame(filesSharedFromMe)
  #   df_fromMe$downloaded[df_fromMe$downloaded == 0 ] <- "No"
  #   df_fromMe$downloaded[df_fromMe$downloaded == 1 ] <- "Yes"
  #   df_fromMe$available[df_fromMe$available == 0 ] <- "No"
  #   df_fromMe$available[df_fromMe$available == 1 ] <- "Yes"
  #   names(df_fromMe) <- c("fileID", "File Name","Shared with", "Name", "Lastname", "Remarks" , "Date Shared", "Downloaded", "Date Downloaded", "Times Downloaded", "Available")
  #
  #   # showing table with files shared from me
  #   output$tabSharedFromMe = DT::renderDataTable({
  #     server = TRUE
  #     isolate({
  #       data <- df_fromMe
  #       datatable(
  #         data,
  #         class = 'cell-border stripe',
  #         # extensions = 'FixedColumns',
  #         selection = "none",
  #         options = list(
  #           scrollX = TRUE,
  #           # fixedColumns = list(leftColumns = 2, rightColumns = 0),
  #           pageLength = 10,#,
  #           # autoWidth = TRUE,
  #           columnDefs = list(list(visible=FALSE, targets=1))
  #         )
  #       )
  #     })
  #   })
  #   dbDisconnect(mydb)
  # }
  #
  # validateUserCreateForm <- function(){
  #   mail <- trimws(input$newUsrMail)
  #   fName <- trimws(input$newUsrFName)
  #   lName <- trimws(input$newUsrLName)
  #   org   <- trimws(input$newUsrOrg)
  #   pass <- trimws(input$newUsrPass)
  #
  #   validEmail <- validateEmail(mail)
  #
  #   if(!as.logical(validEmail[1])){
  #     updateTextInput(session, "newUsrMail",
  #                     label ="* Email Address (username): INVALID EMAIL ")
  #     output$pass <- renderText(paste("<font color=red><h4>", validEmail[2],"</h4></font>", sep = ""))
  #     return(FALSE)
  #   }
  #   else{
  #     updateTextInput(session, "newUsrMail",
  #                     label ="* Email Address (username): ")
  #   }
  #
  #   if(usernameIsInDb(mail)){
  #     output$pass <- renderText("<font color=red><h4>User is already registered</h4></font>")
  #     return(FALSE)
  #   }
  #
  #   lnMail  <- nchar(mail)
  #   lnPass  <- nchar(pass)
  #   lnPassR <- nchar(trimws(input$newUsrPassRepeat))
  #   lnFName <- nchar(fName)
  #   lnLName <- nchar(lName)
  #   lnOrg   <- nchar(org)
  #   lnCounS <- nchar(trimws(input$countrySelection))
  #
  #   lenghtValid <- lnMail * lnPass * lnPassR * lnFName * lnLName * lnOrg * lnCounS
  #   passwMatch  <- lnPass == lnPassR && pass == trimws(input$newUsrPassRepeat)
  #
  #   if(lenghtValid == 0){
  #     output$pass <- renderText("<font color=red><h4>Must complete all fields with (*)</h4></font>")
  #     return(FALSE)
  #   }
  #
  #   valPass <- validatePassword(pass)
  #
  #   if(!as.logical(valPass[1])){
  #     output$pass <- renderText(paste("<font color=red><h4>", valPass[2],"</h4></font>", sep=""))
  #     return(FALSE)
  #   }
  #
  #   if(!passwMatch ){
  #     output$pass <- renderText("<font color=red><h4>Passwords don't match</h4></font>")
  #     return(FALSE)
  #   }
  #
  #   validFname <- validateInput(fName)
  #   if(!as.logical(validFname[1])){
  #     updateTextInput(session, "newUsrFName",
  #                     label ="* Name: INVALID STRING")
  #     output$pass <- renderText(paste0("<font color=red><h4>", validFname[2], "</h4></font>"))
  #     return(FALSE)
  #   }
  #   else{
  #     updateTextInput(session, "newUsrFName",
  #                     label ="* Name: ")
  #   }
  #
  #   validLname <- validateInput(lName)
  #   if(!as.logical(validLname[1])){
  #     updateTextInput(session, "newUsrLName",
  #                     label ="* Last Name: INVALID STRING")
  #     output$pass <- renderText(paste0("<font color=red><h4>", validLname[2], "</h4></font>"))
  #     return(FALSE)
  #   }
  #   else{
  #     updateTextInput(session, "newUsrLName",
  #                     label ="* Last Name: ")
  #   }
  #
  #   validOrg <- validateInput(org)
  #   if(!as.logical(validOrg[1])){
  #     updateTextInput(session, "newUsrOrg",
  #                     label ="* Organization: INVALID STRING")
  #     output$pass <- renderText(paste0("<font color=red><h4>", validOrg[2], "</h4></font>"))
  #     return(FALSE)
  #   }
  #   else{
  #     updateTextInput(session, "newUsrOrg",
  #                     label ="* Organization: ")
  #   }
  #
  #   output$pass <- renderText("")
  #   return(TRUE)
  #
  # }
  #
  # validateChangePassForm <- function(){
  #   curPass <- trimws(input$chngPassCurrent)
  #   newPass <- trimws(input$chngPassNew)
  #
  #   lnCurPass   <- nchar(curPass)
  #   lnNewPass   <- nchar(newPass)
  #   lnNewPRep   <- nchar(trimws(input$chngPassNewRep))
  #
  #   lenghtValid <- lnCurPass * lnNewPass * lnNewPRep
  #   passwMatch  <- lnNewPass == lnNewPRep && trimws(input$chngPassNew) == trimws(input$chngPassNewRep)
  #
  #   if(lenghtValid == 0){
  #     showModal(modalDialog(title = "HiDAP Network", HTML("Must complete all fields")))
  #     return(FALSE)
  #   }
  #
  #   validPass <- validatePassword(curPass)
  #   if(!as.logical(validPass[1])){
  #     showModal(modalDialog(title = "HiDAP Network", HTML(paste("Current Password:", validPass[2]))))
  #     return(FALSE)
  #   }
  #
  #   validPass <- validatePassword(newPass)
  #   if(!as.logical(validPass[1])){
  #     showModal(modalDialog(title = "HiDAP Network", HTML(paste("New Password:", validPass[2]))))
  #     return(FALSE)
  #   }
  #
  #   if(!passwMatch ){
  #     showModal(modalDialog(title = "HiDAP Network", HTML("New Passwords don't match")))
  #     return(FALSE)
  #   }
  #
  #   return(TRUE)
  # }
  #
  # validateEmail <- function(mail){
  #
  #   res <- c(TRUE, "")
  #   if (!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",mail, ignore.case=TRUE)){
  #     res <- c(FALSE, "Not a valid Email")
  #     return(res)
  #   }
  #
  #   mail_split <- strsplit(mail, "")[[1]]
  #   for (letter in mail_split) {
  #     if (!grepl(letter, allowedCharactersMail, fixed=TRUE)){
  #       res <- c(FALSE, "Email contains not valid characters")
  #       return(res)
  #     }
  #   }
  #
  #   return (res)
  #
  # }
  #
  # validatePassword <-function(pass){
  #   lnPass  <- nchar(pass)
  #
  #   if(lnPass < 8 || lnPass > 12 ){
  #     res <- c(FALSE,"Your password must contain at least 8 and at most 12 characters" )
  #     return(res)
  #   }
  #
  #   pass_split <- strsplit(pass, "")[[1]]
  #   # verify that pass contains only allowed characters
  #   for (letter in pass_split) {
  #     if (!grepl(letter, allowedCharactersPass, fixed=TRUE)){
  #       res <- c(FALSE,"Your password contains invalid characters")
  #       return(res)
  #     }
  #   }
  #
  #   res <- c(TRUE,"")
  #   return(res)
  # }
  #
  # usernameIsInDb <- function(username){
  #
  #   #mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  #   m <- dbDriver("MySQL");
  #   mydb <-  DBI::dbConnect(m, user=constUserDB, password=constPassDB, host=constDBHost , dbname=constDBName)
  #
  #
  #   qryUser = dbSendQuery(mydb, paste("select count(*) as cont from users where username = '",username ,"'", sep=""))
  #   res = fetch(qryUser, n=-1)
  #   num <- (res["cont"])
  #   dbDisconnect(mydb)
  #   return(num == 1)
  #
  # }
  #
  # validateInput <- function(input){
  #   input_split <- strsplit(input, "")[[1]]
  #
  #   if (nchar(input) > 100){
  #     res <- c(FALSE, "Input is too long")
  #     return(res)
  #   }
  #
  #   if (nchar(input) < 1){
  #     res <- c(FALSE, "Input is missing")
  #     return(res)
  #   }
  #
  #   # verify that input contains only allowed characters
  #   for (letter in input_split) {
  #     if (!grepl(letter, allowedCharacters, fixed=TRUE)){
  #       res <- c(FALSE, "Input contains not valid characters")
  #       return(res)
  #     }
  #   }
  #   res <- c(TRUE, "")
  #   return(res)
  # }
  #
  #
     incProgress(25/25, detail = paste("..."))
  #
   }) #end shiny progress bar
  #
  # ################## End NETWORK ######################################################################################

})

shinyApp(ui, sv)
