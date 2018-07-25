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
library(stringi)
library(digest)
# library(DT)
library(shiny)
# library(shinyjs)
# library(shinydashboard)
library(datasets)
# library(RMySQL)
library(httr) #library for http requests, used to make POST request to the server
library(bsplus) #hidapnetwork
library(htmltools) #hidapnetwork

library(lubridate)
library(shinyalert) #new

### for maps rendering
library(ggmap)
library(leaflet)
library(magrittr)
library(maps)
library(maptools)
library(raster)
library(rgeos)
library(sp)

## devtools::install_github("dkahle/ggmap") ## use this version of ggmap

### libraries for the "remember me" 
library(shinyStore)  ## install_github("trestletech/shinyStore")
library(PKI)
privKey <- PKI.load.key(file="test.key")

pubKey <- PKI.load.key(file="test.key.pub")

# init default data: TODO make a function with better logic checking whats new
# from fbglobal get_base_dir
#dd = system.file("xdata/Default", package = "fbglobal")
#file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)
# remove dependency on RTools by pointing to a zip.exe. NOTE: needs to be installed
# into HiDAP working dir by installer
#Sys.setenv("R_ZIPCMD" = file.path(Sys.getenv("HIDAP_HOME"), "zip.exe"))



source("www/loginModule/dbData.R", local = TRUE)

#$(".headerTopRight").empty()
# jscode <- '
#     shinyjs.foo = function(params) {
#
#     console.log(params);
#
# }
# '
sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo"

jscode <- '
shinyjs.collapse = function(boxid) {
$("#" + boxid).closest(".box").find("[data-widget=collapse]").click();
}

shinyjs.getcookie = function(params) {
  var user = Cookies.get("user_af");
  var pass = Cookies.get("pass_af");
  if (typeof pass !== "undefined") {
    
    Shiny.onInputChange("jscookie_user", user);
    Shiny.onInputChange("jscookie_pass", pass);
    //Shiny.onInputChange("jscookie", user + pass);
  } else {
    var cookie = "";
    //Shiny.onInputChange("jscookie", cookie);
    Shiny.onInputChange("jscookie_user", cookie);
    Shiny.onInputChange("jscookie_pass", cookie);
  }
}
shinyjs.setcookie = function(params) {
  Cookies.set("user_af", escape(params[0]), { expires: 0.5 });
  Cookies.set("pass_af", escape(params[1]), { expires: 0.5 }); 
   Shiny.onInputChange("jscookie_user", escape(params[0]));
}
shinyjs.rmcookie = function(params) {
  Cookies.remove("user_af");
  Cookies.remove("pass_af");
  //Shiny.onInputChange("jscookie", "");
}

'

ui <- dashboardPage(


  
  
  # skin = "green",
  dashboardHeader(title = "", titleWidth = "250px"
                  #tags$script(HTML("$('body').addClass('sidebar-mini');"))
  ),#end Header
  dashboardSidebar(width = "250px",

                   #div(style="margin-right: auto;",img(src = "Logo1.png", width = "250")),
                   br(),
                   # div(img(src="missing-image.png", width = "150px"), style="text-align: center;"),
                   div(sidebarMenuOutput("menuUser")),

                   sidebarMenu(

                     id = "tabs",
                     # menuItem("Phenotype tool", icon = icon("th-list"),


                     br(),
                     sidebarMenuOutput("menu")#, #menu is render in login.R when users logs in
                     # menuItem("Site information", tabName = "trialSite", icon = icon("map-marker")),
                     # menuItem("Fieldbook", icon = icon("book"),
                     #          #menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file")),
                     #          menuSubItem("Create fieldbook", tabName = "newFieldbookAgrofims", icon = icon("file")),
                     # 
                     #          menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                     #          menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser"))#,
                     #          # menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                     # ),


                     #  menuItem("Phenotype", icon = icon("th-list"),
                     #
                     #          menuItem("Material Management",
                     #                   #menuSubItem("Manage list", tabName = "manageList", icon = icon("table")),
                     #                   # menuSubItem("Manage list", tabName = "manageListAgrofims", icon = icon("table")),
                     #                   #menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                     #                   # menuSubItem("Clone list", tabName = "generateListAgrofims", icon = icon("list")),
                     #                   menuSubItem("Material list", tabName = "materialList", icon = icon("list"))
                     #                   # menuSubItem("Trial site", tabName = "trialSite", icon = icon("list"))
                     #                   #menuSubItem("Family list", tabName = "createList", icon = icon("list-alt")) ,
                     #                   #menuSubItem("Parental list", tabName = "parentList", icon = icon("list-alt")),
                     #                   #menuSubItem("Network List", tabName = "networkList", icon = icon("plane")),
                     #                   #menuSubItem("Distribution Data", tabName = "distributionDB", icon = icon("database"))
                     #          ),
                     #
                              # menuItem("Fieldbook management",
                              #          #menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file")),
                              #          menuSubItem("Create fieldbook", tabName = "newFieldbookAgrofims", icon = icon("file")),
                              # 
                              #          menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                              #          menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser"))#,
                              #          # menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              # ),

                              # menuItem("Single Trial Analysis",
                              #          #menuSubItem("Single trial graph",tabName = "SingleChart", icon = icon("calculator")),
                              #          menuSubItem("Single report", tabName = "singleAnalysisReport", icon = icon("file-text-o"))#,
                              #          #menuSubItem("Genetic report", tabName = "geneticAnalysisReport", icon = icon("file-text-o"))
                              # 
                              #          #menuSubItem("Data Transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              # )#,
                     #          #
                     #          # menuItem("PVS Trial Analysis",
                     #          #          menuSubItem("PVS report", tabName = "singlePVS", icon = icon("calculator"))#,
                     #          #          #menuSubItem("PVS anova report",tabName = "singlePVS", icon = icon("calculator"))
                     #          # ),
                     #          #
                     #          # menuItem("MET Analysis",
                     #          #          #menuSubItem("MET analytical graph",tabName = "metAnalysisGraphs", icon = icon("calculator")),
                     #          #          menuSubItem("MET report", tabName = "metAnalysisReport",icon = icon("file-text-o"))#,
                     #          # ),
                     #          #
                     #          # menuItem("Index Selection",
                     #          #          menuSubItem("Elston index",tabName = "elstonIndex",icon = icon("file-text-o")),
                     #          #          #menuSubItem("Pesek-Baker index", tabName = "pesekIndex",icon = icon("indent")),
                     #          #          menuSubItem("Selection response", tabName = "selResponse",icon = icon("indent")),
                     #          #          menuSubItem("Drought indexes", tabName = "droughtIndex",icon = icon("indent"))
                     #
                     #
                     #          #,
                     #
                     # ),



                     # menuItem("Geographic Information", icon = icon("globe"),
                     #          menuSubItem("Locations table",tabName = "trialSitesTable",icon = icon("file-text-o")),
                     #          menuSubItem("Add trial sites",tabName = "trialSites", icon = icon("location-arrow"))
                     #          ),

                     # menuItem("Documentation",  icon = icon("copy")#,
                     #          # menuSubItem("HiDAP documents", tabName = "docHidap",icon = icon("file-text-o"))#,
                     # ),
                     #
                     # menuItem("Help",  icon = icon("question-circle")#,
                     #          # menuSubItem("Check updates", tabName = "updateHidap",icon = icon("refresh"))#,
                     # ),
                     #
                      #menuItem("About", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)

                   )

                   # br(),
                   # hr(),
                   # br(),


                   # )
  ),

  dashboardBody(

    tags$head(
      tags$script(src = "js.cookies.js")
    ),
    
    useShinyjs(),
    extendShinyjs(text = jscode),
    initStore("store", "shinyStore-haf", privKey), # Namespace must be unique to this application!
    #
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    # ),
    #for the top right text
    
    
    
    tags$head(tags$style(HTML(
      '.headerTopRight {
      line-height: 50px;
      text-align: right;
      font-family: "Arial";
      padding: 0 15px;
      color: black;
      font-size: 2vw;
          }
      @media (min-width: 1200px) {
        .headerTopRight {
          line-height: 0px;
          text-align: right;
          font-family: "Arial";
          padding: 0 15px;
          color: black;
          font-size: x-large;
      color: white;
        }
      }

      .sidebar-menu .treeview-menu {
          padding-left: 26px;
      }
    '))),

    tags$script(HTML('
                                            $(document).ready(function() {
       $("header").find("nav").append(\'<div class="headerTopRight"> <img src="http://bigdata.cgiar.org/wp-content/uploads/2017/04/CGIAR_PforBD-logo.png" width="105px"> </div>\');
       })
       ')
      ),



    tags$head(tags$style(HTML('
        /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #869910;
                              }
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #869910;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #aac309;
                              }
                              /* toggle button when hovered  */
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #869910;
                              }

                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #3C8DBC !important;
                              }






                              .box.box-solid.box-primary>.box-header {
                              color:#555;
                              font-weight: 800;
                              /* background:#aac309; */
                              background: -webkit-linear-gradient(top, #FAFAFA 0%, #E9E9E9 100%);
                              border: 1px solid #D5D5D5;
                              }

                              .box.box-solid.box-primary{
                              /* border-bottom-color:#aac309;
                              border-left-color:#aac309;
                              border-right-color:#aac309;
                              border-top-color:#aac309; */
                              border: 1px solid #D5D5D5;
                              }

                              .box.box-solid.box-primary>.box-header .btn, .box.box-solid.box-primary>.box-header a {
                                  color: #000;
                              }

                              .box {
                                  border-radius: 3px;
                              }

                              .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
                                  color: #555;
                                  font-weight: 800;
                              }






                              .box.box-solid.box-success>.box-header {
                              color:#fff;
                              background:#6c757d
                              }

                              .box.box-solid.box-success{
                              border-bottom-color:#6c757d;
                              border-left-color:#6c757d;
                              border-right-color:#6c757d;
                              border-top-color:#6c757d;
                              }



                              '
    ))),


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
      # tabItem(tabName = "login",  div( uiOutput("uiLogin"), uiOutput("pass"), style = 'max-width:700px;' )),
      # tabItem(tabName = "logoutTab",div(uiOutput("uiLogout"))),
      tabItem(tabName = "changePass",div(uiOutput("uiChangePass"), uiOutput("mssgChngPass"))),
      tabItem(tabName = "userProfile",div(uiOutput("uiUserProfile"))),

      tabItem(tabName = "register", div(uiOutput("registerMsg"), uiOutput("uiRegister") )),
      tabItem(tabName = "forgotPass", div(uiOutput("uiForgotPass"),uiOutput("pass") )),
      ##### end UI HIDAP NETOWORK ####


      #### MATERIAL LIST CREATION #########
      tabItem(tabName = "driveNet", uiOutput("driveScreen")),
      tabItem(tabName = "trialSite",  h1("Site information"), div(uiOutput("trialScreen"))),
      #### END MATERIAL LIST CREATION #####

      ###
      #Codigo Ivan Perez
      tabItem(tabName = "dashboard",

              #br(h2("Highly Interactive Data Analysis Platform")),
              #br( p(class = "text-muted", style="text-align:right", "Highly Interactive Data Analysis Platform")),

              # br(),
              # br(),
              #img(src="potato.jpg", width = "100%"),-
              # img(src="about.jpg", width = "100%"),
              # img(src="images/banner.jpg", width = "1640px", height="390px"),
              img(src="images/banner_agrofims_test2.jpg", width = "100%", height="100%"),

              br(),
              br(),

              h2("HIDAP AgroFIMS v0.0.18"),
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
                  # div(img(src="images/BIG_DATA.png", height = "auto", width="auto"), style=" text-align: center;")
                  div(img(src="images/BIG_DATA.png", width = "190px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",                  solidHeader = TRUE,
                  # div(img(src="images/CIAT.jpg",height = "auto", width="90%"), style="text-align: center;")
                  div(img(src="images/CIAT.jpg", width = "190px"), style="text-align: center;")
                ),


                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  br(),
                  # div(img(src="CIPlogo_RGB.png", height = "auto", width="90%"), style="margin-top: -20px;text-align: center;")
                  div(img(src="CIPlogo_RGB.png", width = "180px"), style="margin-top: -20px; text-align: center;")
                ),

                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  # div(img(src="images/IFPRI.jpg", height = "auto", width="80%"), style="text-align: center;")
                  div(img(src="images/IFPRI.jpg", width = "170px"), style="text-align: center;")
                ),
                box(
                  width = 2, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  # div(img(src="images/Bioversity.jpg", height = "auto", width="70%"), style="margin-top: -20px; text-align: center;")
                  div(img(src="images/Bioversity.jpg", width = "130px"), style="margin-top: -10px; text-align: center;")
                )
              ),

              br(),
              br(),
              br()
      ),

      # Design Experiments Module ----------------------------------------------------
      fbdesign::ui_fieldbook_agrofims(name = "newFieldbookAgrofims"),
      #fbdesign::ui_fieldbook(name = "newFieldbook"),

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
        #fbmlist::managerlist_ui_agrofims(name ="manageListAgrofims"),
        #fbmlist::generate_ui_agrofims(name = "generateListAgrofims")

      # fbmlist::managerlist_ui(name = "manageList"),
      # fbmlist::createlist_ui(name = "createList"),
      # fbmlist::parent_ui(name = "parentList"),
      # fbmlist::distribution_ui(name = "distributionDB"),

      # #brapps::fbasingle_ui("SingleChart"),
      #
       fbanalysis::single_hdagrofims_ui(name="singleAnalysisReportAgrofims")#,
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
              #tags$div(id = "test", img(src="cc_by.png"), "2018 International Potato Center. Av La Molina 1895, La Molina - Peru.")
              tags$div(id = "test", "Powered by HIDAP")
            ),
            href="#"
          ),
          tags$style("footer {background-color: #222d32;height: 40px;position: absolute;bottom: 0;width: 100%;}"),
          tags$style("#test {color: #fff;padding-top: 9px;}")
        )
      )
    )

  )
)


############################################################

sv <- function(input, output,  session) ({
  
  observe({
    js$getcookie()
    if (!is.null(input$jscookie_user) &&
        input$jscookie_user != "") {
      checkCredentials(input$jscookie_user, input$jscookie_pass)
    }
    
  })
  
  session$userData$logged <- F
  session$userData$userId <- NULL

  USER <- reactiveValues(Logged = FALSE, username = NULL, id = NULL, fname = NULL, lname = NULL, org=NULL, country=NULL)

  useShinyjs()
  extendShinyjs(text = jscode)



  # USER <- reactiveValues(Logged = FALSE, username = NULL, id = NULL, fname = NULL, lname = NULL, org=NULL, country=NULL)
  dt_myMaterialList <- reactiveValues()

  source("www/loginModule/userMenuUi.R",local = TRUE)
  source("www/driveModule/drive.R", local = TRUE)
  source("www/sitesModule/sites.R", local = TRUE)
  source("www/loginModule/login.R", local = TRUE)


  values <- shiny::reactiveValues(crop = "sweetpotato", amode = "brapi")

  withProgress(message = 'Loading HiDAP', value = 0, {

  incProgress(1/25, detail = paste("..."))

  # fbcheck::fbcheck_server(input, output, session, values)

  #fbmlist::server_managerlist(input, output, session, values)
  fbmlist::server_managerlist_agrofims(input, output, session, values)
  #fbmlist::server_generate(input, output, session, values)
  fbmlist::server_generate_agrofims(input, output, session, values)
  #fbmlist::server_createlist(input, output, session, values)
  #fbmlist::server_parentlist(input, output, session, values)
  #fbmlist::server_distribution(input,output,session, values)

  incProgress(2/25, detail = paste("..."))


  fbdesign::server_design_agrofims(input, output, session, values)
  # fbdesign::server_design_big(input, output, session, values)
  # fbopenbooks::fbopenbooks_server(input, output, session, values)
  fbanalysis::single_hdagrofims_server(input, output, session, values)
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




     incProgress(25/25, detail = paste("..."))
  #
   }) #end shiny progress bar
  #
  # ################## End NETWORK ######################################################################################

})

shinyApp(ui, sv)
