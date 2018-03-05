#library(d3heatmap)
#library(shinysky)
library(data.table)
#library(shinyTree)

#library(doBy)
library(tidyr)
library(DT)
library(brapi)
library(brapps)
library(agricolae)
library(dplyr)
library(openxlsx)
#library(fbmet)
#library(fbhelp)
#library(fbdesign)
#library(rhandsontable)
library(shinydashboard)
#library(date)

library(purrr)
#library(shinyURL)
#library(qtlcharts)
#library(leaflet)
#library(withr)
library(st4gi)
library(tibble)
library(knitr)
library(readxl)
#library(countrycode)
#library(fbsites)
#library(fbmlist)
#library(fbmet)

#library(fbcheck)
#library(fbmlist)
#library(countrycode)
#library(shinyjs)
library(DBI)
library(RMySQL)
library(spsurvey)
library(foreign)
library(tools)
library(stringr)
library(shinyBS)
#library(fbdesign)
#library(fbopenbooks)
library(fbanalysis)
#library(traittools)
#library(sbformula)
library(pepa)
library(shinyFiles)
library(rlist)
#library(rprojroot)
#library(factoextra)
#library(ggrepel)

#library(fbdocs)
#library(geneticdsg)

#package fbupdate
#library(remotes)
#library(fbupdate)
library(tibble)
library(shinyjs)
library(brapi)


library(ggplot2)
#library(st4gi)
#library(pepa)
library(readxl)
library(eda4plant)
library(st4gi)
library(GGally)
library(ggrepel)
library(brapi)
library(dplyr)


# init default data: TODO make a function with better logic checking whats new
# from fbglobal get_base_dir

#dd = system.file("xdata/Default", package = "fbglobal")
#file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)

# remove dependency on RTools by pointing to a zip.exe. NOTE: needs to be installed
# into HIDAP working dir by installer
#Sys.setenv("R_ZIPCMD" = file.path(Sys.getenv("HIDAP_HOME"), "zip.exe"))


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "HIDAP", titleWidth = "250px"
                  #tags$script(HTML("$('body').addClass('sidebar-mini');"))
  ),#end Header
  dashboardSidebar(width = "250px",
                   
                   #div(style="margin-right: auto;",img(src = "Logo1.png", width = "250")),
                   br(),
                   div(img(src="hidapicon.png", width = "150px"), style="text-align: center;"),
                   
                   #sidebarSearchForm(label = "Enter a word", "searchText", "searchButton"),
                   sidebarMenu(
                     id = "tabs",
                     # menuItem("Phenotype tool", icon = icon("th-list"),
                     menuItem("Data Analysis", icon = icon("th-list"),
                              
                              # menuItem("Material Management",
                              #          menuSubItem("Manage list", tabName = "manageList", icon = icon("table")),
                              #          menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                              #          menuSubItem("Family list", tabName = "createList", icon = icon("list-alt")) ,
                              #          #menuSubItem("Parental list", tabName = "parentList", icon = icon("list-alt")),
                              #          menuSubItem("Distribution Data", tabName = "distributionDB", icon = icon("database"))
                              # ),
                              
                              # menuItem("Fieldbook management",
                              #          menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file")),
                              #          menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                              #          menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                              #          menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              # ),
                              
                              menuItem("Single Trial Analysis",
                                       #menuSubItem("Single trial graph",tabName = "SingleChart", icon = icon("calculator")),
                                       menuSubItem("Single report", tabName = "singleAnalysisReport_sbase", icon = icon("file-text-o"))#,
                                       #menuSubItem("Genetic report", tabName = "geneticAnalysisReport", icon = icon("file-text-o"))
                                       
                                       #menuSubItem("Data Transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              ),
                              
                              # menuItem("PVS Trial Analysis",
                              #          menuSubItem("PVS report", tabName = "singlePVS", icon = icon("calculator"))#,
                              #          #menuSubItem("PVS anova report",tabName = "singlePVS", icon = icon("calculator"))
                              # ),
                              # 
                              menuItem("MET Analysis",
                                       #menuSubItem("MET analytical graph",tabName = "metAnalysisGraphs", icon = icon("calculator")),
                                       menuSubItem("MET report", tabName = "metAnalysisReport_sbase",icon = icon("file-text-o"))#,
                              ),
                              
                              menuItem("Graph",
                                       #menuSubItem("MET analytical graph",tabName = "metAnalysisGraphs", icon = icon("calculator")),
                                       menuSubItem("Exploratory Graphics", tabName = "edaGraph_sbase",icon = icon("file-text-o"))#,
                              ),
                              
                              
                              menuItem("Index Selection",
                                       menuSubItem("Elston index",tabName = "elstonIndex",icon = icon("file-text-o")),
                                       #menuSubItem("Pesek-Baker index", tabName = "pesekIndex",icon = icon("indent")),
                                       menuSubItem("Selection response", tabName = "selResponse",icon = icon("indent"))
                              )#,
                              
                     ),
                     
                     # menuItem("Geographic Information", icon = icon("globe"),
                     #          menuSubItem("Add trial sites",tabName = "trialSites", icon = icon("location-arrow")),
                     #          menuSubItem("Locations table",tabName = "trialSitesTable",icon = icon("file-text-o"))
                     # ),
                     
                     # menuItem("Documentation",  icon = icon("book"),
                     #          menuSubItem("HIDAP documents", tabName = "docHidap",icon = icon("file-text-o"))#,
                     # ),
                     
                     # menuItem("Help",  icon = icon("refresh"),
                     #          menuSubItem("Check updates", tabName = "updateHidap",icon = icon("refresh"))#,
                     # ),
                     # 
                     
                     menuItem("About", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)#,
                     
                   )
                   # )
  ),
  
  dashboardBody(
    #
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    ),
    
    includeCSS("www/custom.css"),
    
    tabItems(
      
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
              
              h3("HIDAP-SweetPotato Base"),
              p(class = "text-muted", style="text-align:justify",
                #paste("HiDAP is a Highly Interactive Data Analysis Platform originally meant to support clonal crop breeders at the <a href='http://www.cipotato.org' target='_new'>International Potato Center</a>. It is part of a continuous institutional effort to improve data collection, data quality, data analysis and open access publication. The recent iteration simultaneously also represents efforts to unify best practices from experiences in breeding data management of over 10 years, specifically with DataCollector and CloneSelector for potato and sweetpotato breeding, to address new demands for open access publishing and continue to improve integration with both corporate and community databases (such as biomart and sweetpotatobase) and platforms such as the <a href='https://research.cip.cgiar.org/gtdms/' target='_new'> Global Trial Data Management System (GTDMS)</a> at CIP. </br> One of the main new characteristics of the current software development platform established over the last two years is the web-based interface which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops. Key features include support for data capture, creation of field books, upload field books from and to accudatalogger, data access from breeding databases (e.g., <a href = 'http://germplasmdb.cip.cgiar.org/' target='_new'>CIP BioMart</a>, <a href='http://www.sweetpotatobase.org' target='_new'>sweetpotatobase</a> via <a href='http://docs.brapi.apiary.io/' target='_new'>breeding API</a>), data quality checks, single and multi-environmental data analysis, selection indices, and report generations. For users of DataCollector or CloneSelector many of the features are known but have been improved upon. Novel features include list management of breeding families, connection with the institutional pedigree database, interactive and linked graphs as well as reproducible reports. With the first full release by end of November 2016 we will include all characteristics from both DataCollector and CloneSelector. HIDAP, with additional support from <a href='https://sweetpotatogenomics.cals.ncsu.edu/' target='_new'>GT4SP</a>, <a href='http://www.rtb.cgiar.org/' target='_new'>RTB</a>, USAID, and <a href='http://cipotato.org/research/partnerships-and-special-projects/sasha-program/' target='_new'>SASHA</a>, is aimed to support the broader research community working on all aspects with primary focus on breeding, genetics, biotechnology, physiology and agronomy.")
                shiny::includeHTML("www/about_hidap.txt")
              ),
              
              
              br(),
              br(),
              
              fluidRow(
                
                box(
                  width = 3, style="background-color = #fff", height = "128px",
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
                  width = 3, style="background-color = #fff", height = "128px",
                  solidHeader = TRUE,
                  div(img(src="BTI.png", height = "108px"), style="text-align: center;")
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
      
      tabItem(tabName = "integration",
              fluidRow(
                box(
                  title = "CIPFBS report", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  tags$iframe(src = "http://176.34.251.32/cipfieldbookstorage_dev/protected/extensions/grid/demo.php",
                              seamless=NA, width = "100%", height = "800px"
                  )
                )
              ),
              
              br(),
              br(),
              br()
      ),
      #Fin codigo Ivan Perez
      ###
      
      # Design Experiments Module ----------------------------------------------------
      #fbdesign::ui_fieldbook(name = "newFieldbook"),
      
      # Data Quality and Check Fieldbook Module  ----------------------------------------------------
      #fbcheck::fbcheck_ui(name= "checkFieldbook"),
      
      # Fieldbook Manager Module ----------------------------------------------------
      #fbopenbooks::fbopenbooks_ui(name="openFieldbook"),
      
      # Data Transformation
      #fbanalysis::dtr_ui(name = "singleAnalysisTrans"),
      
      
      # Material List Module ----------------------------------------------------
      
      #fbmlist::generate_ui(name = "generateList"),
      #fbmlist::managerlist_ui(name = "manageList"),
      #fbmlist::createlist_ui(name = "createList"),
      #fbmlist::parent_ui(name = "parentList"),
      #fbmlist::distribution_ui(name = "distributionDB"),
      
      #brapps::fbasingle_ui("SingleChart"),
      
      
      
      #fbanalysis::single_ui(name="singleAnalysisReport"),
      
      
      fbanalysis::single_sbase_ui(name="singleAnalysisReport_sbase"),
      #fbanalysis::genetic_ui(name="geneticAnalysisReport"),
      fbanalysis::met_sbase_ui(name="metAnalysisReport_sbase"),
      #fbanalysis::met_ui(name="metAnalysisReport"),
      #fbmet::met_ui("metAnalysisGraphs"),
      
      
      # fbsites::addsite_ui(name = "trialSites"),
      # fbsites::ui_site(name ="trialSitesTable"),
      # 
      
      eda4plant::edaplant_sbase_ui(name = "edaGraph_sbase"),
      
      fbanalysis::elston_ui_sbase(name="elstonIndex"),
      #fbanalysis::ui_pvs(name = "singlePVS"),
      
      #fbdocs::fbdocs_ui(name = "docHidap") ,
      
      #Hidap Update Module
      # fbupdate::fbupdate_ui(name = "updateHidap"),
      
      
      #fbmlist::parent_ui(name = "parentList"),
      
      #fbanalysis::pbaker_ui(name="pesekIndex"),
      
      brapps::rts_ui("selResponse"),
      
      tabItem(tabName = "analysis",
              h2("Analysis"),
              p(class = "text-muted",
                paste("Under construction...")
              )
      )
    ) , #end of TabSetPanel
    
    tags$div(
      fluidRow(
        tags$footer(
          a(
            list(
              tags$div(id = "test", img(src="cc_by.png"), "2016 International Potato Center. Av La Molina 1895, La Molina - Peru.")
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
  
  #
  #
  #   try({
  #   brapi_con("sweetpotato", "sgn:eggplant@sweetpotatobase-test.sgn.cornell.edu",
  #             80, "rsimon16",
  #             "sweetpotato")
  #   })
  
  #shinyURL.server()
  
  #fbcheck::fbcheck_server(input, output, session, values)
  
  #fbmlist::server_managerlist(input, output, session, values)
  #fbmlist::server_generate(input, output, session, values)
  #fbmlist::server_createlist(input, output, session, values)
  #fbmlist::server_parentlist(input, output, session, values)
  #fbmlist::server_distribution(input,output,session, values)
  
  #fbdesign::server_design(input, output, session, values)
  #fbdesign::server_design_big(input, output, session, values)
  #fbopenbooks::fbopenbooks_server(input, output, session, values)
  fbanalysis::single_server_base(input, output, session, values)
  #fbanalysis::dtr_server(input, output, session, values)
  
  fbanalysis::met_server_sbase(input, output, session, values)
  
  fbanalysis::elston_server_sbase(input, output, session, values)
  #fbanalysis::pbaker_server(input, output, session, values)
  
  
  eda4plant::edaplant_sbase_server(input,output, session ,values)
  
  #fbanalysis::pvs_server(input, output, session, values)
  #fbanalysis::genetic_server(input, output, session, values)
  #fbanalysis::pvs_anova_server(input, output, session, values)
  
  #fbdocs::fbdocs_server(input, output, session, values)
  
  #fbupdate::fbupdate_server(input, output, session, values = values)
  #fbsites::server_addsite(input, output, session, values = values)
  #fbsites::server_site(input, output, session, values = values)
  
  #brapps::fieldbook_analysis(input, output, session, values)
  #brapps::locations(input, output, session, values)
  #fbmet::met_sv(input, output, session, values)
  brapps::rts_sv(input, output, session, values)
  
  # drat::addRepo("c5sire")
  # res = eventReactive(input$about_update, {
  #   cat("Ok")
  #   if(brapi::can_internet()){
  #   withProgress({
  #   try({
  #     update.packages(ask = FALSE)
  #   })
  #   }, message = "Checking for updates ...")
  #   }
  # })
  #
  #
  # })
  
})

shinyApp(ui, sv)
