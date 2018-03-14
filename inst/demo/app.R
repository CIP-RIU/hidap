#library(shinysky)
#library(data.table)
#library(shinyTree)
#library(brapi)
library(brapps)
library(shinyFiles)
#library(DT)
# library(agricolae)
# library(dplyr)
# library(openxlsx)
#library(fbmet)
#library(fbhelp)
#library(fbdesign)
library(rhandsontable)
library(shinydashboard)
library(d3heatmap)
#library(shinyURL)
# library(qtlcharts)
#library(leaflet)
# library(dplyr)
# library(withr)
# library(DT)
# library(st4gi)
# library(tibble)
# library(knitr)
# library(readxl)
# library(countrycode)
# library(fbsites)
# library(fbmlist)

#library(fbcheck)
#library(fbmlist)
#library(countrycode)
library(shinyjs)
# library(DBI)
# library(RMySQL)
# library(spsurvey)
# library(foreign)
# library(tools)
library(stringr)
library(shinyBS)
# library(fbdesign)
# library(fbopenbooks)
# library(fbanalysis)
 # library(traittools)
 # library(sbformula)
# library(pepa)

# init default data: TODO make a function with better logic checking whats new
# from fbglobal get_base_dir
dd = system.file("xdata/Demo", package = "fbglobal")
file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)
dd = system.file("xdata/Default", package = "fbglobal")
file.copy(from = dd, to = fbglobal::get_base_dir(""), recursive = TRUE)



ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP",
                                    dropdownMenuOutput("messageMenu")
                    ),

                    dashboardSidebar(
                      sidebarMenu(id = "menu",
                                  # menuItem("Material management", icon = icon("list"),
                                  #          menuSubItem("Manage lists", icon = icon("list-ol"),
                                  #                      tabName = "phe_ml_manager"),
                                  #
                                  #          menuSubItem("Clone list", icon = icon("paste"),
                                  #                      tabName = "phe_ml_clone"),
                                  #
                                  #          menuSubItem("Family list", icon = icon("list"),
                                  #                      tabName = "phe_ml_family")
                                  #
                                  #
                                  #          ),

                                  menuItem("Phenotype", icon = icon("leaf"),
#
#                                            menuSubItem("New fieldbook", icon = icon("file"),
#                                                        tabName = "phe_fb_new"),
#
#                                            menuSubItem("Open fieldbook", icon = icon("file-o"),
#                                                        tabName = "phe_fb_open"),

                                           # menuSubItem("Check fieldbook", icon = icon("eraser"),
                                           #             tabName = "phe_fb_check"),

                                           menuSubItem("Single trial analytical graphs",
                                                       tabName = "phe_dashboard", icon = icon("calculator")),

                                           # menuSubItem("Single trial report",
                                           #             tabName = "phe_set_report", icon = icon("calculator")),

                                           menuSubItem("MET analytical graphs",
                                                       tabName = "phe_met", icon = icon("calculator")),

                                           # menuSubItem("MET report",
                                           #             tabName = "phe_met_report", icon = icon("calculator")),
                                           #
                                           # menuSubItem("ELston index",
                                           #             tabName = "phe_elston", icon = icon("calculator")),
                                           #
                                           # menuSubItem("Pesek Baker index",
                                           #             tabName = "phe_pesek", icon = icon("calculator")),

                                           menuSubItem("Selection response",
                                                       tabName = "phe_rts", icon = icon("calculator"))

                                           #numericInput("fbaInput", "Fieldbook ID", 142, 1, 9999)


                                  ),

                                  # menuItem("Environment", tabName = "env_dashboard", icon = icon("globe")
                                  # )
                                  # ,
                                  menuItem("About", tabName = "about_dashboard", icon = icon("dashboard"),
                                           selected = TRUE,
                                           badgeLabel = "new", badgeColor = "green"),


                                  HTML("<div style='display:none'>"),
                                  #shinyURL.ui(label = "",width=0, copyURL = F, tinyURL = F),
                                  #shinyURL.ui("URL", tinyURL = F)
                                  HTML("</div>")
)

),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
                      ),
                      tabItems(
                        tabItem(tabName = "about_dashboard",
                                hidap::about("Highly Interactive Data Analysis Platform for Clonal Crop Breeding")
                        ),
                        # tabItem(tabName = "env_dashboard",
                        #         h3("Temporarily disabled")
                        #         #brapps::locations_ui("Trial Location Explorer")
                        # ),
                        # tabItem(tabName = "phe_fb_new",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbdesign::ui_fieldbook(name = "phe_fb_new"))
                        #         ))),
                        # tabItem(tabName = "phe_fb_open",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbopenbooks::fbopenbooks_ui(name = "phe_fb_new"))
                        #         ))),
                        # tabItem(tabName = "phe_fb_check",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbcheck::fbcheck_ui(name = "phe_preprocess"))
                        #         ))),
                        # tabItem(tabName = "phe_ml_clone",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbmlist::generate_ui("phe_ml_clone"))
                        #         ))),
                        # tabItem(tabName = "phe_ml_manager",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbmlist::managerlist_ui(name = "phe_ml_manager"))
                        #         ))),
                        # tabItem(tabName = "phe_ml_family",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbmlist::createlist_ui(name = "phe_ml_family"))
                        #         ))),
                        #


                        tabItem(tabName = "phe_met",
                                fluidRow((
                                  column(width = 12,
                                         fbmet::met_ui("Multi-Environment Analytical Graphs"))
                                ))),

                        # tabItem(tabName = "phe_set_report",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbanalysis::single_ui("phe_set"))
                        #         ))),

                        # tabItem(tabName = "phe_met_report",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbanalysis::met_ui("phe_met"))
                        #         ))),
                        #
                        #
                        # tabItem(tabName = "phe_elston",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbanalysis::elston_ui("phe_elston"))
                        #         ))),
                        # tabItem(tabName = "phe_pesek",
                        #         fluidRow((
                        #           column(width = 12,
                        #                  fbanalysis::pbaker_ui("phe_pbaker"))
                        #         ))),

                        tabItem(tabName = "phe_rts",
                                fluidRow((
                                  column(width = 12,
                                         brapps::rts_ui("phe_rts"))
                                ))),


                        tabItem(tabName = "phe_dashboard",
                                #p("Temporarily disabled")
                                brapps::fbasingle_ui("Single-Environment Trial Explorer")
                        )

                      )        )
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

  # fbcheck::fbcheck_server(input, output, session, values)

  # fbmlist::server_managerlist(input, output, session, values)
  # fbmlist::server_generate(input, output, session, values)
  # fbmlist::server_createlist(input, output, session, values)
  #
  # fbdesign::server_design(input, output, session, values)
  # fbdesign::server_design_big(input, output, session, values)
  # fbopenbooks::fbopenbooks_server(input, output, session, values)
  # fbanalysis::single_server(input, output, session, values)

  # fbanalysis::met_server(input, output, session, values)
  #
  # fbanalysis::elston_server(input, output, session, values)
  # fbanalysis::pbaker_server(input, output, session, values)

  brapps::fieldbook_analysis(input, output, session, values)
  #brapps::locations(input, output, session, values)
  fbmet::met_sv(input, output, session, values)
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









