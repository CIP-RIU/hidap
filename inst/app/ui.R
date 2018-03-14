# # add hidap repository to local path
# options(repos = c(hidap = "https://c5sire.github.io/hd", getOption("repos")))
#
# # set HIDAP_HOME environment variable
# Sys.setenv(HIDAP_HOME = "D:/HIDAP/")
#
# # set local library for hidap libs
# .libPaths("D:/HIDAP/libs")



library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)
library(date)
library(agricolae)
library(doBy)


library(shinydashboard)
library(rhandsontable)
library(shinyTree)
library(shinyFiles)
library(shinyBS)
library(leaflet)


source("R/tab_resource_dashboard.R")
source("R/tab_environment_dashboard.R")
source("R/tab_phenotype_analysis.R")


dashboardPage(skin = "yellow",

  dashboardHeader(title = "HIDAP4RTB",
      dropdownMenuOutput("messageMenu")

  ),

  dashboardSidebar(
    div(style="margin-right: auto;",img(src = "Logo1.png", width = "230")),
    sidebarMenu(id = "menu",
      # menuItem("Summary",
      #  menuSubItem("Summary Dashboard", tabName = "dashboard_summary", icon = icon("dashboard"),
      #     selected = TRUE)
      # ),
       menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "new", badgeColor = "green"),
       menuItem("Data tools",
        #menuSubItem("Data sources", tabName = "dashboard_source"),
        #menuSubItem("Data import", tabName = "dashboard_import"),
        menuSubItem("Data checks", tabName = "dashboard_check")
        ) ,
       menuItem("Phenotype tools" ,
        # menuSubItem("Phenotype Dashboard", tabName = "dashboard_phenotype", icon = icon("dashboard")
        #          ),
        menuSubItem("Analysis", icon = icon("book"), tabName = "fieldbook_analysis"),
        conditionalPanel(
          "input.menu == 'fieldbook_analysis'",
          selectInput("fb_analysis_crop", "Select a crop:",
                      choices = fbcrops::get_crop_table()$crop_name, selected = "sweetpotato"),
          uiOutput("fieldbook_list"),
          uiOutput("fb_def_reps"),
          uiOutput("fb_def_block"),
          uiOutput("fb_def_plot"),
          uiOutput("fb_def_genotype"),
          uiOutput("fb_def_variables")
          #,
          # selectInput("fb_analysis", "Select analysis:",
          #             choices = c("ANOVA" = "aov")),
          # HTML("<center>"),
          # shiny::actionButton("butDoPhAnalysis", "Analyze!", inline = TRUE),
          # HTML("</center>")
        )


        # menuSubItem("New fieldbok", icon = shiny::icon("star"),
        #              tabName = "phenotype_fieldbook_design")
        # ,
        # fbdesign::ui_fieldbook_params()
        #,
        # menuSubItem("Import fieldbook", icon = icon("file-excel-o"), tabName = "fbImport"),
        # conditionalPanel(
        #   "input.menu == 'fbImport'",
        #   HTML("<center>"),
        #   shinyFiles::shinyFilesButton('fb_file', 'File select', 'Please select a file', FALSE),
        #   verbatimTextOutput("fb_file_sel"),
        #   HTML("</center>")
        # ),

 #       menuSubItem("Check fieldbook", icon = icon("check-square"), tabName = "fbCheck"),
#        menuSubItem("Export fieldbook", icon = icon("file-excel-o"), tabName = "fbExport")
 #       menuSubItem("Import images", icon = icon("photo"), tabName = "fbFotoImport"),



        # ,
        # menuSubItem("Catalogues", icon = icon("book"), tabName = "fbCatalog")
      ),
      # menuItem("Genotype",
      #  menuSubItem("Genotype cross marker", tabName = "genotype_cross_marker", icon = icon("dashboard"))
      # ,
      #  conditionalPanel(
      #    "input.menu == 'dashboard_genotype'",
      #    selectInput("period", "Period", 1:10)
      #  )
      # ),
      menuItem("Environment",
       menuSubItem("Environment Dashboard", tabName = "dashboard_environment", icon = icon("dashboard"))
      )
#,

      # menuItem("Integration",
      #  menuSubItem("MET", tabName = "integration_MET", icon = icon("dashboard")),
      #  HTML("<center>"),
      #  shiny::actionButton("butDoMETAnalysis", "Analyze MET!", inline = TRUE),
      #  HTML("</center>"),
      #  #menuSubItem("QTL mapping", tabName = "integration_qtl_mapping", icon = icon("dashboard")),
      #  menuSubItem("QTL analyses", tabName = "integration_qtl", icon = icon("dashboard")),
      #  menuSubItem("Genomic selection", tabName = "integration_gs", icon = icon("dashboard")),
      #  menuSubItem("Breeding program", tabName = "integration_breeding", icon = icon("dashboard"))
      #
      # ),

 # menuItem("Supporting information",
 #          menuSubItem("Dashboard", icon = icon("dashboard"), tabName = "resource_dashboard"
 #          ),
 #          menuSubItem("Crops", icon = icon("leaf"), tabName = "resource_crop"),
 #          menuSubItem("Breeding programs", icon = icon("crop"), tabName = "resource_program"),
 #          menuSubItem("Breeding program stage", icon = icon("crop"), tabName = "resource_program_stage"),
 #          menuSubItem("Plant materials", icon = icon("star"),
 #                      tabName = "resource_material_list"),
 #          fbmaterials::ui_material_list_params(),
 #          menuSubItem("Sites", icon = icon("location-arrow"), tabName = "resource_site"),
 #          menuSubItem("Data dictionary", icon = icon("book"), tabName = "resource_dictionary"),
 #          cropont::ui_dictionary_params(),
 #          menuSubItem("Modules for dictionaries", icon = icon("book"),
 #                      tabName = "resource_modules"),
 #          fbmodule::ui_module_params()
 #
 # )
 # ,
 #    menuItem( "Sharing",
 #      menuSubItem("Sharing Dashboard", tabName = "sharing_dashboard", icon = icon("dashboard")),
 #      menuSubItem("Index data citations", tabName = "sharing_citations", icon = icon("dashboard")),
 #      menuSubItem("Data deposit", tabName = "sharing_deposit", icon = icon("dashboard")),
 #      menuSubItem("Social networks", tabName = "sharing_social", icon = icon("dashboard"))
 #    )
    # ,
    # menuItem("Help",
    #    menuSubItem("Documentation", tabName = "help_documentation", icon = icon("dashboard")),
    #    menuSubItem("Tasks", tabName = "help_tasks", icon = icon("dashboard")),
    #    menuSubItem("Tutorials", tabName = "help_tutorials", icon = icon("dashboard"))
    # )
     )

  ),
  dashboardBody(height = 900,

    tabItems(
      tabItem(tabName = "dashboard",
              h2("Hight Interactive Data Analysis Platform for clonal plant breeding"),


              br(),

              img(src="potato2.png", width = "100%"),

              br(),
              br(),

              "HIDAP v1.0 build 3 [30/4/2016]",
              p(class = "text-muted", style="text-align:justify",
                paste("HIDAP is a tool designed to help breeders of clonal plants (likw potato and sweetpotato) carry out field trial planning, documentation, analysis and reporting")
              ),

              tags$div(style = "color: #9b9691;float: right;", "International Potato Center (CIP)"),

              br(),
              tags$div(style = "horizontal-align: middle;", img(src="gt4sp.png", width = 100)),
              br(),
              br()
      ),
      fbcollect::tab_dataSource(),
      fbcheck::fbcheck_ui(name = "dashboard_check"),
      #tabItem(tabName = "dashboard_source",tabBox(id = "x")),

      shinydashboard::tabItem(tabName = "genotype_cross_marker",
      shiny::fluidRow(
        shinydashboard::box(width = 12,
                            title = "Cross marker scores",
                            rhandsontable::rHandsontableOutput("hot_cross_marker", height = 600)
        ),
        shiny::fluidRow(
          shinydashboard::box(width = 12, title = "Report")
        )
      )
      )
      ,

      tab_environment_dashboard(),

      shinydashboard::tabItem(tabName = "integration_qtl",
      shiny::fluidRow(
        shinydashboard::tabBox(id = "qtl_tab", width = 12,
           shiny::tabPanel("Map",
                           qtlcharts::iplotMap_output('qtl_map_output')
           ),
            shiny::tabPanel("Interactive LOD curve",
                               qtlcharts::iplotScanone_output('lod_output')
                               ),
            shiny::tabPanel("Gene correlation",
                            qtlcharts::iplotCorr_output('qtl_output')
            ),
            shiny::tabPanel("RF",
                           qtlcharts::iplotRF_output('rf_output')
            ),
            shiny::tabPanel("QTL effects over time",
                           qtlcharts::iplotMScanone_output('qtl_time_output')
           )
        )
      )
      ),
      #
      tab_resource_dashboard(),
      fbcrops::ui_crop(),
      fbprogram::ui_program(),
      fbprstages::ui_program_stage(),
      fbmaterials::ui_material_list(),
      fbsites::ui_site(),
      cropont::ui_dictionary(),
      fbmodule::ui_module(),
      fbdesign::ui_fieldbook(),
      tab_phenotype_analysis()
    )
  )
)
