tab_phenotype_analysis <- function(){
  tabItem(tabName = "fieldbook_analysis",
     fluidRow(
            tabBox(width = 12, #collapsible = TRUE,
                #title = "Field trial",
                title = textOutput("fb_fieldbook_title"),
                tabPanel("Fieldbook",{

                  rHandsontableOutput("hotFieldbook", height = 400)
                }),
                tabPanel("Minimal",{
                  rHandsontableOutput("hotMinimal", height = 400)
                }),
                tabPanel("Installation",{
                  rHandsontableOutput("hotInstallation", height = 400)
                }),
                tabPanel("Material List",{
                  rHandsontableOutput("hotMaterialList", height = 400)
                }),
                tabPanel("Crop Management",{
                  rHandsontableOutput("hotCropManagement", height = 400)
                }),
                tabPanel("Variable List",{
                  rHandsontableOutput("hotVariableList", height = 400)
                }),
                tabPanel("Soil Analysis",{
                  rHandsontableOutput("hotSoilAnalysis", height = 400)
                }),
                tabPanel("Weather Data",{
                  rHandsontableOutput("hotWeatherData", height = 400)
                })
            )


          ),
          fluidRow(
            tabBox(width = 12,


                   tabPanel(title = "Fieldmap",
                            textOutput("fb_fieldmap_title"),
                            d3heatmap::d3heatmapOutput("fb_fieldmap_check")
                   ),
                   # tabPanel(title = "Trait histogram",
                   #          plotOutput("fb_histogram_check")
                   # ),
                  tabPanel(title = "Correlations (interactive)",
                           qtlcharts::iplotCorr_output('vcor_output')
                  )
                  # ,
                  # tabPanel(title = "Reports",
                  #          htmlOutput("fb_report")
                  #
                  # )

            )
          )
  )
}
