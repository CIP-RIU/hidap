
tab_environment_dashboard <- function(){
  tabItem(tabName = "dashboard_environment",
          fluidRow(
            column(width = 8,
                   box(width = NULL,
                       collapsible = TRUE,
                       leafletOutput("map")
                   ),
                   box(width = NULL,
                       title = "Location summary report",
                       #actionButton("locs_report_button", "Do report!"),
                       htmlOutput("rep_loc_docs"),
                       #htmlOutput("rep_loc_docx"),
                       htmlOutput("rep_loc"))

            )
            ,
            column(width = 4,
                   tabBox(width = NULL, title = "Site",
                      tabPanel("Info",
                          htmlOutput("site_desc")
                      ), tabPanel("Fieldtrials",
                          htmlOutput("site_fieldtrials")
                                  ),
                          tabPanel("Genotypes",
                          htmlOutput("site_genotypes")
                                   )
                    ),

                   box(width = NULL,
                       plotOutput("dot_yield"))

            )
          )
  )
}
