tab_resource_module<- function(){
  tabItem(tabName = "resource_module",
          fluidRow(
            box(width = 12, solidHeader = TRUE, status = "warning",
                selectInput("module_crop", "Select a crop:",
                            unique(get_crop_table()$crop_name) ),

                uiOutput("module_list")
                ),

            box(width = 12, height = 1200,
                title = "Module configuration",
                verbatimTextOutput("module_var_list")
            )
          )
  )
}
