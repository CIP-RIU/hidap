tab_resource_dictionary <- function(){
  tabItem(tabName = "resource_dictionary",
          fluidRow(
            box(width = 12, solidHeader = TRUE, status = "warning",
                selectInput("dictionary_crop", "Select a crop:",
                            unique(get_crop_table()$crop_name) )),

            box(width = 12,
                title = "Dictionary configuration",
                rHandsontableOutput("hot_dictionary", height = 600)
            )
          )
  )
}
