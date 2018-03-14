tab_resource_dashboard <- function(){
  tabItem(tabName = "resource_dashboard",
          fluidRow(
            valueBox(nrow(fbcrops::get_crop_table()), "Crops", icon = icon("leaf")),
            valueBox(nrow(fbprogram::get_program_table()), "Programs", icon = icon("crop")),
            valueBox(fbmaterials::get_fieldbook_total(), "Fieldbooks", icon = icon("star")),
            valueBox(fbmaterials::get_material_total(), "Materials", icon = icon("star"))
          )
  )
}
