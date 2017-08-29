tab_resource_material <- function(){
  tabItem(tabName = "resource_material",
          fluidRow(
            # box(width = 3,
            #     title = "Browse material lists",
            #     shinyTree("material_tree")
            #     #rHandsontableOutput("hot_materials_list_ids")
            # ),
            box(width = 9, solidHeader = TRUE, status = "warning",
                title = "Create new lists",
                fluidRow(
                  column(width = 4,
                         box(width = NULL,
                             radioButtons("mlist_choose_list_source", "Import from:",
                                          choices = c("List", "Excel") ),

                             conditionalPanel(
                               "input.mlist_choose_list_source == 'List'",
                               selectInput("mlist_lists", label = "Please select list(s):",
                                           choices = list_material_lists())
                             ),
                             conditionalPanel(
                               "input.mlist_choose_list_source == 'Excel'",
                               shinyFilesButton('mlist_files', 'File selection', 'Please select a file', FALSE )

                             )
                         )
                  ),
                  column(width = 4,
                         box(width = NULL,
                             selectInput("mlist_crop", "Assign a crop:",
                                         unique(get_crop_table()$crop_name) )
                         ),
                         box(width = NULL,
                             selectInput("mlist_year", "Assign a year:", 2000:2050 )
                         )
                  ),
                  column(width = 4,
                         box(width = NULL,
                             textInput("mlist_name", "Assign a new list name:", "TEST123")#,
                             #actionButton("doListButton", "Create new material list!")

                         )
                  )

                )


            ),
            column(width = 3,
                   box(width = NULL, title="Actions", background = "black",

                       actionButton("doListButton", "Create new list!"),
                       HTML("<br/>"),
                       uiOutput('selectMList'),
                       HTML("<br/>"),
                       downloadButton("downloadMaterialListData", "Export list!")

                   )
            )
          ),


          box( width = 12,
               title = "Plant materials:",
               #verbatimTextOutput('selTxt'),


               rHandsontableOutput("hot_materials", height = 300)
          )
  )
}
