columnNames <- c("id", "Site ID", "Type", "Name", "Country","First level admin", "Second level admin", "Third level admin","Fourth level admin" , "Village", "Nearest populated place", "Elevation", "Latitude", "Longitude","Creation date")

dt <- reactiveValues()

uiTrialScreenMain <- function(){
  aux <- updTrialSites()
  names(aux) <- columnNames
  dt$trialSites <- aux

    fluidRow(
      tags$script("$(document).on('click', 'button', function () {
                  Shiny.onInputChange('siteClickId',this.id);
                  Shiny.onInputChange('siteClick', Math.random())
    });"),
      box(
        #shinysky::showshinyalert(session, "alert_hagroSites", paste("New site has been successfully added"), styleclass = "success"),
        # title = " List site information",
        title = tagList(shiny::icon("list-ol"), "List Site information"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        
        # actionButton("btNewTrialSite", "Add New Site",  style="color: #fff; background-color: #35b872;", icon = icon("plus-circle")),
        actionButton("btNewTrialSite", "Add New Site",  class = "btn-primary",style="color: #fff;", icon = icon("plus-circle")),
        # shinysky::shinyalert("alert_SI_created", FALSE, auto.close.after = 4),
        br(),br(),br(),
        dataTableOutput("Sites_table")
      ),
      box(
        title = "Map",
        # title = tagList(shiny::icon("list-ol"), "List Site information"),
        status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        actionButton("btShowMap", "View sites",  class = "btn-primary",style="color: #fff;"),br(), br(),
        column( width = 12,
                leafletOutput("mymap1a", "100%", "550px")
        )
      ),
      box(solidHeader = FALSE, width = 12)
  )

}

# map1a = leaflet() %>%
#   addTiles("https://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
#   setView(lng = -4.04296, lat = 16.30796, zoom = 2) #%>%
#
# # Muestra mapa "1a"
# output$mymap1a <- renderLeaflet(map1a)

# output$mymap1a <- renderLeaflet(
#   leaflet() %>%
#     addTiles() %>%  # Add default OpenStreetMap map tiles
#     # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
#     setView(lng = -4.04296, lat = 16.30796, zoom = 2) #%>%
# )

output$Sites_table <- renderDataTable({

    DT=dt$trialSites
    print(dt$trialSites)
    if(nrow(dt$trialSites) > 0){
      DT[["Actions"]]<-
        paste0('
               <div  role="group" aria-label="Basic example">
                  <button type="button" class="btn btn-secondary view" id=siteView_',1:nrow(dt$trialSites),'><i class="fa fa-eye"></i></button>
                  <button type="button" class="btn btn-secondary edit" id=siteEdit_',1:nrow(dt$trialSites),'><i class="fa fa-edit"></i></button>
                   <button type="button" class="btn btn-secondary delete" id=siteDelete_',1:nrow(dt$trialSites),'><i class="fa fa-trash"></i></button>
               </div>

               ')
    }
    datatable(DT,
              escape=F,
              selection = "none",
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                columnDefs = list(list(visible=FALSE, targets=c(1, 12, 13, 14, 15)))
              ))

  # })
})




uiTrialSiteNew <- function(pData = NULL){
  strCreate = "Create"
  strCreateId = "btCreateSite"
  boxTitle <- "Create Site information"
  boxIcon <- "plus-circle"
  veg <- NULL
  if(is.null(pData)){
    vData <- vector("list", 13)
  }
  else{
    vData <- pData
    strCreate <- "Save"
    strCreateId <- "btUpdateSite"
    boxTitle <- "Update Site information"
    boxIcon  <- "edit"
   # veg <-  strsplit(vData[[13]], ',')[[1]]
  }


  mchoices = c("Farmer field", "Experiment station field", "Greenhouse/screenhouse", "Government forest", "Private forest", "Pasture", "Water body")
  fluidRow(
    shinydashboard::box(
    title = tagList(shiny::icon(boxIcon), boxTitle),
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 12,
    column(width = 6,
          disabled(textInput("inSiteID", label="Site ID", value = vData[[2]] )),
          selectizeInput("inSiteType", label="Site type", choices = mchoices, multiple  = TRUE , options = list(maxItems = 1, placeholder ="Select one..."), selected= vData[[3]] ),
          textInput("inSiteName", label = "Site name", value=vData[[4]]),
          # selectInput("inSiteCountry", label="Choose country", choices = listCountries),

          selectizeInput("inSiteCountry", label="Country name", multiple = TRUE,
                         choices = unique(geodb$NAME_0),
                         selected= vData[[5]],
                         options = list(maxItems = 1, placeholder = 'Select country... ')),

          #textInput("inSiteAdmin1", label = "Site, first-level administrative division name", value=vData[[6]]),

          uiOutput("fbsites_ui_admin1"),
          uiOutput("fbsites_ui_admin2"),
          uiOutput("fbsites_ui_admin3"),
          uiOutput("fbsites_ui_admin4"),
          uiOutput("fbsites_ui_admin5"),

          #textInput("inSiteAdmin2", label = "Site, second-level administrative division name", value=vData[[7]]),
          #textInput("inSiteVillage", label = "Village name", value=vData[[8]]),
          textInput("inSiteNearestPlace", label = "Nearest populated place", value=vData[[13]]),

          # radioButtons("select_geosystem", label = h4("Type of coordinates system",style = "font-family: 'Arial', cursive;
          #                                       font-weight: 500; line-height: 1.1; color: #4d3a7d;"),
          #              choices = c("Decimal", "Sexagesimal"),
          #              selected = "Decimal"),
          # 
          # conditionalPanel(
          #   condition = "input.select_geosystem == 'Decimal'",
          # 
          #                  shiny::numericInput(inputId = "fbsites_latitude",  value = 0.0, label = "Latitude"),
          #                  shiny::numericInput(inputId = "fbsites_longitude", value = 0.0, label = "Longitude")
          #   ),
          # 
          # 
          # conditionalPanel(
          #   condition = "input.select_geosystem == 'Sexagesimal'",
          # 
          #       column(4,  div(style="display:inline-block",
          #                  h4("Latitude"),
          #                  numericInput(inputId = "fbsites_grade_lat", label="Grades (°)", value = 10),
          #                  numericInput(inputId = "fbsites_minute_lat", label="Minutes (')", value = 10),
          #                  numericInput(inputId = "fbsites_second_lat", label="Seconds ('')", value = 10),
          #                  selectInput(inputId  =  "fbsites_orientation_lat", label="Orientation",
          #                              choices  = c("N","S"), selected = "N" )#,
          #       ),
          # 
          #       column(4,  div(style="display:inline-block",
          #                  h4("Longitude"),
          #                  numericInput(inputId = "fbsites_grade_long", label="Grades (°)", value = 10),
          #                  numericInput(inputId = "fbsites_minute_long", label="Minutes (')", value = 10),
          #                  numericInput(inputId = "fbsites_second_long", label="Seconds ('')", value = 10),
          #                  selectInput(inputId =  "fbsites_orientation_long", label="Orientation",
          #                              choices = c("E","W"), selected = "E" )#,
          #       )#,
          # 
          #   ),

          #textInput("inSiteElevation", label = "Site elevation (m.a.s.l.)", value=vData[[9]]),
          shiny::numericInput(inputId = "inSiteElevation" ,label = "Site elevation (m.a.s.l.)", value = vData[[9]] ),
          shiny::numericInput(inputId = "inSiteLatitude" , label = "Site latitude (in decimal degrees)", value = vData[[10]] ),
          #textInput("inSiteLatitude", label = "Site latitude (in decimal degrees)", value=vData[[10]]),
          shiny::numericInput(inputId = "inSiteLongitude" , label = "Site longitude (in decimal degrees)", value = vData[[11]] ),
          #textInput("inSiteLongitude", label = "Site longitude (in decimal degrees)", value=vData[[11]]),

          # selectizeInput("inSiteVegetation", label="Vegetation surrounding the experiment site", multiple = TRUE, selected =  veg,
          #                choices = c("Grasslan", "Crops", "Forest", "Woodland", "Shrub land", "Savanna", "Other"),
          #                options = list(maxItems = 5, placeholder = 'select surroundings ')),
          # textAreaInput("inSiteDescNotes", label="Site description notes", value=vData[[12]]),


          useShinyalert(),
          actionButton(strCreateId, strCreate, class = "btn-primary",style="color: #fff;"),
          actionButton("goToMainSiteScreen", "Cancel")#,


          #shinysky::shinyalert("alert_hagroSites", FALSE, auto.close.after = 8)

    )
  )#end box
  ,
  box(solidHeader = FALSE, width = 12)
  )#end fluidrow

}
