columnNames <- c("id", "Site ID", "Type", "Name", "Country","First level admin", "Second level admin", "Village", "Eleveation", "Latitude", "Longitude","Description", "Vegetation surrounding", "Creation date")

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
        # title = " List site information",
        title = tagList(shiny::icon("list-ol"), "List Site information"),
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, width = 12,
        actionButton("btNewTrialSite", " Create",  style="color: #fff; background-color: #35b872;", icon = icon("plus-circle")),
        br(),br(),br(),
        dataTableOutput("Sites_table")
      ),
      box(
        title = "Markers Map",
        # title = tagList(shiny::icon("list-ol"), "List Site information"),
        status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        actionButton("btShowMap", "View Markers",  style="color: #fff; background-color: #35b872;"),br(), br(),
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
                columnDefs = list(list(visible=FALSE, targets=c(1, 8, 9, 10, 11, 12)))
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
   veg <-  strsplit(vData[[13]], ',')[[1]]
  }


  mchoices = c("Farmer field", "Experiment station field", "Greenhouse/screenhouse", "Government forest", "Private forest", "Pasture", "Water body")
  fluidRow(
    shinydashboard::box(
    title = tagList(shiny::icon(boxIcon), boxTitle),
    status = "primary", solidHeader = TRUE,
    collapsible = TRUE, width = 12,
    column(width = 6,
          disabled(textInput("inSiteID", label="Site ID", value = vData[[2]] )),
          selectizeInput("inSiteType", label="Site type", choices = mchoices, multiple  = FALSE, selected= vData[[3]] ),
          textInput("inSiteName", label = "Site name", value=vData[[4]]),
          # selectInput("inSiteCountry", label="Choose country", choices = listCountries),

          selectizeInput("inSiteCountry", label="Country", multiple = TRUE,
                         choices = listCountries,
                         selected= vData[[5]],
                         options = list(maxItems = 1, placeholder = 'select a country ')),

          textInput("inSiteAdmin1", label = "First-level administrative division name", value=vData[[6]]),
          textInput("inSiteAdmin2", label = "Second-level administrative division name", value=vData[[7]]),
          textInput("inSiteVillage", label = "Village name", value=vData[[8]]),
          textInput("inSiteElevation", label = "Site elevation (m.a.s.l.)", value=vData[[9]]),
          textInput("inSiteLatitude", label = "Site latitude (decimal degrees)", value=vData[[10]]),
          textInput("inSiteLongitude", label = "Site longitude (decimal degrees)", value=vData[[11]]),

          selectizeInput("inSiteVegetation", label="Vegetation surrounding the experiment site", multiple = TRUE, selected =  veg,
                         choices = c("Grasslan", "Crops", "Forest", "Woodland", "Shrub land", "Savanna", "Other"),
                         options = list(maxItems = 5, placeholder = 'select surroundings ')),
          textAreaInput("inSiteDescNotes", label="Site description notes", value=vData[[12]]),


          actionButton(strCreateId, strCreate, style="color: #fff; background-color: #35b872;"),
          actionButton("goToMainSiteScreen", "Cancel")
    )
  )#end box
  ,
  box(solidHeader = FALSE, width = 12)
  )#end fluidrow

}
