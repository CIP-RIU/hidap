source("www/sitesModule/ui_sites.R", local = TRUE)

map1a = leaflet() %>%
  addTiles("https://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png") %>%
  setView(lng = -4.04296, lat = 16.30796, zoom = 3) #%>%

observeEvent(input$btNewTrialSite, {
  output$trialScreen <- renderUI({
    uiTrialSiteNew()
  })
})



observeEvent(input$goToMainSiteScreen, {
  output$trialScreen <- renderUI({
    uiTrialScreenMain()
  })

})


observeEvent(input$btCreateSite, {
  if(validateNewList()){

    vSiteType <- input$inSiteType       #var1
    vSiteNama <- input$inSiteName       #var2
    vCountry  <- input$inSiteCountry    #var3
    vAdmin1   <- input$inSiteAdmin1     #var4
    vAdmin2   <- input$inSiteAdmin2     #var5
    vVillage  <- input$inSiteVillage    #var6
    vElevation <- input$inSiteElevation #var7
    vLatitud  <- input$inSiteLatitude   #var8
    vLongitude <- input$inSiteLongitude #var9
    vVegetation <- paste(input$inSiteVegetation, collapse = ",") #var11
    vDescNotes <- input$inSiteDescNotes #var10


    vSiteId <-  stri_rand_strings(1, 5,  '[A-Z]') #var12

    date  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    createDate <-as.character(Sys.time(), "%Y-%m-%d %H:%M:%S")

    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    insQry=  paste0("insert into user_sites ( " ,
                    " var1, var2, var3, var4, var5, var6, var7, var8, var9, var10, var11, var12, created, user_id) values('")
    insQry= paste0(insQry, vSiteType)
    insQry= paste(insQry, vSiteNama, vCountry, vAdmin1, vAdmin2, vVillage, vElevation, vLatitud, vLongitude, vDescNotes, vVegetation, vSiteId,  createDate, sep="','")
    insQry= paste0(insQry, "', " , USER$id, ")")
    qryUsers = dbSendQuery(mydb, insQry)
    dbDisconnect(mydb)
    output$trialScreen <- renderUI({
      uiTrialScreenMain()
    })


  }
  else{
    ## TO DO
    ## what to do if the input is not valid.
  }
})

observeEvent(input$btUpdateSite, {
  if(validateNewList()){

    vSiteType <- input$inSiteType       #var1
    vSiteName <- input$inSiteName       #var2
    vCountry  <- input$inSiteCountry    #var3
    vAdmin1   <- input$inSiteAdmin1     #var4
    vAdmin2   <- input$inSiteAdmin2     #var5
    vVillage  <- input$inSiteVillage    #var6
    vElevation <- input$inSiteElevation #var7
    vLatitud  <- input$inSiteLatitude   #var8
    vLongitude <- input$inSiteLongitude #var9
    vVegetation <- paste(input$inSiteVegetation, collapse = ",") #var11
    vDescNotes <- input$inSiteDescNotes #var10


    vSiteId <-  input$inSiteID #var12

    date  <- as.character(Sys.time(), "%Y%m%d%H%M%S")
    modifyDate <-as.character(Sys.time(), "%Y-%m-%d %H:%M:%S")

    mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
    updQry=  paste0("update user_sites set" ,
                    " var1 = '", vSiteType, "', ",
                    " var2 = '", vSiteName, "', ",
                    " var3 = '", vCountry, "', ",
                    " var4 = '", vAdmin1, "', ",
                    " var5 = '", vAdmin2, "', ",
                    " var6 = '", vVillage, "', ",
                    " var7 = '", vElevation, "', ",
                    " var8 = '", vLatitud, "', ",
                    " var9 = '", vLongitude, "', ",
                    " var10 = '", vDescNotes, "', ",
                    " var11 = '", vVegetation, "', ",
                    " modified = '", modifyDate, "' ",
                    "where user_id = " , USER$id, " " ,
                    "and var12 = '", vSiteId , "'")

    qryUsers = dbSendQuery(mydb, updQry)
    dbDisconnect(mydb)
    output$trialScreen <- renderUI({
      uiTrialScreenMain()
    })


  }
  else{
    ## TO DO
    ## what to do if the input is not valid.
  }
})


observeEvent(input$btShowMap, {
  output$mymap1a <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
      setView(lng = -4.04296, lat = 16.30796, zoom = 2) #%>%
  )
  updateMarkers()
})


updateMarkers <- function(){
  len <- nrow(dt$trialSites)
  isolate({
    leafletProxy("mymap1a") %>% clearMarkers()
    for (i in 1:len){
      leafletProxy("mymap1a") %>%
        addMarkers(lng=as.numeric(dt$trialSites[i,11]), lat=as.numeric(dt$trialSites[i,10]), popup="aaaaa")
    }
  })

}

observeEvent(input$siteClick,{


  # leaflet("mymap1a") %>%
  #   addMarkers(lng=-7.611575, lat=-72.552344, popup="The birthplace")


  if (input$siteClickId%like%"siteView"){

    row_to_view = as.numeric(gsub("siteView_","",input$siteClickId))
    # print(paste0("view:", row_to_view))
    showModal(modalViewSite(row_to_view))
  }
  else if (input$siteClickId%like%"siteEdit"){

    row_to_edit = as.numeric(gsub("siteEdit_","",input$siteClickId))
    output$trialScreen <- renderUI({
      uiTrialSiteNew(dt$trialSites[row_to_edit,])
    })
  }

  else if (input$siteClickId%like%"siteDelete"){

    row_to_delete = as.numeric(gsub("siteDelete_","",input$siteClickId))
    # dt$trialSites <- dt$trialSites[-row_to_delete, ]

    dt$trialSites <- deleteSite(row_to_delete)
    updateMarkers()
  }
}
)


modalViewSite <- function(pSiteID){
  nm <- names(dt$trialSites)
  len <- length(nm)
  vals  <- dt$trialSites[pSiteID,]
  ml <- c(vals[[2]])
  for (i in 3:len){
    ml <-c(ml, vals[[i]])
  }
  list <-data.frame(Variable = names(vals[,-1]),  Value = ml)

  modalDialog(
    fluidPage(
      h3(strong("Site information"),align="center"),
      hr(),
      HTML("<center>"),
      renderTable(list, align = "rl"),
      HTML("</center>")

    )
  )
}


deleteSite <- function(pSiteID){

  dbId <- dt$trialSites[pSiteID,1]
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  strQry = paste0("DELETE FROM user_sites
                   WHERE user_id = ", USER$id,
                  " AND id = " , dbId)

  qryMyfiles = dbSendQuery(mydb,strQry)
  dbDisconnect(mydb)
  return(updTrialSites())
  # dt$trialSites <- dt$trialSites[-pSiteID,]

}

updTrialSites <- function() {
  mydb = dbConnect(MySQL(), user=constUserDB, password=constPassDB, dbname=constDBName, host=constDBHost)
  strQry = paste0("SELECT
                  id,
                   var12,
                   var1,
                   var2,
                   var3,
                   var4,
                   var5,
                   var6,
                   var7,
                   var8,
                   var9,
                   var10,
                   var11,
                   created
                   FROM user_sites
                   WHERE user_id = ", USER$id,
                  " order by created DESC, id DESC")
  qryMyfiles = dbSendQuery(mydb,strQry)
  myFiles = fetch(qryMyfiles, n=-1)
  df_withMe <- data.frame(myFiles)
  dbDisconnect(mydb)
  return(df_withMe)
}




### to do: function to validate new site inputs
validateNewList <- function(){
  return(TRUE)
}
