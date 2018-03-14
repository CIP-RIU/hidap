server_environment <- function(input, output, session, values){

  #locs <- "data/sites/Master-list-trial-sites.xlsx"
  #locs <- readxl::read_excel(locs,1)
  locs <- fbsites::get_site_table()
  names(locs) <- toupper(names(locs))

  locs <- locs[!is.na(locs$LATD), ]
  locs <- locs[!is.na(locs$LOND), ]

  locs$LATD <- as.numeric(locs$LATD)
  locs$LOND <- as.numeric(locs$LOND)
  #print(str(locs))
  lng1 <- min(locs$LOND)
  lng2 <- max(locs$LOND)
  lat1 <- min(locs$LATD)
  lat2 <- max(locs$LATD)

  locs$CNTRY <- as.character(locs$CNTRY)
  locs$AEZ <- as.character(locs$AEZ)
  locs$CONT <- as.character(locs$CONT)
  locs$CREG <- as.character(locs$CREG)
  locs$ADM4 <- as.character(locs$ADM4)
  locs$ADM3 <- as.character(locs$ADM3)
  locs$ADM2 <- as.character(locs$ADM2)
  locs$COMMENT <- as.character(locs$COMMENT)
  locs$ALTERN <- as.character(locs$ALTERN)

  nvar <- abs(round(rnorm(length(locs$LOND))*100, 0)) + 10
  locs <- cbind(locs, nvar)

  locsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(locs[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    #print(bounds)
    subset(locs,
           LATD >= latRng[1] & LATD <= latRng[2] &
             LOND >= lngRng[1] & LOND <= lngRng[2])
  })


  observeEvent(input$map_env_marker_click, {
    setMap_msg(input$map_env_marker_click)

  })

  output$rep_loc <- renderUI({

    input$locs_report_button

    locs <- isolate({ locsInBounds()})
    n = nrow(locsInBounds())
    if(n<1) return("no locations in view!")
    report = paste0("report_",input$fb_analysis,".Rmd")
    #report = file.path("inst", "rmd", "report_location.Rmd")
    report = file.path(system.file("rmd", package = "hidap"), "report_location.Rmd")

    fn <- rmarkdown::render(report,
                            #output_format = "all",
                            output_dir = "www/reports/",
                            params = list(
                              locs = locs))

    html <- readLines("www/reports/report_location.html")
    HTML(html)


  })

  output$rep_loc_docs <- renderUI({
    file_report = "reports/report_location.pdf"
    locs <- isolate({ locsInBounds()})

    pdf <-paste0(" <a href='",file_report,"'>PDF</a>")
    file_report = "reports/report_location.docx"
    if(file.exists(file_report)) {
      docx <-paste0("<a href='",file_report,"'>DOCX</a>")
    }
    #HTML(paste(pdf, docx))
  })

  output$dot_yield <- renderPlot({
    data <- locsInBounds()$ELEV
    n = length(data)
    data <- as.numeric(data)
    if(n < 1) return("no data")
    hist(data, main = "Elevation", xlim = c(0,3600))
  })

  loc_info <- eventReactive(input$map_marker_click, {
    event <- input$map_marker_click
    msg <- values[["map_msg"]]

    rec <- subset(locs,
                  LATD == as.numeric(event$lat) & LOND == as.numeric(event$lng))
    if(nrow(rec) != 1) return("No location selected.")
    #rec = rec[1:(ncol(rec))]
    paste(names(rec),": ", rec, "<br/>", sep="")
  }, ignoreNULL = FALSE)

  output$site_desc <- renderUI ({
    HTML(loc_info())
  })

  loc_fieldtrial_info <- eventReactive(input$map_marker_click, {
    event <- input$map_marker_click
    msg <- values[["map_msg"]]

    rec <- subset(locs,
                  LATD == as.numeric(event$lat) & LOND == as.numeric(event$lng))
    if(nrow(rec) != 1) return("No location selected.")
    #rec = rec[1:(ncol(rec))]
    #paste(names(rec),": ", rec, "<br/>", sep="")
    #"Do get the fieldtrial list!"
    paste(fbmaterials::get_fieldbook_list_by_loc(rec$SHORTN), collapse = ", ")
  }, ignoreNULL = FALSE)

  output$site_fieldtrials <- renderUI ({
    HTML(loc_fieldtrial_info())
  })

  loc_genotype_info <- eventReactive(input$map_marker_click, {
    event <- input$map_marker_click
    msg <- values[["map_msg"]]

    rec <- subset(locs,
                  LATD == as.numeric(event$lat) & LOND == as.numeric(event$lng))
    if(nrow(rec) != 1) return("No location selected.")
    #rec = rec[1:(ncol(rec))]
    #paste(names(rec),": ", rec, "<br/>", sep="")
    #"Do get the fieldtrial list!"
    paste(fbmaterials::get_genotype_list_by_loc(rec$SHORTN), collapse = ", ")
  }, ignoreNULL = FALSE)

  output$site_genotypes <- renderUI ({
    HTML(loc_genotype_info())
  })

  output$map <- renderLeaflet({
    m = leaflet(width = "50%") %>% addTiles()
    m = m %>% leaflet::fitBounds(lng1,lat1, lng2, lat2)
    m %>% addMarkers(locs$LOND, locs$LATD,popup=locs$FULLN,
                     clusterOptions = markerClusterOptions())
  })

}

