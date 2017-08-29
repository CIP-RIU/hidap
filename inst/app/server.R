#
# # add hidap repository to local path
# options(repos = c(hidap = "https://c5sire.github.io/hd", getOption("repos")))
#
# # set HIDAP_HOME environment variable
# # Sys.setenv(HIDAP_HOME = "D:/HIDAP/")
#
# # set local library for hidap libs
# .libPaths("D:/HIDAP/libs")

library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)
library(date)
library(agricolae)
library(doBy)



library(shiny)
library(rhandsontable)
library(shinyTree)
library(shinyFiles)
#library(leaflet)
library(rmarkdown)
library(fbsites)
library(ggplot2)
library(plotly)

source("R/utils.R")
source("R/utils_fieldbook.R")
source("R/server_environment.R")
library(brapi)
library(stringr)
library(traittools)
library(sbformula)
library(data.table)
library(doBy)
#library(fbimport)




#print(fbglobal::get_base_dir())

shinyServer <- function(input, output, session) {

  values = shiny::reactiveValues()

  fbsites::server_site(input, output, session, values = values)
  fbcrops::server_crop(input, output, session, values = values)
  fbprogram::server_program(input, output, session, values = values)
  fbprstages::server_program_stages(input, output, session, values = values)
  fbmaterials::server_material_list(input, output, session, values = values)
  cropont::server_dictionary(input, output, session, values = values)
  fbmodule::server_module(input, output, session, values = values)
  fbdesign::server_design(input, output, session, values = values)

  fbcollect::srv_dataSource(input, output, session, values = values)
  fbcheck::fbcheck_server(input, output, session, values = values)

  #setMap_msg = function(x) values[["map_msg"]] = x

  get_fb_list <- reactive({
    fbl <- values[["ph_fb_list"]]
    #print(fbl)
    if(is.null(fbl)) {
      fbl <- fbmaterials::get_fieldbook_list(input$fb_analysis_crop, TRUE)
    }
    #print(fbl)
    fbl
  })


  output$fb_fieldmap_check <- d3heatmap::renderD3heatmap({
    #print("ok")
    if (!is.null(input[["phenotype_fb_choice"]])) {
      DF <- fbmaterials::get_fieldbook_data(  input[["phenotype_fb_choice"]])
      #print(head(DF))
      ci = input$hotFieldbook_select$select$c
      #print(ci)
      trt = names(DF)[ncol(DF)]
      if (!is.null(ci)) trt = names(DF)[ci]
      #print(trt)
      fm <- fbmaterials::fb_to_map(DF, gt = input[["def_genotype"]],
                                   variable = trt,
                                   rep = input[["def_rep"]],
                                   # blk = input[["def_block"]],
                                   plt = input[["def_plot"]]
      )
      #print(head(fm))
      amap = fm[["map"]]
      anot = fm[["notes"]]
      #print(head(amap))
      # print(head(anot))
      d3heatmap::d3heatmap(x = amap,
                           cellnote = anot,
                           colors = "Blues",
                           Rowv = FALSE, Colv = FALSE,
                           dendrogram = "none")
    }

  })



  # output$module_list <- renderUI({
  #   selectInput("module_crop_module", "Select a module:",
  #               choices = get_crop_modules(input$module_crop) )
  # })

  # output$module_var_list <- renderText({
  #   fp <- file.path(fname_module, "PTBM")
  #   load(fp)
  #   x <- paste(list_variables[,2],":", list_variables[,1])
  #   paste(x, collapse = ",\n ")
  # })




  output$fieldbook_list <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    selectInput("phenotype_fb_choice", "Select a fieldbook:",choices = get_fb_list(), width = 400)
  })


  # Field trial loading
  output$hotFieldbook <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)

      if(!is.null(DF)){

        rh = rhandsontable(DF,
                      selectCallback = TRUE,
                      readOnly = FALSE,useTypes = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 6)

        # from here onwards color check
        # nt <- length(fb_trait)
        # out_temp <- list()
        # renderer_trait <-  list()
        # out_temp[[1]] <- rhandsontable::rhandsontable(DF,
        #                                               readOnly = FALSE,
        #                                               selectCallback = TRUE,
        #                                               useTypes = TRUE)
        #
        #
        # #out_temp[[1]] <- DF #rhandsontable::rhandsontable(data = fieldbook_dashboard,readOnly = FALSE,useTypes = TRUE) #%>%
        # for(i in 1:nt){
        #     renderer_trait[[i]] <- fbcheck::render_trait(fb_trait[i], datadict)
        #     #print(renderer_trait[[i]])
        #     j <- i+1
        #     #print(j)
        #      out_temp[[j]] <- rhandsontable::hot_col(hot = out_temp[[i]],
        #                                              col = fb_trait[i],
        #                               allowInvalid = TRUE,
        #                               copyable = TRUE,
        #                               renderer = renderer_trait[[i]])
        # }
        #
        # out_temp
        #
        # k <- nt+1
        # out_temp  %>%
        rh %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 4)


        #str(out_temp) %>% print
      #   k <- nt+1
      #
      #   out_temp[[k]] %>%
      #     hot_context_menu(
      #       customOpts = list(
      #         csv = list(name = "Download to CSV",
      #                    callback = htmlwidgets::JS(
      #                      "function (key, options) {
      #                      var csv = csvString(this);
      #                      var link = document.createElement('a');
      #                      link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
      #                      encodeURIComponent(csv));
      #                      link.setAttribute('download', 'data.csv');
      #                      document.body.appendChild(link);
      #                      link.click();
      #                      document.body.removeChild(link);
      # }"))))



      }
    })
  } )

  output$hotMinimal <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "Minimal")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )

  output$hotInstallation <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "Installation")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )


  output$hotCropManagement <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "CropManagement")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )

  output$hotMaterialList <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "MaterialList")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )

  output$hotVariableList <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "VariableList")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )

  output$hotWeatherData <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "WeatherData")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )

  output$hotSoilAnalysis <- renderRHandsontable({
    try({
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      DF = attr(DF, "SoilAnalysis")
      if(!is.null(DF)){
        rhandsontable(DF,
                      selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols( fixedColumnsLeft = 1)
      }
    })
  } )


  output$fb_def_reps <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,1 ) == "R"]
        return(selectInput("def_rep","Define replication:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_plot <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,2 ) == "PL"]
        tf <- c(tf, "")
        return(selectInput("def_plot","Define plot:", tf, ti))
      }
    }
    ""
  })


  output$fb_def_block <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {

        ti <- tf[stringr::str_sub(tf, 1,1 ) == "B"]
        if(length(ti) == 0) return(NULL)
        tf <- c(tf, "")
        return(selectInput("def_block","Define block:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_genotype <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_factors(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        ti <- tf[stringr::str_sub(tf, 1,1 ) %in% c("G", "I", "C")]
        return(selectInput("def_genotype","Define genotype:", tf, ti))
      }
    }
    ""
  })

  output$fb_def_variables <- renderUI({
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    if(!exists_fb) return("")
    if(!is.null(input[["phenotype_fb_choice"]])) {
      tf <- fbmaterials::get_trial_variables(input$phenotype_fb_choice)
      if(length(tf) > 0) {
        #ti <- tf[stringr::str_sub(tf, 1,1 ) %in% c("G", "I", "C")]
        chc <- as.list(tf)
        names(chc) = tf
        selected = names(chc)[(length(chc) - 3):length(chc)]
        #print(chc)
        return(selectizeInput("def_variables",choices = chc,
                              selected = selected,
                              label = "Variables",
                              multiple = TRUE))
      }
    }
    ""
  })



  shiny::observeEvent(input$butDoPhAnalysis, ({
    #if(exists(input[["fb_analysis_crop"]])) {
    exists_fb <- fbmaterials::exists_fieldbook(input[["fb_analysis_crop"]])
    #print("1")
    if(!exists_fb) return("")
    #print("2")
    #if(is.null(input$horFieldbook)) return("")
    #print("3")
    if (!is.null(input[["phenotype_fb_choice"]])) {
      # print("4")


      DF <- fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      #print("4A")
      # y <- input$hotFieldbook_select$select$c
      # if(is.null(y)) return(HTML(""))
      # #print(y)
      # y <- names(DF)[y]

      y <- input$def_variables
      #print("1")
      if(input$fb_analysis == "descriptive"){
        report = paste0("report_",input$fb_analysis,".Rmd")
        report_dir = file.path("inst", "rmd")
      }
      if(input$fb_analysis == "aov"){

        report =  "rcbd2_withchild.Rmd"
        report_dir = system.file("rmd", package = "pepa")
        #report_dir = system.file("app/www/reports", package = "hidap")

        #y = y[1]
      }
      wd = getwd()
      #result_dir  = file.path(wd, "www", "reports")
      result_dir  =  system.file("app/www/reports", package = "hidap")
      #print(attr(DF, "meta"))
      # print(paste("report dir: ", report_dir))
      # print(wd)
      # print(result_dir)
      author =  paste0(Sys.getenv("USER"), " using HIDAP")
      withProgress(message = "Creating report ...",
                   detail = "This may take a while ...", value = 0,{
                     try({
                       devtools::in_dir(report_dir, {
                         print("X")
                         rmarkdown::render(report,
                                           output_format = c("pdf_document", "word_document",
                                                             "html_document"),
                                           output_dir = file.path(wd, "www"),
                                           params = list(
                                             meta = attr(DF, "meta"),
                                             data = DF,
                                             rep  = input$def_rep,
                                             treat = input$def_genotype,
                                             trait = y,
                                             maxp = 0.1,
                                             author = author))
                         #print("Y")
                       }) # in_dir
                       incProgress(1/3)
                     }) # try

                     try({
                       report_html = stringr::str_replace(report, ".Rmd", ".html")
                     })
                     output$fb_report <- renderUI("")
                     report = file.path(wd, "www", report_html)
                     print(report)
                     html <- readLines(report)
                     incProgress(3/3)
                   })
      output$fb_report <- renderUI(HTML(html))

    }
  })

  )

  output$fb_fieldmap_title <- renderText({
    out  = ""
    if (!is.null(input[["phenotype_fb_choice"]])) {
      DF <- fbmaterials::get_fieldbook_data(  input[["phenotype_fb_choice"]])
      reps = unlist(input[["def_rep"]])
      ci = input$hotFieldbook_select$select$c
      trt = names(DF)[ncol(DF)]
      if (!is.null(ci)) trt = names(DF)[ci]
      out = paste("Displaying spatial variation of trait variable:",  trt)
      if(length(unique(DF[, reps])) <= 1) {
        out = "Only one replication."
      }
    }
    HTML(out)
  })

  output$fb_fieldbook_title <- renderText({
    out  = ""
    if (!is.null(input[["phenotype_fb_choice"]])) {
      out = input$phenotype_fb_choice
    }
    HTML(out)
  })







  output$fb_report <- renderUI({
    if(!is.null(input[["phenotype_fb_choice"]])) {
      DF = fbmaterials::get_fieldbook_data(
        input$fb_phenotype_fb_choice)

      y <- input$hotFieldbook_select$select$c
      if(is.null(y)) return(HTML(""))
      #print(y)
      y <- names(DF)[y]

      report = paste0("reports/report_",input$fb_analysis,".Rmd")

      fn <- rmarkdown::render(report,
                              #output_format = "all",
                              output_dir = "www/reports/",
                              params = list(
                                fieldbook = DF,
                                independent = input$fb_def_geno,
                                dependent = y))

      report = paste0("www/reports/report_",input$fb_analysis,".html")
      html <- readLines(report)
      HTML(html)
    }
  })



  setHot_cross_marker = function(x) values[["hot_cross_marker"]] = x

  output$hot_cross_marker = rhandsontable::renderRHandsontable({
    shiny::withProgress(message = 'Loading table', {
      #list_name <- input$module_name
      #print(input$module_crop)
      DF_cross_marker <- fbqtl::get_cross_marker_table(crop = "potato",
                                                       name = "data.loc.rds" )
      #print(DF_cross_marker)
      if(!is.null(DF_cross_marker)){
        setHot_cross_marker(DF_cross_marker)
        rh <- rhandsontable::rhandsontable(DF_cross_marker,   stretchH = "all")
        rhandsontable::hot_table(rh, highlightCol = TRUE, highlightRow = TRUE)
      } else {
        NULL
      }
    })
  })


  library(qtlcharts)
  data(geneExpr)
  data(grav)
  library(qtl)


  output$qtl_output = qtlcharts::iplotCorr_render({
    iplotCorr(geneExpr$expr, geneExpr$genotype, reorder=TRUE,
              chartOpts=list(cortitle="Correlation matrix",
                             scattitle="Scatterplot"))
  })

  output$lod_output = qtlcharts::iplotScanone_render({
    data(hyper)
    hyper <- calc.genoprob(hyper, step=1)
    out <- scanone(hyper)

    # iplotScanone with no effects
    iplotScanone(out, chr=c(1, 4, 6, 7, 15))


    # iplotScanone with CIs
    iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15))

  })

  output$qtl_map_output = qtlcharts::iplotScanone_render({
    data(hyper)
    map <- pull.map(hyper)[1:15]

    iplotMap(map, shift=TRUE)

  })

  output$qtl_time_output = qtlcharts::iplotMScanone_render({
    data(grav)
    library(qtl)
    grav <- calc.genoprob(grav, step=1)
    grav <- reduce2grid(grav)

    # we're going to subset the phenotypes
    phecol <- seq(1, nphe(grav), by=5)

    # the times were saved as an attributed
    times <- attr(grav, "time")[phecol]

    # genome scan
    out <- scanone(grav, phe=phecol, method="hk")


    # plot with qualitative labels on y-axis
    iplotMScanone(out)

  })

  output$rf_output = qtlcharts::iplotRF_render({
    data(fake.f2)
    fake.f2 <- est.rf(fake.f2)
    iplotRF(fake.f2)
  })

  output$vcor_output = qtlcharts::iplotCorr_render({
    shiny::withProgress(message = 'Loading table', {
      DF = fbmaterials::get_fieldbook_data(
        input$phenotype_fb_choice)
      treat <- input$def_genotype
      trait <- input$def_variables
      DF = DF[, c(treat, trait)]

      DF[, treat] <- as.factor(DF[, treat])

      # exclude the response variable and empty variable for RF imputation
      datas <- names(DF)[!names(DF) %in% c(treat, "PED1")] # TODO replace "PED1" by a search
      x <- DF[, datas]
      for(i in 1:ncol(x)){
        x[, i] <- as.numeric(x[, i])
      }
      y <- DF[, treat]
      if (any(is.na(x))){
        DF <- randomForest::rfImpute(x = x, y = y )
        #data <- cbind(y, data)

      }
      names(DF)[1] <- treat


      #DF = dplyr::summarise_each(DF, funs(mean))

      DF = agricolae::tapply.stat(DF, DF[, treat])
      #print(head(DF))
      #corDF = cor(DF[, -c(1:2)])
      DF = DF[, -c(2)]
      names(DF)[1] = "Genotype"
      row.names(DF) = DF$Genotype
      DF = DF[, -c(1)]

      # iplotCorr(DF,  reorder=TRUE,
      #           chartOpts=list(cortitle="Correlation matrix",
      #                          scattitle="Scatterplot"))

      iplotCorr(DF)
    })
  })

  server_environment(input, output, session, values)

  # get fieldbooks in

  volumes <- getVolumes(c("(E:)", "Page File (F:)"))
  shinyFileChoose(input, 'fb_file', roots = volumes, session = session,
                  filetypes = c("xls", "xlsx"))

  output$fb_file_sel <- renderPrint({
    if (!is.null(input[["fb_file"]])) {
      fp <- shinyFiles::parseFilePaths( volumes, input$fb_file)
      fp <- as.character(fp$datapath[1])

      try({
        withProgress({
        #print(fp)
        ok = dc2hd(fp)
        #update list display
        # TODO
        values[["ph_fb_list"]] <- NULL

        basename(fp)
        }, message = paste("Importing file", basename(fp)))
      }, silent = TRUE)
    }
  })


  extract_material_list <- function(fieldbook, values){
    ml <- attr(fieldbook, "MaterialList")
    if(!is.null(ml)){
      try({
        nt <- fbmaterials::new_materials_table()
        nn <- names(nt)

        # add missing column
        pedigree = rep("", nrow(ml))
        ml <- cbind(ml, pedigree)
        new_seq <- c(4, 5, 6, 2, 3, 7, 15, 8, 9, 10, 11, 12, 13, 14)

        ml <- ml[, new_seq]
        names(ml) <- nn

        meta <- attr(fieldbook, "meta")
        crop <- meta$crop
        year <- meta$year
        mlist_name <- meta$title
        fbmaterials::post_material_table(ml, crop, year, mlist_name)
        values <- NULL

      })
     }
    values
  }


  # formula

  get_terms_formula <- function(s){
    trms <- stringr::str_extract_all(s, "[a-zA-Z_\\.]{2,20}[:]{2}[a-zA-Z_\\.]{2,20}")[[1]]
    trms[!duplicated(trms)]
  }


  get_terms <- function(s){
    frm_ref <- get_terms_formula(s)
    if(length(frm_ref)>0){
      s <- stringr::str_replace_all(s, frm_ref, "")
    }
    trms <- stringr::str_extract_all(s, "[a-zA-Z]{1}[a-zA-Z-_0-9\\.]{2,20}")[[1]]
    trms[!duplicated(trms)]
  }



  # read_onto <- function(file_path){
  #   readxl::read_excel(file_path, 2)
  # }

  get_formula <- function(onto){
    onto[!is.na(onto$Formula),"Formula" ]
  }

  get_decimals <- function(onto, syn = "Trait abbreviation"){
    onto[, c(syn, "Decimal places")]
  }

  get_function_name <- function(formula){
    fn = NULL
    if(stringr::str_detect(formula,"=")){
      fn = stringr::str_split(formula, "=")[[1]][1] %>% stringr::str_trim(side = "both")
    }
    fn
  }


  apply_formula <- function(fieldbook, formula){
    af <- function(fieldbook, formula){
      terms  = get_terms(formula[1])

      if (all(terms[-1] %in% names(fieldbook))) {

        try({
          n = length(terms[-1])
          for(i in 1:n){
            fieldbook[, terms[i+1]] = as.numeric(fieldbook[, terms[i+1]])
          }
        })

        if(!all(lapply(fieldbook[, terms[-1]], is.numeric) %>% unlist)){
          return(fieldbook)
        }

        x <- with(fieldbook,{
          # check if all term columns are present
          eval(parse(text = formula[1]))

        })
        fn = get_function_name(formula[1])
        if(!(fn %in% names(fieldbook))){
          fieldbook = cbind(fieldbook, x)
          names(fieldbook)[ncol(fieldbook)] = fn
        }
        if(fn %in% names(fieldbook)){
          fieldbook[, fn] = x
        }
      }
      fieldbook
    }
    # remove temporarily function calls
    formula = formula[!stringr::str_detect(formula, "::")]

    for(i in 1:length(formula)){
      fieldbook = af(fieldbook, formula[i])
    }
    fieldbook
  }


  dc2hd <- function(file_path){
    # check tabs
    capture.output({
    sheets <- readxl::excel_sheets(file_path)
    }, file = file.path(tempdir(), "tmp.txt"))
    min_sh <- c("Fieldbook", "Minimal", "Installation", "Material List",
                "Crop_management", "Var List", "Soil_analysis", "Weather_data")
    is_dc <- function(min_sh, sheets){
      all(min_sh %in% sheets)
    }
    ok = FALSE
    if(is_dc(min_sh, sheets)){
      try({
        # read fieldbook
        capture.output({
        fb <- readxl::read_excel(file_path, "Fieldbook")
        names(fb) <- stringr::str_trim(names(fb), "both")
        fbv <- fb
        #print("3")
        for(i in 4:ncol(fbv)){
          for(j in 1:nrow(fbv)){
            #if(i != 5){
            vv = fbv[j, i] %>% stringr::str_trim("both")
            if(vv %in% c(".", "?", "-", "*")) fbv[j, i] = NA
            #}
          }
        }
        for(i in 1:3){
          #if(is.factor(fbv[, i])){
          fbv[, i] <- as.factor(fbv[, i])
          #}
        }
        try({
          for(i in 4:ncol(fbv)){
            fbv[, i] <- as.numeric(fbv[, i])
          }
        })
        fb <- fbv
        fb <- fb[with(fb, order(PLOT)), ]

        n <- length(levels(fb$PLOT))
        fb <- fb[1:n, ]

        row.names(fb) = 1:n


        # read minimal meta & attach
        mn <- readxl::read_excel(file_path, "Minimal")
        nt <- readxl::read_excel(file_path, "Installation")
        ml <- readxl::read_excel(file_path, "Material List")
        cm <- readxl::read_excel(file_path, "Crop_management")
        vl <- readxl::read_excel(file_path, "Var List")
        sa <- readxl::read_excel(file_path, "Soil_analysis")
        wd <- readxl::read_excel(file_path, "Weather_data")

        attr(fb, "Minimal") <- mn
        attr(fb, "Installation") <- nt
        attr(fb, "MaterialList") <- ml
        attr(fb, "CropManagement") <- cm
        attr(fb, "VariableList") <- vl
        attr(fb, "SoilAnalysis") <- sa
        attr(fb, "WeatherData") <- wd

        # get minimal data
        old_name = basename(file_path)
        trial_type = "NN"
        title = stringr::str_trim(
          stringr::str_replace(old_name, "\\.xls[x]{0,1}", "" )
        )
        year <- as.integer(stringr::str_extract(
          mn[mn$Factor == "Begin date", "Value"], "[0-9]{4}")
        )
        country <- toupper(mn[mn$Factor == "Country", "Value"])
        iso2 <- toupper( stringr::str_sub(country, 1, 2) )
        site <- mn[mn$Factor == "Locality", "Value"]
        contact <- mn[mn$Factor == "Leader", "Value"]
        materials <- as.character(unique(fb$INSTN))
        variables <- names(fb)[-c(1:3)]
        crop_sh <- stringr::str_sub(title,1, 2)
        crop <- switch(crop_sh,
                       "PT" = "potato",
                       "SP" = "sweetpotato")

        meta <- list(crop = crop,
                     old_name = old_name,
                     title = title,
                     year = year,
                     country = country,
                     iso2 = iso2,
                     contact = contact,
                     materials = materials,
                     variables = variables)
        attr(fb, "meta") = meta

        # apply formula and formats
        onto <- cropont::get_dictionary_table(crop)
        fb <- apply_formula(fb, get_formula(onto))
        # apply rounding level according to ontology table
        dp = get_decimals(onto )
        ix = which(names(fb) %in% dp[,1])
        for(i in 1:length(ix)){
          pl <- dp[dp[, 1] == names(fb)[ix[i]], 2]
          fb[, ix[i]] <- round(fb[, ix[i]], pl )
          if(pl == 0) {
            fb[, ix[i]] <- as.integer(fb[, ix[i]])
          }
        }




        # save to target path
        dir_out <- fbglobal::fname_fieldbooks(crop)
        if(!dir.exists(dir_out)){
          dir.create(dir_out, recursive = TRUE)
        }
        f_out <- file.path(dir_out, paste0(title, ".rda") )
        saveRDS(fb, f_out )
        # get material list
        # save material list apart
        values[["ml_list_crop_year"]] <-
           extract_material_list(fb, values[["ml_list_crop_year"]])

        ok = TRUE
        }, file = file.path(tempdir(), "tmp.txt"))
      })

    }
    ok
  }

  output$fb_histogram_check <- renderPlot({
    if (!is.null(input[["phenotype_fb_choice"]])) {
      DF <- fbmaterials::get_fieldbook_data(  input[["phenotype_fb_choice"]])
      #DF = input$hotFieldbook
      #print(head(DF))
      ci = input$hotFieldbook_select$select$c
      print(ci)
      trt = names(DF)[ncol(DF)]

      if (is.null(ci)) {
        trt = names(DF)[ci]
        ci = ncol(DF)
      }
      print(trt)
      ggplot(DF, aes(x = as.symbol(trt)))  + geom_histogram()

    }
  })


  shiny::observeEvent(input$butDoMETAnalysis, ({
      DF <- readxl::read_excel("D:/HIDAP/potato-combine/combine_example/combine4met.xlsx")
      report = "met.Rmd"
      report_dir = system.file("rmd", package = "pepa")
      wd = getwd()
      result_dir  =  system.file("app/www/reports", package = "hidap")
      author =  paste0(Sys.getenv("USERNAME"), " using HIDAP")
      withProgress(message = "Creating report ...",
                   detail = "This may take a while ...", value = 0,{
                     try({
                       devtools::in_dir(report_dir, {
                         print("X")
                         rmarkdown::render(report,
                                           output_format = c("html_document"),
                                           output_dir = file.path(wd, "www"),
                                           params = list(
                                             traits = c("MTWP", "MTYA", "NMTP", "NPE", "NPH"),
                                             geno = "INSTN",
                                             env = "ENV",
                                             rep = "REP",
                                             data = DF,
                                             maxp = .1,
                                             author = author))
                         #print("Y")
                       }) # in_dir
                       incProgress(1/3)
                     }) # try

                     try({
                       report_html = stringr::str_replace(report, ".Rmd", ".html")
                     })
                     output$fb_report <- renderUI("")
                       report = file.path(wd, "www", report_html)
                     print(report)
                     html <- readLines(report)
                     incProgress(3/3)
                   })
      output$fb_report <- renderUI(HTML(html))


  })
  )


}

