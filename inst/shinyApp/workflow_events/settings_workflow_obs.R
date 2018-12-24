source("server_funcs/niche_layers.R",local = T)
source("server_funcs/data_gbif.R",local = T)
source("server_funcs/dynamicMapMethods.R",local = T)
source("server_funcs/niche_layers_extract.R",local = T)
source("server_funcs/niche_space_visualizations.R",local = T)
source("server_funcs/k_means_methods.R",local = T)
source("server_funcs/correlation_methods.R",local = T)
source("server_funcs/bioclim_methods.R",local = T)
source("helpers/ellipsoid_3D_plot.R",local = T)
source("helpers/pROC.R",local = T)
source("server_funcs/ellipsoid_methods.R",local = T)
source("server_funcs/model_proj_methods.R",local = T)
source("server_funcs/partial_roc_methods.R",local = T)
source("server_funcs/binary_map_methods.R",local =T)
source("server_funcs/bin_test_methods.R",local =T)
source("server_funcs/extrapolation_methods.R",local=T)
source("server_funcs/gistools_methods.R",local =T)
source("helpers/ldraw2sp.R",local = TRUE)

#volumes <- c(path.expand('~'))
osSystem <- Sys.info()["sysname"]

if(osSystem %in% c("Darwin","Linux") ){

  #------------------------------------------------------------------------------
  # Raster layer directory

  #volumes <- getVolumes()
  volumes <- c('home'="/Users/",'Volumes'="/")
  shinyFiles::shinyDirChoose(input, "ras_layers_directory",
                             roots=  c('home'="/Users/",'Volumes'="/"),
                             session = session
  )

  #shinyFiles::shinyDirChoose(id = "ras_layers_directory",input = input,
  #                           session =session,defaultPath = getwd())

  # User raster (niche) layers

  output$layers_directory <- renderPrint({
    layers_dir <- shinyFiles::parseDirPath(volumes, input$ras_layers_directory)
    if(length(layers_dir)>0L)
      return(layers_dir)
    else
      return("Press the button and select a dir")
  })



  rasterLayers <- reactive({

    if( input$getEnvData && !is.null(getEnvData()))
      return(getEnvData())
    else{

      layers_dir <- shinyFiles::parseDirPath(volumes, input$ras_layers_directory)
      ras_formats <- "(*.asc$)|(*.bil$)|(*.sdat$)|(*.rst$)|(*.nc$)|(*.tif$)|(*.envi$)|(*.img$)"
      layDirs <-list.files(layers_dir,pattern = ras_formats)
      #print(layDirs)

      input$loadNicheLayers
      isolate({

        if(input$loadNicheLayers > 0 && length(layers_dir) > 0L && length(layDirs)>0L)
          return(rlayers_ntb(layers_dir))
        else
          return(NULL)
      })
    }


  })

  #observe({
  #  print(rasterLayers())
  #})
  #------------------------------------------------------------------------------
  # Porjection Raster layer directory


  shinyFiles::shinyDirChoose(input, "proj_layers_directory",
                             roots=  c('home'="/Users/",'Volumes'="/"),
                             session = session)


  output$proj_directory <- renderPrint({
    layers_dir <- shinyFiles::parseDirPath(volumes, input$proj_layers_directory)
    if(length(layers_dir)>0L)
      return(layers_dir)
    else
      return("Press the button and select a dir")
  })


  proj_rasterLayers <- reactive({

    if(input$getEnvData && !is.null(getEnvData_future())) return(getEnvData_future())
    layers_dir <- shinyFiles::parseDirPath(volumes, input$proj_layers_directory)
    ras_formats <- "(*.asc$)|(*.bil$)|(*.sdat$)|(*.rst$)|(*.nc$)|(*.tif$)|(*.envi$)|(*.img$)"
    layDirs <-list.files(layers_dir,pattern = ras_formats)

    input$loadNicheLayers
    isolate({

      if(input$loadNicheLayers > 0 && length(layers_dir) > 0L && length(layDirs)>0L)
        return(rlayers_ntb(layers_dir))
      else
        return(NULL)
    })
  })


  #------------------------------------------------------------------------------
  # Workflow directory

  shinyFiles::shinyDirChoose(input, "wf_directory",
                             roots=  c('home'="/Users/",'Volumes'="/"),
                             session = session)


  workflowDir <- reactive({
    path <-shinyFiles::parseDirPath(volumes,
                                    input$wf_directory)
    ifelse(nchar(path)>0L, path <- paste0(path,"/"),path)
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })

  output$work_directory <- renderPrint({
    if(is.null(workflowDir()))
      message1 <- "Press the button and select dir"
    else
      message1 <- workflowDir()
    return(message1)
  })

  # --------------------------------------------------------
  # Save polygon to directory
  shinyFiles::shinyDirChoose(input, "poly_dir",
                             roots=  c('home'="/Users/",'Volumes'="/"),
                             session = session)

  # Polygon directory
  poly_dir <- reactive({
    path <- shinyFiles::parseDirPath(volumes, input$poly_dir)
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })


}


if(osSystem %in% c("Windows")){
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$ras_layers_directory
    },
    handlerExpr = {
      if (input$ras_layers_directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'ras_layers_directory'))

        # update the widget value
        updateDirectoryInput(session, 'ras_layers_directory', value = path)

      }
    }
  )


  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$proj_layers_directory
    },
    handlerExpr = {
      if (input$proj_layers_directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'proj_layers_directory'))

        # update the widget value
        updateDirectoryInput(session, 'proj_layers_directory', value = path)

      }
    }
  )




  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$wf_directory
    },
    handlerExpr = {
      if (input$wf_directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'wf_directory'))

        # update the widget value
        updateDirectoryInput(session, 'wf_directory', value = path)

      }
    }
  )

  # --------------------------------------------------------
  # Save polygon to directory

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$poly_dir
    },
    handlerExpr = {
      if (input$poly_dir > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'poly_dir'))

        # update the widget value
        updateDirectoryInput(session, 'poly_dir', value = path)

      }
    }
  )


  # Raster layer directory
  rasterLayersDir <- reactive({
    path <- readDirectoryInput(session, 'ras_layers_directory')
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })


  # Raster layer directory
  proj_rasterLayersDir <- reactive({
    path <- readDirectoryInput(session, 'proj_layers_directory')
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })


  # Workflow directory
  workflowDir <- reactive({
    path <- readDirectoryInput(session, 'wf_directory')

    ifelse(nchar(path)>0L, path <- paste0(path,"/"),path)
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })

  # User raster (niche) layers

  rasterLayers <- reactive({
    if(input$getEnvData && !is.null(getEnvData())) return(getEnvData())
    layers_dir <- rasterLayersDir()
    input$loadNicheLayers
    isolate({
      if(input$loadNicheLayers > 0 && length(layers_dir) > 0L)
        return(rlayers_ntb(layers_dir))
      else
        return(NULL)
    })
  })


  # User projection raster (niche) layers

  proj_rasterLayers <- reactive({
    if(input$getEnvData && !is.null(getEnvData_future())) return(getEnvData_future())
    layers_dir <-  proj_rasterLayersDir()
    input$loadNicheLayers
    isolate({
      if(input$loadNicheLayers > 0 && length(layers_dir) > 0L)
        return(rlayers_ntb(layers_dir))
      else
        return(NULL)
    })
  })

  # Polygon directory
  poly_dir <- reactive({
    path <- readDirectoryInput(session, 'poly_dir')
    if(length(path)>0L)
      return(path)
    else
      return(NULL)
  })

}


getEnvData <- eventReactive(input$get_now,{

  variable <- input$wc_var
  resol <- as.numeric(input$wc_resol)

  if(length(workflowDir())>0L && nchar(workflowDir())>2){
    dirtosv <- workflowDir()
  }
  else
    dirtosv <- getwd()
  layers_dir <-  file.path(dirtosv,"ntbox_envLayers")
  if(!dir.exists(layers_dir))
    dir.create(layers_dir)

  if(input$env_data=="wc" && input$getEnvData){
    wc <- raster::getData(name= 'worldclim',var=variable,res=resol)
    layers_dir2 <- file.path(layers_dir,
                             paste0("wc_",input$wc_var,
                                    "_",input$wc_resol))

    if(!dir.exists(layers_dir2))
      dir.create(layers_dir2)

    archpaths <- file.path(layers_dir2,
                           paste0(names(wc),
                                  input$env_format))

    1:length(archpaths) %>%
      purrr::map(~raster::writeRaster(wc[[.x]],archpaths[.x],overwrite=TRUE))

    return(wc)
  }
  return()
})

getEnvData_future <- eventReactive(input$get_now_future,{

  variable <- input$wc_var
  resol <- as.numeric(input$wc_resol)

  if(length(workflowDir())>0L && nchar(workflowDir())>2){
    dirtosv <- workflowDir()
  }
  else
    dirtosv <- getwd()
  layers_dir <-  file.path(dirtosv,"ntbox_envLayers")
  if(!dir.exists(layers_dir))
    dir.create(layers_dir)

  if(input$env_data=="wc_future" && input$getEnvData){
    rcp <- as.numeric(as.character(input$rcp))
    wc_model <- input$CMIP5_mod
    year <- as.numeric(as.character(input$year_sc))
    wc <- raster::getData(name= 'CMIP5',var=variable,res=resol,
                          rcp=rcp, model= wc_model,year=year)
    if(is.null(wc)) return(NULL)
    layers_dir2 <- file.path(layers_dir,
                             paste0("CMIP5_","rcp",rcp,"_y",year,
                                    "_",names(input$CMIP5_mod),
                                    "_",input$wc_var,
                                    "_",input$wc_resol))

    if(!dir.exists(layers_dir2))
      dir.create(layers_dir2)

    archpaths <- file.path(layers_dir2,paste0(names(wc),input$env_format))

    1:length(archpaths) %>%
      purrr::map(~raster::writeRaster(wc[[.x]],archpaths[.x],overwrite=TRUE))

    return(wc)

  }
  return()


})


observe({
  if(!is.null(getEnvData()) || !is.null(getEnvData_future())){
    cat("Climate data downloaded from:\n")
    cat("http://worldclim.org/\n")
    cat("Please cite as:\n")
    cat("   Hijmans RJ, Cameron SE., Parra JL, Jones Peter G., Jarvis Andy. (2005)\n
        Very high resolution interpolated climate surfaces for global land areas.\n
        Int J Climatol 25:1965–1978 . doi: 10.1002/joc.1276\n")
  }
  })

# Shape layers in directory

layers_shp <- reactive({
  if(!is.null(poly_dir())){
    layersDir <- list.files(poly_dir(),pattern = "*.shp$",full.names = F)
    #layers <- lapply(layersDir,
    #                 function(x)
    #                   str_extract_all(string = x,
    #                                   pattern = "([A-z]|[:digit:])+[^.\\shp]")[[1]])
    #layers <- unlist(layers)
    layers <- layersDir[grep(".shp",x = layersDir)]
    layers <- gsub(pattern = ".shp",x = layers,"")

    layers <- c("Select a layer",layers)
    return(layers)
  }
  else
    return()
})

observe({
  if(!is.null(layers_shp())){
    updateSelectInput(session,inputId = "poly_files",
                      choices =c("Select a layer"=NULL,layers_shp()) ,
                      selected = NULL)
  }
})

# Read polygons



myPolygon <- reactive({
  l_featuers <- input$dyMap_cas_draw_all_features
  if(!is.null(l_featuers) && input$poly_from == 1){
    ntb_polygons <- ldraw2sp(leaflet_draw = l_featuers)
    if(!is.character(ntb_polygons))
      ntb_polygons$area_sqkm <- raster::area(ntb_polygons) / 1000000
    return(ntb_polygons)
  }
  if(input$define_M == 1 && input$poly_from == 0 && !is.null(poly_dir()) &&  !is.null(input$poly_files)){
    map <- try(readOGR(dsn = poly_dir(),layer = input$poly_files),silent = TRUE)
    if(class(map) != "try-error")
      return(map)
    else
      return()
  }
})


#myPolygon <- reactive( {
#  l_featuers <- input$dyMap_gis_draw_all_features
#  if(!is.null(l_featuers)){
#    ntb_polygons <- ldraw2sp(leaflet_draw = l_featuers)
#    if(!is.character(ntb_polygons))
#      ntb_polygons$area_sqkm <- raster::area(ntb_polygons) / 1000000
#    return(ntb_polygons)
#  }

#})




# -----------------------------------------------------------------
# Saving workflow
# -----------------------------------------------------------------



# -----------------------------------------------------------------------
# Observer for writing to the Workflow geographic data report
# ---------------------------------------------------------------------

observeEvent(input$saveState, {

  if(length(workflowDir())>0L){
    if(nchar(workflowDir()) > 0L){

      # Create a directory for OCC data.
      data_dir_path <- paste0(workflowDir(),"NicheToolBox_OccData")
      if(!dir.exists(data_dir_path))
        dir.create(data_dir_path)
      # Create a directory for workflow report
      wf_dir_path <- paste0(workflowDir(),"NicheToolBox_workflowReport")
      if(!dir.exists(wf_dir_path))
        dir.create(wf_dir_path)

      # Animated map of GBIF records

      anifile <- paste0(tempdir(),"/",temGBIF())
      anima_save <- paste0(wf_dir_path,"/",input$genus,"_",
                           input$species,"_animation.gif")

      if(file.exists(anifile)) file.copy(anifile, anima_save)

      #------------------------------------------------------------
      # NicheToolBox data report
      #------------------------------------------------------------

      # Path to report source

      report_path <- system.file("shinyApp/ntb_report/data_report.Rmd",
                                 package = "ntbox")

      mchart_path <- system.file("shinyApp/ntb_report/MotChartInstructions.Rmd",
                                 package = "ntbox")

      # Save polygon
      if(!is.null(myPolygon())){
        file_dir <- paste0(data_dir_path,"/",
                           "M_Shapefiles_",input$dataset_dynMap)
        if(!dir.exists(file_dir))
          dir.create(file_dir)
        poly_name <- input$polygon_name
        if(length(poly_name)<1)
          poly_name <- paste0("dynMpolygon_ntb",sample(1:1000,1))
        poly_name_ext <- paste0(poly_name,".shp")
        #if(poly_name_ext %in% list.files(file_dir)){
        #  poly_name <- paste0(poly_name,"B_RandNUM",sample(1:1000,1))
        #}
        rgdal::writeOGR(myPolygon(), file_dir, poly_name,"_",input$dataset_dynMap, driver="ESRI Shapefile",overwrite_layer = T)

      }

      # save HTML path

      report_save <- paste0(wf_dir_path,"/","data_report.html")
      # save MotionChart display instructions
      mchart_save <- paste0(wf_dir_path,"/","DisplayMotionChartIns.html")

      # Compile workflow report

      rmarkdown::render(input = report_path,
                        output_format = html_document(pandoc_args = c("+RTS", "-K64m","-RTS"),
                                                      highlight="haddock",
                                                      self_contained = FALSE,
                                                      toc = TRUE,theme = "readable"),
                        output_file = report_save)


      # Compile Motion Chart instructions

      #render(input = mchart_path,
      #       output_format = html_document(pandoc_args = c("+RTS", "-K64m","-RTS"),
      #                                     highlight="haddock",
      #                                     toc = TRUE,theme = "readable"),
      #       output_file = mchart_save)


      # Save raw GBIF data (from GBIF data search)
      if(!is.null(data_gbif_search())){



        gbif_file_raw <- paste0(data_dir_path,"/",
                                input$genus,"_",
                                input$species,"_GBIF_raw_data",".csv")

        write.csv(data_gbif_search(),file = gbif_file_raw,row.names = FALSE)

      }
      # Save cleaned GBIF data (from GBIF data search)
      if(!is.null(data_gbif_search())){
        gbif_file_clean <- paste0(data_dir_path,"/",
                                  input$genus,"_",
                                  input$species,
                                  "GBIF_cleaned_data",".csv")
        write.csv(data_gbif(),file = gbif_file_clean,row.names = FALSE)
      }

      # Save GBIF data from dynamic map (no polygon)
      if(!is.null(dataDynamic()) && input$dataset_dynMap == "gbif_dataset"){
        gbif_file_clean_dynamic <- paste0(data_dir_path,"/",
                                          input$genus,"_",
                                          input$species,
                                          "GBIF_clean_dynamic_data",".csv")
        write.csv(dataDynamic(),file = gbif_file_clean_dynamic,row.names = FALSE)
      }
      # Save GBIF data from dynamic map (in polygon)
      if(!is.null(data_poly()) && input$dataset_dynMap == "gbif_dataset"){
        gbif_file_clean_dynamic_poly <- paste0(data_dir_path,"/",
                                               input$genus,"_",
                                               input$species,
                                               "GBIF_clean_Polygon_dynamic_data",
                                               ".csv")
        write.csv(data_poly(),file = gbif_file_clean_dynamic_poly,row.names = FALSE)
      }




      # Save raw user data (from user data)

      if(!is.null(data_user())){
        user_file_raw <- paste0(data_dir_path,"/",
                                "user_raw_data",".csv")
        write.csv(data_user(),user_file_raw,row.names = FALSE)
      }

      # Save cleaned user data (from user data)

      if(!is.null(data_user_clean())){
        user_file_clean <- paste0(data_dir_path,"/",
                                  "user_cleaned_data",".csv")
        write.csv(data_user_clean(),user_file_clean,row.names = FALSE)
      }
      # Save user data from dynamic map (no polygon)
      if(!is.null(dataDynamic()) && input$dataset_dynMap == "user_dataset"){
        user_file_clean_dynamic <- paste0(data_dir_path,"/",
                                          "user_clean_dynamic_data",".csv")
        write.csv(dataDynamic(),user_file_clean_dynamic,row.names = FALSE)
      }
      # Save GBIF data from dynamic map (in polygon)
      if(!is.null(data_poly()) && input$dataset_dynMap == "user_dataset"){
        user_file_clean_dynamic_poly <- paste0(data_dir_path,"/",
                                               "user_clean_Polygon_dynamic_data",
                                               ".csv")
        write.csv(data_poly(),user_file_clean_dynamic_poly,row.names = FALSE)
      }


    }
  }

})


# -----------------------------------------------------------------------
# Observer for writing to the Workflow: Niche data report
# ---------------------------------------------------------------------

observeEvent(input$saveState, {
  niche_data <- data_extraction()
  if(nchar(workflowDir()) > 0L && !is.null(niche_data) && length(workflowDir())>0L){
    # Create a directory for niche data.
    niche_dir_path <- paste0(workflowDir(),"NicheToolBox_NicheData")
    if(!dir.exists(niche_dir_path))
      dir.create(niche_dir_path)
    # Create a directory for workflow report
    wf_dir_path <- paste0(workflowDir(),"NicheToolBox_workflowReport")
    if(!dir.exists(wf_dir_path))
      dir.create(wf_dir_path)


    # Save data extraction

    if(!is.null(niche_data())){

      #------------------------------------------------------------
      # NicheToolBox niche data report
      #------------------------------------------------------------

      # Path to report source

      niche_data_report_path <- system.file("shinyApp/ntb_report/niche_data_report.Rmd",
                                            package = "ntbox")

      # save HTML path

      niche_data_report_save <- paste0(wf_dir_path,"/","niche_data_report.html")



      rmarkdown::render(input = niche_data_report_path,
                        output_format = html_document(pandoc_args = c("+RTS", "-K64m","-RTS"),
                                                      highlight="haddock",
                                                      toc = TRUE,theme = "readable"),
                        output_file = niche_data_report_save)


      niche_data <- niche_data()
      ifelse(input$datasetM== "gbif_dat",data <- "GBIF_data", data <- "User_data")
      ifelse(input$extracted_area== "all_area",
             raster_data <- "All_raster_area",raster_data <- "M_polygon_area")

      write.csv(niche_data, paste0(niche_dir_path,
                                   "/niche_",data,"_",raster_data,".csv"),
                row.names = FALSE)
      if(!is.null(kmeans_df()))
        write.csv(kmeans_df(), paste0(niche_dir_path,
                                      "/niche_",data,"_kmeansCluster.csv"),
                  row.names = FALSE)

      if(!is.null(corr_table())){
        write.csv(corr_table(), paste0(niche_dir_path,
                                       "/niche_",data,"_corretable.csv"),
                  row.names = FALSE)

        niche_dir_path <- paste0(workflowDir(),"NicheToolBox_NicheData")
        save_corfind <- paste0(niche_dir_path,"/niche_correlationfinder.txt")
        corr_finder <- summs_corr_var()$cor_vars_summary
        capture.output(print(corr_finder),file=save_corfind)

      }

    }

  }
})
