source("server_funcs/niche_layers.R",local = T)
source("server_funcs/data_gbif.R",local = T)
source("server_funcs/dynamicMapMethods.R",local = T)
source("server_funcs/niche_layers_extract.R",local = T)
source("server_funcs/niche_space_visualizations.R",local = T)
source("server_funcs/k_means_methods.R",local = T)
source("server_funcs/correlation_methods.R",local = T)
source("server_funcs/bioclim_methods.R",local = T)
source("helpers/ellipsoid_3D_plot.R",local = T)
#source("helpers/pROC.R",local = T)
source("server_funcs/ellipsoid_methods.R",local = T)
source("server_funcs/model_proj_methods.R",local = T)
source("server_funcs/partial_roc_methods.R",local = T)
source("server_funcs/binary_map_methods.R",local =T)
source("server_funcs/bin_test_methods.R",local =T)
source("server_funcs/extrapolation_methods.R",local=T)
source("server_funcs/gistools_methods.R",local =T)
source("server_funcs/ellipsoid_selection_methods.R",local =T)
source("helpers/ldraw2sp.R",local = TRUE)

#volumes <- c(path.expand('~'))
osSystem <- Sys.info()["sysname"]

if(osSystem %in% c("Darwin","Linux") ){

  #------------------------------------------------------------------------------
  # Raster layer directory

  #volumes <- getVolumes()
  #volumes <- c('home'="/Users/",'Volumes'="/")
  volumes <- c("Home" = "/home", 'Volumes'="/")
  shinyFiles::shinyDirChoose(input, "ras_layers_directory",
                             roots=   c("Home" = "/home", 'Volumes'="/"),
                             session = session
  )

  #shinyFiles::shinyDirChoose(id = "ras_layers_directory",input = input,
  #                           session =session,defaultPath = getwd())

  # User raster (niche) layers

  output$layers_directory <- renderPrint({
    #volumes <- c('root'="/")
    #print(volumes)
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
                             roots=  c("Home" = "/home", 'Volumes'="/"),
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
                             roots=   c("Home" = "/home", 'Volumes'="/"),
                             session = session)


  workflowDir <- reactive({
    path <- shinyFiles::parseDirPath(volumes,
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
    #wc <- raster::getData(name= 'worldclim',var=variable,res=resol)

    layers_dir2 <- file.path(layers_dir,
                             paste0("wc_",input$wc_var,
                                    "_",input$wc_resol))


    if(!dir.exists(layers_dir2))
      dir.create(layers_dir2)

    wc <- raster::stack(geodata::worldclim_global(var=variable,
                                                  res=resol,
                                                  path = layers_dir2))
    #wc <- ntbox::wc_cmip5()

    archpaths <- file.path(layers_dir2,
                           paste0(names(wc),
                                  input$env_format))

    1:length(archpaths) %>%
      purrr::map(~raster::writeRaster(wc[[.x]],archpaths[.x],overwrite=TRUE))

    return(wc)
  }
  if(input$env_data=="ch_pre" && input$getEnvData){
    chelsa_biocurrent <- get_chelsa(period = "current",
                                    sv_dir =layers_dir,
                                    load2r = TRUE,
                                    parallel = input$ch_parallel)
    return(chelsa_biocurrent)
  }
  if(input$env_data=="env_pres" && input$getEnvData){
    envirem_pass <-  get_envirem_clim(period= "current",
                                      gcm = NULL,
                                      region = input$env_reg,
                                      resolution = input$env_res,
                                      fmt= input$env_fmt,
                                      sv_dir = layers_dir,
                                      load2r = TRUE)
    return(envirem_pass)
  }
  if(input$env_data=="env_elev_pres" && input$getEnvData){
    envirem_pass <-  get_envirem_elev(period= "current",
                                      region = input$env_reg,
                                      resolution = input$env_res,
                                      fmt= input$env_fmt,
                                      sv_dir = layers_dir,
                                      load2r = TRUE)
    return(envirem_pass)
  }
  if(input$env_data=="bio_pre" && input$getEnvData){
    bio_oracle <-  get_bio_oracle(period= "current",
                                    var_type = input$bio_type,
                                    model = NULL,
                                    scenario = NULL,
                                    sv_dir = layers_dir,
                                    load2r = TRUE,
                                    parallel = input$ch_parallel)
    return(bio_oracle)
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
  if(input$env_data=="ch_fut" && input$getEnvData){
    chelsa_biofuture <- get_chelsa(period = input$ch_period,
                                   model = input$ch_model,
                                   rcp = input$ch_rcp,
                                   sv_dir =layers_dir,
                                   load2r = TRUE,
                                   parallel = input$ch_parallel)
    return(chelsa_biofuture)
  }

  if(input$env_data=="env_pass" && input$getEnvData){
    envirem_pass <-  get_envirem_clim(period= input$env_period,
                                      gcm = input$env_gcm,
                                      region = input$env_reg,
                                      resolution = input$env_res,
                                      fmt= input$env_fmt,
                                      sv_dir = layers_dir,
                                      load2r = TRUE)
    return(envirem_pass)
  }
  if(input$env_data=="env_elev_pass" && input$getEnvData){
    envirem_pass <-  get_envirem_elev(period= input$env_period,
                                      region = input$env_reg,
                                      resolution = input$env_res,
                                      fmt= input$env_fmt,
                                      sv_dir = layers_dir,
                                      load2r = TRUE)
    return(envirem_pass)
  }
  if(input$env_data=="bio_fut" && input$getEnvData){


    bio_oracle_fut <-  get_bio_oracle( period= input$bio_year,
                                       var_type = input$bio_type_fut,
                                       model = input$bio_model,
                                       scenario = tolower(input$bio_scenario),
                                       sv_dir = layers_dir,
                                       load2r = TRUE,
                                       parallel = input$ch_parallel)
    return(bio_oracle_fut)
  }

  return()
})

observe({
  if(!is.null(getEnvData()) || !is.null(getEnvData_future())){
    if(input$env_data=="wc" ||
       input$env_data=="wc_future"){
      cat("Climate data downloaded from:\n")
      cat("http://worldclim.org/\n")
      cat("Please cite as:\n")
      cat("   Hijmans RJ, Cameron SE., Parra JL, Jones Peter G., Jarvis Andy. (2005)\n
          Very high resolution interpolated climate surfaces for global land areas.\n
          Int J Climatol 25:1965–1978 . doi: 10.1002/joc.1276\n")
    }
    if(input$env_data=="ch_pre" ||
       input$env_data=="ch_fut"){
      cat("Climate data downloaded from:\n")
      cat("http://chelsa-climate.org/\n")
      cat("Please cite as:\n")
      cat(" Karger, D.N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H., Soria-Auza,\n
          R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2017) Climatologies at\n
          high resolution for the earth's land surface areas. Scientific Data 4, 170122.")

    }
    if(input$env_data=="bio_pre" ||
       input$env_data=="bio_fut"){
      cat("Climate data downloaded from:\n")
      cat("http://www.bio-oracle.org/index.php\n")
      cat("Please cite as:\n")
      cat(" Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrao, E. A.,&\n
          De Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for\n
          bioclimatic modelling. Global Ecology and Biogeography.")

    }
    if(input$env_data=="env_pres" ||
       input$env_data=="env_pass" ||
       input$env_data=="env_elev_pres" ||
       input$env_data=="env_elev_pass"){
      cat("Climate data downloaded from:\n")
      cat("https://envirem.github.io/\n")
      cat("Please cite as:\n")
      cat(" Title P.O., Bemmels J.B. 2018. ENVIREM: an expanded set of\n
          bioclimatic and topographic variables increases flexibility\n
          and improves performance of ecological niche modeling. \n
          Ecography. 41:291-307.")
    }

  }
  })

bio_oracle <- reactive({
  if(input$getEnvData && input$env_data=='bio_fut'){
    bio_oracle <- base::readRDS(file.path(system.file("extdata",
                                                      package = "ntbox"),
                                          "bio_oracle.rds"))


    nbio <- unique(bio_oracle[,c('year','model',
                                 'scenario',"type")])

    years <- nbio %>% split(.$year)
    biobd <- years[[input$bio_year]]
    return(biobd)
  }
})

observe({
  biobd <- bio_oracle()
  if(is.data.frame(biobd)){
    models <- unique(biobd$model)
    updateRadioButtons(session,"bio_model",
                       choices = models)
  }
})

biomodels_bd <- reactive({
  biobd <- bio_oracle()
  if(is.data.frame(biobd)){
    models_bd <- biobd %>% split(.$model)
    return(models_bd)
  }
  return()
})

observe({
  if(!is.null(biomodels_bd())){
    scenarios <- biomodels_bd()[[input$bio_model]]
    updateRadioButtons(session,"bio_scenario",
                       choices = unique(scenarios$scenario))
  }
})

bio_types_bd <- reactive({
  if(input$bio_scenario != "a"){
    scenarios <- biomodels_bd()[[input$bio_model]]
    return(scenarios)
  }
  return()
})
observe({
  if(is.data.frame(bio_types_bd())){
    updateSelectInput(session, "bio_type_fut",
                      choices = bio_types_bd()$type)
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
    #map <- try(readOGR(dsn = poly_dir(),layer = input$poly_files),silent = TRUE)
    map <- try(sf::st_read(dsn = poly_dir(),
                           layer = input$poly_files),
               silent = TRUE)
    map <- sf::as_Spatial(map)

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
      data_dir_path <- file.path(workflowDir(),
                                 "ntbox_OccData")
      if(!dir.exists(data_dir_path))
        dir.create(data_dir_path)
      # Create a directory for workflow report
      wf_dir_path <- file.path(workflowDir(),
                               "ntbox_workflowReport")
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
        poly_name_dir_ext <- file.path(file_dir,poly_name_ext)
        sf_pol <- sf::st_as_sf(myPolygon())
        sf::st_write(sf_pol,poly_name_dir_ext,delete_layer=TRUE)

        #rgdal::writeOGR(myPolygon(),
        #                file_dir,
        #                poly_name,"_",
        #                input$dataset_dynMap,
        #                driver="ESRI Shapefile",
        #                overwrite_layer = T)

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

      if(input$search_gbif_data && !is.null(data_gbif_search())){



        gbif_file_raw <- paste0(data_dir_path,"/",
                                input$genus,"_",
                                input$species,"_GBIF_raw_data",".csv")

        write.csv(data_gbif_search(),file = gbif_file_raw,row.names = FALSE)

      }
      # Save cleaned GBIF data (from GBIF data search)
      if(input$search_gbif_data && !is.null(data_gbif_search())){
        gbif_file_clean <- paste0(data_dir_path,"/",
                                  input$genus,"_",
                                  input$species,
                                  "GBIF_cleaned_data",".csv")
        write.csv(data_gbif(),file = gbif_file_clean,row.names = FALSE)
      }

      # Save GBIF data from dynamic map (no polygon)
      if(input$search_gbif_data &&
         !is.null(dataDynamic()) &&
         input$dataset_dynMap == "gbif_dataset"){
        gbif_file_clean_dynamic <- paste0(data_dir_path,"/",
                                          input$genus,"_",
                                          input$species,
                                          "GBIF_clean_dynamic_data",".csv")
        write.csv(dataDynamic(),file = gbif_file_clean_dynamic,row.names = FALSE)
      }
      # Save GBIF data from dynamic map (in polygon)
      if(input$search_gbif_data &&
         !is.null(data_poly()) &&
         input$dataset_dynMap == "gbif_dataset"){
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
  if(nchar(workflowDir()) > 0L &&
     !is.null(niche_data) && length(workflowDir())>0L){
    # Create a directory for niche data.
    niche_dir_path <- file.path(workflowDir(),
                                "ntbox_NicheData")
    if(!dir.exists(niche_dir_path))
      dir.create(niche_dir_path)
    # Create a directory for workflow report
    wf_dir_path <- file.path(workflowDir(),
                             "ntbox_workflowReport")
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
                                                      highlight="haddock",self_contained = FALSE,
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

        niche_dir_path <- paste0(workflowDir(),"ntbox_NicheData")
        save_corfind <- paste0(niche_dir_path,"/niche_correlationfinder.txt")
        corr_finder <- summs_corr_var()$cor_vars_summary
        capture.output(print(corr_finder),file=save_corfind)

      }

    }

  }
})


# -----------------------------------------------------------------------
# Observer for writing to the Workflow: Ecological niche modeling report
# ---------------------------------------------------------------------

observeEvent(input$saveState, {

  if(nchar(workflowDir()) > 0L &&
     !is.null(niche_data) && length(workflowDir())>0L){

    wf_dir_path <- file.path(workflowDir(),
                             "ntbox_workflowReport")

    enm_report_path <- system.file("shinyApp/ntb_report/enm_report.Rmd",
                                   package = "ntbox")

    enm_report_save <- file.path(wf_dir_path,"enm_report.html")


    if(!is.null(mve_obj_all()) || !is.null(mve_obj_m())){

      try({

        rmarkdown::render(input = enm_report_path,
                          output_format = html_document(pandoc_args = c("+RTS",
                                                                        "-K64m",
                                                                        "-RTS"),
                                                        self_contained = FALSE,
                                                        highlight="haddock",
                                                        toc = TRUE,
                                                        theme = "readable"),
                          output_file = enm_report_save)


      },silent = T)



      #model_name <- paste0()
      #raster::writeRaster(ellipsoid_obj$suitRaster,)

      models_dir_path <- file.path(workflowDir(),
                                  "ntbox_enm_files")
      if(!dir.exists(models_dir_path))
        dir.create(models_dir_path)

      # Save raster all all train Ellispoid models

      try({



        if(input$selectBios_all_all_train && (!is.null(mve_obj_all()) || !is.null(mve_obj_m()))){
          vars <- names(mve_obj_all()$centroid)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_ellip_all_proj_trained_all.asc")

          metafile <- base::gsub(pattern = ".asc",
                                 replacement =".text" ,
                                 mod_name)

        model <- ellip_model_all_rast_all_train()$suitRaster
        capture.output(mve_obj_all(),file = file.path(models_dir_path,
                                                      metafile))
        raster::writeRaster( model, file.path(models_dir_path,
                                              mod_name))

        }
      },silent = TRUE)

      # Save raster all M train Ellispoid models

      try({


        if(input$selectBios_all_m_train && (!is.null(mve_obj_all()) || !is.null(mve_obj_m()))){
          vars <- names(mve_obj_m()$centroid)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_ellip_all_proj_trained_m.asc")

          metafile <- base::gsub(pattern = ".asc",
                                 replacement =".text" ,
                                 mod_name)

          model <-ellip_model_all_rast_m_train()$suitRaster

          capture.output(mve_obj_m(),file = file.path(models_dir_path,
                                                      metafile))
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))
        }
      },silent = TRUE)

      # Save raster M all train Ellispoid models

      try({

        if(input$selectBios_m_all_train && (!is.null(mve_obj_all()) || !is.null(mve_obj_m()))){
          vars <- names(mve_obj_all()$centroid)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_ellip_m_proj_trained_all.asc")
          metafile <- base::gsub(pattern = ".asc",
                                 replacement =".text" ,
                                 mod_name)

          capture.output(mve_obj_all(),file = file.path(models_dir_path,
                                                        metafile))
          model <-ellip_model_m_rast_all_train()$suitRaster
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))

        }
      },silent = TRUE)

      # Save raster m m train Ellispoid models

      try({



        if(input$selectBios_m_m_train && (!is.null(mve_obj_all()) || !is.null(mve_obj_m()))){
          vars <- names(mve_obj_m()$centroid)
          mod_name <- paste0(paste( vars,
                                   collapse = "_"),
                             "_ellip_m_proj_trained_m.asc")

          metafile <- base::gsub(pattern = ".asc",
                                 replacement =".text" ,
                                 mod_name)

          model <-ellip_model_m_rast_m_train()$suitRaster

          capture.output(mve_obj_m(),file = file.path(models_dir_path,
                                                      metafile))
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))


        }
      },silent = TRUE)



      # Save raster all all train Bioclim models

      try({

        if(input$run_bioclim_all_all_train && !is.null(bioclim_model_all_all_train())){

          vars <- names(bioclim_model_all_all_train()$train@min)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_bioclim_all_proj_trained_all.asc")

          model <- bioclim_model_all_all_train()$prediction
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))

        }
      },silent = TRUE)


      # Save raster m all train Bioclim models

      try({

        if(input$run_bioclim_m_all_train && !is.null(bioclim_model_m_all_train())){

          vars <- names(bioclim_model_m_all_train()$train@min)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_bioclim_m_proj_trained_all.asc")

          model <- bioclim_model_m_all_train()$prediction
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))

        }
      },silent = TRUE)

      # Save raster all m train Bioclim models

      try({

        if(input$run_bioclim_all_m_train && !is.null(bioclim_model_all_m_train())){

          vars <- names(bioclim_model_all_m_train()$train@min)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_bioclim_all_proj_trained_m.asc")

          model <- bioclim_model_all_m_train()$prediction
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))

        }
      },silent = TRUE)

      # Save raster m m train Bioclim models

      try({

        if(input$run_bioclim_m_m_train && !is.null(bioclim_model_m_m_train())){

          vars <- names(bioclim_model_m_m_train()$train@min)
          mod_name <- paste0(paste(vars,
                                   collapse = "_"),
                             "_bioclim_m_proj_m_all.asc")

          model <- bioclim_model_m_m_train()$prediction
          raster::writeRaster( model, file.path(models_dir_path,
                                                mod_name))

        }
      },silent = TRUE)


    }

  }
})

# -----------------------------------------------------------------------
# Observer for writing to the Workflow: Model evaluation report
# ---------------------------------------------------------------------


observeEvent(input$saveState, {

  if(nchar(workflowDir()) > 0L &&
     length(workflowDir())>0L){

    if(!is.null(partialRoc()) || !is.null(threshold_search2()) ||
       !is.null(mtp_threshold()) || (input$compBin &&
                                     !is.null(binary_user_method())) ||
       (input$run_binomial && !is.null(binomial_testDF())) ||
       !is.null(percentil_threshold())){
      wf_dir_path <- file.path(workflowDir(),
                               "ntbox_workflowReport")

      model_eval_report_path <- system.file("shinyApp/ntb_report/model_eval.Rmd",
                                            package = "ntbox")

      meval_report_save <- file.path(wf_dir_path,"model_eval_report.html")

      meval_dir <- file.path(workflowDir(),"ntbox_model_eval_files")
      if(!dir.exists(meval_dir)) dir.create(meval_dir)


      rmarkdown::render(input = model_eval_report_path,
                        output_format = html_document(pandoc_args = c("+RTS",
                                                                      "-K64m",
                                                                      "-RTS"),
                                                      self_contained = FALSE,
                                                      highlight="haddock",
                                                      toc = TRUE,
                                                      theme = "readable"),
                        output_file = meval_report_save)


      if(!is.null(partialRoc())){

        proc_path <- file.path(meval_dir,"proc_results.csv")

        write.csv(partialRoc(),proc_path,row.names = FALSE)
        proc_stats_path <- file.path(meval_dir,"proc_stats.txt")

        capture.output(pRocStats(),file = proc_stats_path)

      }

      if(!is.null(threshold_search2())){
        th_optim_path <- file.path(meval_dir,"confusion_matrix_optim.csv")
        meta_optim_path <- file.path(meval_dir,"confusion_matrix_optim_metadata.csv")

        th_optim <- round( meta_data_cm_method()$cutoff_threshold,4)
        raster_optim_path <- file.path(meval_dir,
                                       paste0("bin_cmatrix_optim_th_",
                                              th_optim,".asc"))
        write.csv(threshold_search2(),th_optim_path,row.names = FALSE)
        write.csv(meta_data_cm_method(),meta_optim_path,row.names = FALSE)
        try({
          raster::writeRaster(binary_cm_method(),raster_optim_path)
        },silent = TRUE)
      }
      if(!is.null(mtp_threshold())){
        meta_mtp_path <- file.path(meval_dir,"mtp_metadata.csv")

        write.csv(meta_data_mtp_method(),meta_mtp_path,row.names = FALSE)

        th_mtp <- round( meta_data_mtp_method()$cutoff_threshold,8)

        raster_mtp_path <- file.path(meval_dir,
                                       paste0("bin_mtp_th_",
                                              th_mtp,".asc"))
        try({
          raster::writeRaster(binary_mtp_method(),raster_mtp_path)
        },silent = TRUE)
      }
      if(!is.null(percentil_threshold())){
        meta_percentil_path <- file.path(meval_dir,"user_percentil_metadata.csv")

        write.csv(meta_data_percentil_method(),meta_percentil_path,row.names = FALSE)

        th_percentil <- round( meta_data_percentil_method()$cutoff_threshold,8)

        raster_percentil_path <- file.path(meval_dir,
                                     paste0("bin_user_percentil_th_",
                                            input$percentil_th,"_",
                                            th_percentil,".asc"))
        try({
          raster::writeRaster(binary_percentil_method(),raster_percentil_path)
        },silent = TRUE)
      }

      if(input$compBin && !is.null(binary_user_method())){

        meta_udt_path <- file.path(meval_dir,"user_threshold_metadata.csv")

        write.csv(meta_data_user_method(),meta_udt_path,row.names = FALSE)

        th_udt <- round( meta_data_user_method()$cutoff_threshold,4)
        raster_udt_path <- file.path(meval_dir,
                                     paste0("bin_user_th_",
                                            th_udt,".asc"))

        try({
          raster::writeRaster(binary_user_method(),raster_udt_path)
        },silent = TRUE)

      }

      if(input$run_binomial && !is.null(binomial_testDF())){
        bino_results_path <- file.path(meval_dir,
                                       "binomial_test_results.csv")
        bino_coords_path <- file.path(meval_dir,
                                      "binomial_coordinates_values.csv")

        write.csv(binomial_testDF()$results_bin,bino_results_path,
                  row.names = FALSE)

        write.csv(binomial_testDF()$coords_df,bino_coords_path,
                  row.names = FALSE)

      }

    }
  }

})


# -----------------------------------------------------------------------
# Observer for writing to the Workflow: Model evaluation report
# ---------------------------------------------------------------------


observeEvent(input$saveState, {
  if(nchar(workflowDir()) > 0L &&
     length(workflowDir())>0L){

    if((input$run_mop && !is.null(mop_comp())) ||
       (input$run_mess && !is.null(mess_comp())) ||
       (input$run_nt1 && !is.null(exdet_univar_comp())) ||
       (input$run_nt2 && !is.null(exdet_multvar_comp()))){



      wf_dir_path <- file.path(workflowDir(),
                               "ntbox_workflowReport")

      erisk_report_path <- system.file("shinyApp/ntb_report/extrapolation_risk.Rmd",
                                            package = "ntbox")

      erisk_report_save <- file.path(wf_dir_path,"extrapolation_risk_report.html")

      erisk_dir <- file.path(workflowDir(),"ntbox_extrapolation_risk_files")
      if(!dir.exists(erisk_dir)) dir.create(erisk_dir)


      rmarkdown::render(input = erisk_report_path,
                        output_format = html_document(pandoc_args = c("+RTS",
                                                                      "-K64m",
                                                                      "-RTS"),
                                                      self_contained = FALSE,
                                                      highlight="haddock",
                                                      toc = TRUE,
                                                      theme = "readable"),
                        output_file = erisk_report_save)

      if(input$run_mop && !is.null(mop_comp())){
        percent <- as.numeric(as.character(input$ref_percent))
        mop_raw_file <- file.path(erisk_dir,paste0("mop_raw_values_percent_",
                                               percent,".asc"))
        mop_norm_file <- file.path(erisk_dir,paste0("mop_normalized_values_percent_",
                                                   percent,".asc"))

        try({
          raster::writeRaster(mop_comp()[[1]],mop_raw_file)
        },silent = TRUE)

        try({
          raster::writeRaster(mop_comp()[[2]],mop_norm_file)
        },silent = TRUE)

      }
      if(input$run_mess && !is.null(mess_comp())){
        mess_raw_file <- file.path(erisk_dir,"mess_raster.asc")
        try({
          raster::writeRaster(mess_comp(),mess_raw_file)
        },silent = TRUE)
      }
      if(input$run_nt1 && !is.null(exdet_univar_comp())){
        exdet_1_file <- file.path(erisk_dir,"exdet_univarite.asc")
        try({
          raster::writeRaster(exdet_univar_comp(),exdet_1_file)
        },silent = TRUE)
      }
      if(input$run_nt2 && !is.null(exdet_multvar_comp())){
        exdet_2_file <- file.path(erisk_dir,"exdet_multivariate.asc")
        try({
          raster::writeRaster(exdet_multvar_comp(),exdet_2_file)
        },silent = TRUE)
      }

    }
  }
})
