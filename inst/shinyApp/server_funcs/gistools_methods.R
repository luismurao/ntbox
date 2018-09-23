leafMapDynamic_gis <- reactive({
  # Draw map leaflet map

  map <- leaflet::leaflet() %>%
    leaflet::addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>% setView(lng = 0, lat = 0, zoom = 3) %>%
    leaflet.extras::addDrawToolbar(
      targetGroup='draw',
      editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions()))
  return(map)
})


observe({
  wf_directory <- workflowDir()

  if(!is.null(rasterLayers()) && length(nchar(wf_directory))>0L){

    updateSelectInput(session,"layers2pc",
                      choices = names(rasterLayers()),
                      selected = names(rasterLayers()))

    updateSelectInput(session, "layers2reformat",
                      choices = c("Niche layers"="nicheLayersReformat"),
                      selected ="nicheLayersReformat")

    updateSelectInput(session, "layers2CropMask",
                      choices = c("Niche layers"="nicheLayersCM"),
                      selected ="nicheLayersCM" )
  }

  if(!is.null(proj_rasterLayers()) && length(nchar(wf_directory))>0L){
    updateSelectInput(session,"layers2proj",
                      choices = names(proj_rasterLayers()),
                      selected = names(proj_rasterLayers()))

    updateSelectInput(session, "layers2reformat",
                      choices = c("Projection layers"="projLayersReformat"),
                      selected ="projLayersReformat")

    updateSelectInput(session, "layers2CropMask",
                      choices = c("Projection layers"="projLayersCM"),
                      selected = "projLayersCM")


  }
  if(!is.null(rasterLayers()) && !is.null(proj_rasterLayers()) && length(nchar(wf_directory))>0L){
    updateSelectInput(session, "layers2reformat",
                      choices = c("Niche layers"="nicheLayersReformat",
                                  "Projection layers"="projLayersReformat"))

    updateSelectInput(session, "layers2CropMask",
                      choices = c("Niche layers"="nicheLayersCM",
                                  "Projection layers"="projLayersCM"))

  }


})

output$reformatDir <- renderUI({
  wf_directory <- workflowDir()
  if((!is.null(proj_rasterLayers()) || !is.null(rasterLayers())) && length(nchar(wf_directory))>0L)
    textInput("reformatDirectory",
              "Name the directory where the layers will be saved")
})



reformatLayers <- eventReactive(input$go_reformat,{
  wf_directory <- workflowDir()


  if(nchar(input$reformatDirectory)==0L){

    to_wr_layers <- file.path(wf_directory,"NicheLayersnewFormat")
    to_wr_projLayers <- file.path(wf_directory,"ProjLayersnewFormat")


  }
  else{
    to_wr_layers <- file.path(wf_directory,input$reformatDirectory)
    to_wr_projLayers <- file.path(wf_directory,paste0(input$reformatDirectory,"_","ProjLayers"))
  }

  if(all(c("nicheLayersReformat", "projLayersReformat") %in% input$layers2reformat)){


    n_nichelayers <- 1:length(names(rasterLayers()))
    n_projlayers <- 1:length(names(proj_rasterLayers()))



    if(!dir.exists(to_wr_layers))
      dir.create(to_wr_layers)
    if(!dir.exists(to_wr_projLayers))
      dir.create(to_wr_projLayers)

    nicheLayersPath <- file.path(to_wr_layers,
                                 paste0(names(rasterLayers()),
                                        input$raster_format))

    projLayersPath <- file.path(to_wr_projLayers,
                                paste0(names(proj_rasterLayers()),
                                       input$raster_format))


    n_nichelayers %>% purrr::map(~raster::writeRaster(rasterLayers()[[.x]],
                                                      nicheLayersPath[.x], overwrite = TRUE))

    cat("Niche Layers written in ", to_wr_layers, " directory\n")

    n_projlayers %>% purrr::map(~raster::writeRaster(proj_rasterLayers()[[.x]],
                                                     projLayersPath[.x], overwrite = TRUE))


    cat("Projection Layers written in ", to_wr_projLayers, " directory\n")
    return()


  }


  if( input$layers2reformat== "nicheLayersReformat"){


    n_nichelayers <- 1:length(names(rasterLayers()))



    if(!dir.exists(to_wr_layers))
      dir.create(to_wr_layers)


    nicheLayersPath <- file.path(to_wr_layers,
                                 paste0(names(rasterLayers()),
                                        input$raster_format))


    n_nichelayers %>% purrr::map(~raster::writeRaster(rasterLayers()[[.x]],
                                                      nicheLayersPath[.x], overwrite = TRUE))

    cat("Niche Layers written in ", to_wr_layers, " directory\n")

    return()


  }

  if( input$layers2reformat== "projLayersReformat"){

    n_projlayers <- 1:length(names(proj_rasterLayers()))

    if(!dir.exists(to_wr_projLayers))
      dir.create(to_wr_projLayers)


    projLayersPath <- file.path(to_wr_projLayers,
                                paste0(names(proj_rasterLayers()),
                                       input$raster_format))


    n_projlayers %>% purrr::map(~raster::writeRaster(proj_rasterLayers()[[.x]],
                                                     projLayersPath[.x], overwrite = TRUE))

    cat("Projection Layers written in ", to_wr_projLayers, " directory\n")

    return()
  }
})

# Change layer format

output$printLayersInfo <- renderPrint({
  reformatLayers()
})


output$check_projLayers <- renderUI({

  if(!is.null(input$rds_file)){
    pca_obj <- try(readRDS(input$rds_file$datapath))
  }
  else
    pca_obj <- NULL

  if(!is.null(proj_rasterLayers()) && (!is.null(pca_obj) || !is.null(rasterLayers()))){
    checkboxInput("run_pca_proj",label = "Compute PCA projection",value = TRUE)
  }
})

# Dynamic map to select polygon

output$dyMap_gis <- renderLeaflet({
  return(leafMapDynamic_gis())
})

pcas_layers <- eventReactive(input$dopca,{

  pca_format <- input$pca_format
  wf_directory <- file.path(workflowDir(),"pca_current")
  if(!dir.exists(wf_directory))
    dir.create(wf_directory)
  layers_selected <- input$layers2pc
  if(nchar(input$projLayers_wd)>0L){
    wf_directory_proj <- file.path(workflowDir(),input$projLayers_wd)
  }

  if(!is.null(input$rds_file)){
    pca_obj <- try(readRDS(input$rds_file$datapath))
  }
  else
    pca_obj <- NULL

  if(!is.null(rasterLayers())){
    layers_stack <- raster::stack(rasterLayers())
    layers_stack <- layers_stack[[layers_selected]]
  }

  if((!is.null(rasterLayers()) || !is.null(pca_obj)) && nchar(wf_directory)>2L){

    if(!is.null(proj_rasterLayers())){
      projLayers <- proj_rasterLayers()[[input$layers2proj]]
    }
    else
      projLayers <- NULL


    pca <- ntbox::spca(layers_stack = layers_stack,
                       layers_to_proj = projLayers,
                       pca_obj =  pca_obj,
                       sv_dir = wf_directory,
                       layers_format = pca_format,
                       sv_proj_dir = wf_directory_proj)
    return(pca)
  }
  return()
})

output$pca_plot <- renderPlot({
  if(!is.null(pcas_layers)){
    print(pcas_layers()$pca_plot)
  }
})

# Create a polygon to perform CROP and Mask operations


#observeEvent(input$dyMap_gis_draw_all_features, {
#print("All Features")
#print(input$leafmap_draw_all_features)
#  saveRDS(input$dyMap_gis_draw_all_features,"~/Dropbox/leafletDraw/leafletDrae_all.rds")

#})

gtoolsPolygon <- reactive( {
  l_featuers <- input$dyMap_gis_draw_all_features
  if(!is.null(l_featuers)){
    ntb_polygons <- ldraw2sp(leaflet_draw = l_featuers)
    if(!is.character(ntb_polygons))
      ntb_polygons$area_sqkm <- raster::area(ntb_polygons) / 1000000
    return(ntb_polygons)
  }

})



# Read polygons

#gtoolsPolygon <- reactive({
# Create polygon using leaflet maps
#  if(!is.null(input$geojson_coords)){
#    map <- readOGR(input$geojson_coords,"OGRGeoJSON")
#    return(map)
#  }
#  else
#    return(NULL)

#})


output$coords_gis_polygon <- renderPrint({
  if(!is.null(gtoolsPolygon())){
    print(gtoolsPolygon())
  }
})



output$go_MaskCrop <- renderUI({
  wf_directory <- workflowDir()
  if((!is.null(rasterLayers()) || !is.null(proj_rasterLayers())) && length(nchar(wf_directory))){
    actionButton("go_crop_mask",label = "Save layers",styleclass="primary")
  }
})


crop_layers <- reactive({

  if(!is.null(rasterLayers()) && input$layers2CropMask == "nicheLayersCM" && (input$CropMask=="gis_crop" || input$CropMask=="gis_mask") && class(gtoolsPolygon())=="SpatialPolygonsDataFrame")
    ntb_crop <- raster::stack(raster::crop(rasterLayers(),
                                           gtoolsPolygon()))
  else
    ntb_crop <- NULL
  return(ntb_crop)

})


mask_layers <- reactive({
  if(!is.null(crop_layers()) && input$CropMask == "gis_mask"){
    ntb_mask <- raster::stack(raster::mask(crop_layers(),
                                           gtoolsPolygon()))
  }
  else{
    ntb_mask <- NULL
  }
  return(ntb_mask)

})


crop_layers_proj <- reactive({

  if(!is.null(rasterLayers()) && input$layers2CropMask == "projLayersCM" && (input$CropMask=="gis_crop" || input$CropMask=="gis_mask") && class(gtoolsPolygon())=="SpatialPolygonsDataFrame")
    ntb_crop <- raster::stack(raster::crop(proj_rasterLayers(),
                                           gtoolsPolygon()))
  else
    ntb_crop <- NULL
  return(ntb_crop)

})

mask_layers_proj <- reactive({

  if(!is.null(rasterLayers()) && input$layers2CropMask == "projLayersCM" && input$CropMask=="gis_mask" && class(gtoolsPolygon())=="SpatialPolygonsDataFrame"){
    ntb_mask <- raster::stack(raster::mask(crop_layers_proj(),
                                           gtoolsPolygon()))
  }
  else
    ntb_mask <- NULL
  return(ntb_mask)

})

#output$save_cuts <- renderUI({
#  wf_directory <- workflowDir()
#  if(length(nchar(wf_directory))>2L && class(gtoolsPolygon())=="SpatialPolygonsDataFrame"){
#    actionButton(inputId = "save_gtoolspoly",label = "Save")
#  }
#})

# Observer (saves polygon when user click action button)

observeEvent(input$save_gtoolspoly,{
  # Save polygon
  if(class(gtoolsPolygon()) =="SpatialPolygonsDataFrame" && !is.null(workflowDir()) && nchar(workflowDir()) > 0L){
    file_dir <- file.path(workflowDir(),"ntb_gistools_polygon")
    if(!dir.exists(file_dir))
      dir.create(file_dir)
    poly_name <- paste0("polygon_sv_",
                        format(Sys.time(),
                               "%y_%d_%m"))
    writeOGR(gtoolsPolygon(), file_dir, poly_name, driver="ESRI Shapefile",overwrite_layer = T)

  }
})




observeEvent(input$go_crop_mask,{


  if(class(gtoolsPolygon()) =="SpatialPolygonsDataFrame" && !is.null(workflowDir()) && nchar(workflowDir()) > 0L){



    if(class(crop_layers())=="RasterStack" && input$layers2CropMask == "nicheLayersCM" && input$CropMask =="gis_crop"){


      save_dir <- file.path(workflowDir(),
                            "ntbox_nicheLayersCroped")


      if(!dir.exists(save_dir))
        dir.create(save_dir)
      layer_files <- file.path(save_dir,
                               paste0(names(crop_layers()),
                                      input$raster_format2))
      1:length(layer_files) %>%
        purrr::map(~writeRaster(crop_layers()[[.x]],
                                layer_files[.x],overwrite=TRUE))
    }

    if(class(mask_layers())=="RasterStack" && input$layers2CropMask == "nicheLayersCM" && input$CropMask =="gis_mask"){

      save_dir <- file.path(workflowDir(),
                            "ntbox_nicheLayersMasked")

      if(!dir.exists(save_dir))
        dir.create(save_dir)
      layer_files <- file.path(save_dir,
                               paste0(names(mask_layers()),
                                      input$raster_format2))
      1:length(layer_files) %>%
        purrr::map(~writeRaster(mask_layers()[[.x]],
                                layer_files[.x],overwrite=TRUE))
    }

    if(class(crop_layers_proj())=="RasterStack" && input$layers2CropMask == "projLayersCM" && input$CropMask =="gis_crop"){

      save_dir <- file.path(workflowDir(),
                            "ntbox_ProjLayersCroped")
      if(!dir.exists(save_dir))
        dir.create(save_dir)
      layer_files <- file.path(save_dir,
                               paste0(names(crop_layers_proj()),
                                      input$raster_format2))
      1:length(layer_files) %>%
        purrr::map(~writeRaster(crop_layers_proj()[[.x]],
                                layer_files[.x],overwrite=TRUE))
    }

    if(class(crop_layers_proj())=="RasterStack" && input$layers2CropMask == "projLayersCM" && input$CropMask =="gis_mask"){

      save_dir <- file.path(workflowDir(),
                            "ntbox_ProjLayersMasked")
      if(!dir.exists(save_dir))
        dir.create(save_dir)
      layer_files <- file.path(save_dir,
                               paste0(names(mask_layers_proj()),
                                      input$raster_format2))
      1:length(layer_files) %>%
        purrr::map(~writeRaster(mask_layers_proj()[[.x]],
                                layer_files[.x],overwrite=TRUE))
    }


  }
})




