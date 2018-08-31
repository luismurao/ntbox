
leafMapDynamic_gis <- reactive({
  # Draw map leaflet map

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>% setView(lng = 0, lat = 0, zoom = 3) %>%
    addDrawToolbar(polyline = FALSE,
                   edit = TRUE,
                   remove = TRUE,
                   polygon = TRUE,
                   rectangle = TRUE,
                   circle = TRUE,
                   marker = FALSE)

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
  }

  if(!is.null(proj_rasterLayers()) && length(nchar(wf_directory))>0L){
    updateSelectInput(session,"layers2proj",
                      choices = names(proj_rasterLayers()),
                      selected = names(proj_rasterLayers()))

    updateSelectInput(session, "layers2reformat",
                      choices = c("Projection layers"="projLayersReformat"),
                      selected ="projLayersReformat")
  }
  if(!is.null(rasterLayers()) && !is.null(proj_rasterLayers()) && length(nchar(wf_directory))>0L){
    updateSelectInput(session, "layers2reformat",
                      choices = c("Niche layers"="nicheLayersReformat",
                                  "Projection layers"="projLayersReformat"))
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
  wf_directory <- workflowDir()
  layers_selected <- input$layers2pc
  if(nchar(input$projLayers_wd)>0L){
    wf_directory_proj <- file.path(wf_directory,input$projLayers_wd)
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
