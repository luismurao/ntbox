
# Methods to project models in geographic space
# using leaflet maps


models_ell_all_all_train <- reactive({
  mod1 <- ellip_model_all_rast_all_train()
  if(!is.null(mod1)){

    models_in_ntb <- c("Ellip proj All extent, Trained all extent"
                       ="Ellipsoid_all_extent_shape_W")

    return(models_in_ntb)


  } else {
    return(NULL)
    }
})



models_ell_all_m_train <- reactive({
  mod2 <- ellip_model_all_rast_m_train()
  if(!is.null(mod2)){
    models_in_ntb <- c("Ellip proj All extent, Trained in M"
                       = "Ellipsoid_all_extent_shape_M")
    return(models_in_ntb)

  } else {
    return(NULL)
  }
})



models_ell_m_all_train <- reactive({
  mod3 <- ellip_model_m_rast_all_train()
  if(!is.null(mod3)){
    models_in_ntb <- c("Ellip proj M extent, Trained in all extent"
                       = "Ellipsoid_m_extent_shape_W")
    return(models_in_ntb)
  } else{
    return()
  }
})



models_ell_m_m_train <- reactive({
  mod4 <- ellip_model_m_rast_m_train()
  if(!is.null(mod4)){
    models_in_ntb <- c("Ellip proj M extent, Trained M extent"
                       = "Ellipsoid_m_extent_shape_M")
    return(models_in_ntb)
  } else{
    return()
  }
})


models_bio_all_all_train <- reactive({
  mod5 <- bioclim_model_all_all_train()
  if(!is.null(bioclim_model_all_all_train())){
    models_in_ntb <- c("BioCl proj all extent, Trained all extent"
                       ="Bioclim_all_extent_shape_W")
    return(models_in_ntb)
  } else{
    return()
  }
})


models_bio_all_m_train <-reactive({
  mod6 <- bioclim_model_all_m_train()
  if(!is.null(mod6)){
    models_in_ntb <- c("BioCl proj all extent, Trained M extent"
                       = "Bioclim_all_extent_shape_M")
    return(models_in_ntb)

  } else{
    return()
  }
})



models_bio_m_all_train <-reactive({
  mod7 <- bioclim_model_m_all_train()
  if(!is.null(mod7)){
    models_in_ntb <- c("BioCl proj M extent, Trained all extent"
                       = "Bioclim_m_extent_shape_W")
    return(models_in_ntb)

  } else{
    return()
  }
})



models_bio_m_m_train <-reactive({
  mod8 <- bioclim_model_m_m_train()
  if(!is.null(mod8)){
    models_in_ntb <- c("BioCl proj M extent, Trained M extent"
                       = "Bioclim_m_extent_shape_M")
    return(models_in_ntb)

  } else{
    return()
  }
})
models <- reactiveValues()
observe({
  if(!is.null(models_ell_all_all_train())){
    models$a <- models_ell_all_all_train()
  }
})

observe({
  if(!is.null(models_ell_all_m_train())){
    models$b <- models_ell_all_m_train()
  }
})

observe({
  if(!is.null(models_ell_m_all_train())){
    models$c <- models_ell_m_all_train()
  }
})

observe({
  if(!is.null(models_ell_m_m_train())){
    models$d <- models_ell_m_m_train()
  }
})

observe({
  if(!is.null(models_bio_all_all_train())){
    models$e <- models_bio_all_all_train()
  }
})

observe({
  if(!is.null(models_bio_all_m_train())){
    models$f <- models_bio_all_m_train()
  }
})

observe({
  if(!is.null(models_bio_m_all_train())){
    models$g <- models_bio_m_all_train()
  }
})

observe({
  if(!is.null(models_bio_m_m_train())){
    models$h <- models_bio_m_m_train()
  }
})

observe({
  models_in_ntb <- reactiveValuesToList(models)
  models_in_ntb <- unlist(unname(models_in_ntb))
  if(!is.null(models_in_ntb)){
    updateSelectInput(session,inputId = "proj_model1",
                      label = "Select Model",choices = models_in_ntb)
  }

})



#output$showMapGo <- renderUI({
#  if(input$proj_model1 == "Ellipsoid_all_extent_shape_W"){
#    return(actionButton("showGo_all_extent_shape_W","Go !!"))
#  }
#  if(input$proj_model1 == "Ellipsoid_all_extent_shape_M"){
#    return(actionButton("showGo_all_extent_shape_M","Go !!"))
#  }

#})

leaf_ellip_all_all_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(ellip_model_all_rast_all_train())){

    model <- ellip_model_all_rast_all_train()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


leaf_ellip_all_m_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(ellip_model_all_rast_m_train())){

    model <- ellip_model_all_rast_m_train()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


leaf_ellip_m_all_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(ellip_model_m_rast_all_train())){

    model <- ellip_model_m_rast_all_train()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


leaf_ellip_m_m_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(ellip_model_m_rast_m_train())){

    model <- ellip_model_m_rast_m_train()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})

leaf_bio_all_all_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(bioclim_model_all_all_train())){

    model <- bioclim_model_all_all_train()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})

leaf_bio_all_m_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(bioclim_model_all_m_train())){

    model <- bioclim_model_all_m_train()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


leaf_bio_m_all_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(bioclim_model_m_all_train())){

    model <- bioclim_model_m_all_train()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


leaf_bio_m_m_train <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(!is.null(bioclim_model_m_m_train())){

    model <- bioclim_model_m_m_train()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


to_plot_model <- reactive({
  if(input$proj_model1 == "Ellipsoid_all_extent_shape_W"){

    map <- leaf_ellip_all_all_train()
    longitude <- occ_extract()$xy_data[,1]
    latitude <-  occ_extract()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Ellipsoid_all_extent_shape_M"){
    map <- leaf_ellip_all_m_train()
    longitude <- occ_extract_from_mask()$xy_data[,1]
    latitude <-  occ_extract_from_mask()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Ellipsoid_m_extent_shape_W"){
    map <- leaf_ellip_m_all_train()
    longitude <- occ_extract()$xy_data[,1]
    latitude <-  occ_extract()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Ellipsoid_m_extent_shape_M"){
    map <- leaf_ellip_m_m_train()
    longitude <- occ_extract_from_mask()$xy_data[,1]
    latitude <-  occ_extract_from_mask()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Bioclim_all_extent_shape_W"){
    map <- leaf_bio_all_all_train()
    longitude <- occ_extract()$xy_data[,1]
    latitude <-  occ_extract()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Bioclim_all_extent_shape_M"){
    map <- leaf_bio_all_m_train()
    longitude <- occ_extract_from_mask()$xy_data[,1]
    latitude <-  occ_extract_from_mask()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Bioclim_m_extent_shape_W"){
    map <- leaf_bio_m_all_train()
    longitude <- occ_extract()$xy_data[,1]
    latitude <-  occ_extract()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }
  else if(input$proj_model1 == "Bioclim_m_extent_shape_M"){
    map <- leaf_bio_m_m_train()
    longitude <- occ_extract_from_mask()$xy_data[,1]
    latitude <-  occ_extract_from_mask()$xy_data[,2]
    if(input$showOccs){
      map <- map %>% leaflet::addCircleMarkers(lng = longitude,
                                               lat = latitude,
                                               color = "red",
                                               opacity = 0.25)
    }
    return(map)
  }

  else{
    map <- leaflet() %>%
      addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
        attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
        #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )
    return(map)
  }

})

output$ras_models <- renderLeaflet({
  to_plot_model()
})

