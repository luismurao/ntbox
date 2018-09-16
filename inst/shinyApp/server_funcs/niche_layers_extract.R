source("server_funcs/data_gbif.R",local = TRUE)
source("server_funcs/data_user.R",local=TRUE)

observe({
  if(!is.null(data_user_clean())){

    updateSelectInput(session,"datasetM","Select the data set you want to work with",
                      choices = c("User data"="updata",
                                  #"M data from dynamic map" = "M_data",
                                  "Just clip my layers"="clipLayers"), selected ="updata")
  }
  if(!is.null(data_gbif()) && is.null(data_user_clean())){
    updateSelectInput(session,"datasetM","Select the data set you want to work with",
                      choices = c("GBIF data"="gbif_dat",
                                  #"M data from dynamic map" = "M_data",
                                  "Just clip my layers"="clipLayers"), selected ="gbif_dat")
  }
  if(!is.null(data_user_clean()) && !is.null(data_gbif())){
    updateSelectInput(session,"datasetM","Select the data set you want to work with",
                      choices = c("GBIF data"="gbif_dat",
                                  "User data"="updata",
                                  #"M data from dynamic map" = "M_data",
                                  "Just clip my layers"="clipLayers"), selected ="gbif_dat")
  }

})

# Reactive to extract values of niche layers

define_M_raster <- reactive({

  if(!is.null(myPolygon()) && !is.null(rasterLayers())){
    M_raster <- raster::crop(rasterLayers(),myPolygon())
    M_raster <- raster::mask(M_raster,myPolygon())
    return(M_raster)
  }
  else
    return(NULL)

})

# Select a geograpghic dataset

data_to_extract <- reactive({

  if(input$datasetM == "gbif_dat" && !is.null(data_gbif()))
    return(data_gbif()[,c(input$xLongitudeGBIF,input$yLatitudeGBIF)])
  if(input$datasetM == "gbif_dat" && !is.null(data_gbif()) && input$extracted_area == "polygon_of_M")
    return(data_poly()[,c(input$xLongitudeGBIF,input$yLatitudeGBIF)])
  if(input$datasetM == "updata" && !is.null(data_user_clean()))
    return(data_user_clean()[,c(input$xLongitudeUser,input$yLatitudeUser)])
  if(input$datasetM == "updata" && !is.null(data_user_clean()) && input$extracted_area == "polygon_of_M")
    return(data_poly()[,c(input$xLongitudeUser,input$yLatitudeUser)])
  else
    return(NULL)
})


occ_extract_from_mask <- eventReactive(input$run_extract,{

  if(!is.null(define_M_raster()) && !is.null(data_to_extract())){

    data_env <- data.frame(raster::extract(define_M_raster(),
                                           data_to_extract()))
    data_env_xy <- cbind( data_to_extract(),data_env)
    data_env_xy <- na.omit(data_env_xy)
    data_env <- data_env_xy[,-(1:2)]
    xy_data <-data_env_xy[ ,1:2]
    xy_data_index <- attr(data_env_xy,"na.action")


    #if(length(xy_data_index)>0L){
    #  print(xy_data_index )
    #  xy_data <-data_env_xy[xy_data_index ,1:2]
    #  data_env_xy  <- data.frame(xy_data,data_env[data_env_xy,])

    #}
    #else{
    #  xy_data <-data_env_xy[ ,1:2]
    #  data_env_xy  <- data.frame(xy_data,data_env)
    #  data_env <- data_env_xy[,-c(1,2)]
    #}


    return(list(data=data_env,
                xy_data=xy_data,
                xy_data_index=xy_data_index,
                data_env_xy=data_env_xy))
  }
  else
    return(NULL)
})


occ_extract <- eventReactive(input$run_extract,{
  if(!is.null(data_to_extract()) && !is.null(rasterLayers())){
    data_env <- data.frame(raster::extract(rasterLayers(),
                                             data_to_extract()))
    data_env <- na.omit(data_env)
    xy_data_index <- attr(data_env,"na.action")

    if(length(xy_data_index)>0L){
      xy_data <- data_to_extract()[-xy_data_index,]
      data_env_xy  <- data.frame(data_env, xy_data)

    }
    else{
      data_env_xy  <- data.frame(data_to_extract(),data_env)
      data_env_xy <- na.omit(data_env_xy)
      data_env <- data_env_xy[,-c(1,2)]
      xy_data <- data_env_xy[,c(1,2)]
    }

    return(list(data=data_env,xy_data=xy_data,
                xy_data_index=xy_data_index))

  }
  else
    return(NULL)

})




data_extraction <- reactive({
  if(input$extracted_area == "all_area" && !is.null(occ_extract()))
    return(occ_extract()$data)
  if(input$extracted_area == "polygon_of_M" && !is.null(occ_extract_from_mask()))
    return(occ_extract_from_mask()$data)
  else
    return(NULL)
})



output$dataM <- renderDataTable({
  if(is.null(rasterLayers())) {
    message <- "Load niche layers in AppSettings section"
    data_niche <- data.frame(No_Data = message)
    return(data_niche)
  }
  else if(!is.null(rasterLayers()) && input$extracted_area == "all_area"){
    if(input$datasetM == "gbif_dat" && is.null(data_gbif())){
      message <- "You need to have GBIF data! Search GBIF data in section: Data -> GBIF data"
      data_niche <- data.frame(No_Data = message)
      return(data_niche)
    }
    if(input$datasetM == "updata" && is.null(data_user_clean())){
      message <- "You need to upload your occurrence data! Upload your data in section: Data -> User data"
      data_niche <- data.frame(No_Data = message)
      return(data_niche)
    }
  }
  else if(input$extracted_area == "polygon_of_M" && is.null(define_M_raster())){
    menssage <- "You need to define your polygon of M in section: Data -> Dynamic map -> Define a polygon of M"
    data_niche <- data.frame(No_Data = message)
    return(data_niche)
  }
  else if(input$extracted_area == "polygon_of_M" && !is.null(define_M_raster())){
    if(input$datasetM == "gbif_dat" && is.null(data_gbif())){
      message <- "You need to have GBIF data! Search GBIF data in section: Data -> GBIF data"
      data_niche <- data.frame(No_Data = message)
      return(data_niche)
    }
    if(input$datasetM == "updata" && is.null(data_user_clean())){
      message <- "You need to upload your occurrence data! Upload your data in section: Data -> User data"
      data_niche <- data.frame(No_Data = message)
      return(data_niche)
    }
  }
  if((!is.null(data_user_clean()) || !is.null(data_gbif())) && (!is.null(define_M_raster()) || !is.null(rasterLayers())) && is.null(data_extraction())){
    message <- "Press Go!!! button"
    data_niche <- data.frame(Press_Go = message)
    return(data_niche)
  }
  if(!is.null(data_extraction())){
    data_niche <- data_extraction()
    return(data_niche)
  }


},
options = list(aLengthMenu = c(5, 10, 25,
                               50, 100, 500),
               iDisplayLength = 10))




#output$raster_sample <- renderPlot({

#  if(!is.null(rasterLayers())){
#    if(input$extracted_area == "all_area" && !is.null(data_to_extract())){
#      plot(rasterLayers()[[1]])
#    }

#    if(input$extracted_area == "polygon_of_M" && !is.null(data_to_extract()) && !is.null(define_M_raster())){
#      plot(define_M_raster()[[1]])
#    }
#    if(input$extracted_area == "polygon_of_M" && !is.null(data_to_extract()) && is.null(define_M_raster())){
#      messages <- "Define M area using dynamic Map"
#      x <- -10:10
#      y <- x
#      plot(x,y,type="n", xlab="No Data", ylab="No data")
#      text(0,0,messages,cex=3 )
#    }

#  }
#  else{
#    messages <- "Load niche layers in App settings section"
#    x <- -10:10
#    y <- x
#    plot(x,y,type="n", xlab="No Data", ylab="No data")
#    text(0,0,messages,cex=3 )
#  }

#})

# Download data table with niche vars values

output$downloadExtraction <- downloadHandler(
  filename = function() return(paste0(input$genus,"_",input$species,"niche_data.csv")),
  content = function(file) {
    if(!is.null(data_extraction())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(data_extraction(),file,row.names = FALSE)
    }
  }
)


# Download clipped rasters

tar_clippedRas <- reactive({
  if(!is.null(define_M_raster())){
    tempfilePath <- tempfile(pattern = "clippedRaster")
    temdirRas <- dir.create(tempfilePath)
    raslayers <- define_M_raster()
    layernames <- names(raslayers)
    filenames <- paste0(tempfilePath,"/",layernames,".asc")
    sapply(1:length(filenames), function(x) writeRaster(raslayers[[x]],filename = filenames[x],overwrite=TRUE))
    tarRas <- paste0(tempfilePath,"/clippedRaster.tar")
    utils::tar(tarRas,tempfilePath,compression='gzip')
    return(tarRas)
  }
  else
    return(NULL)
})

output$downClippedR <- downloadHandler(
  filename = function() return(paste0(input$genus,"_",input$species,"M_rasters.tar")),
  content = function(file) {
    if(!is.null(tar_clippedRas())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      file.copy(tar_clippedRas(), file)
    }
  }
)
