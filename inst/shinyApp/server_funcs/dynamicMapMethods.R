source("server_funcs/data_gbif.R",local = TRUE)
source("server_funcs/data_user.R",local=TRUE)
# Reactive function (returns a dataset selected by the user)

leafMapDynamic_base <- reactive({
  # Draw map leaflet map

  map <- leaflet::leaflet() %>%
    leaflet::addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
      attribution =  'Tiles courtesy of the <a href="https://usgs.gov/">U.S. Geological Survey</a>'
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>% setView(lng = 0, lat = 0, zoom = 3) %>%
    leaflet.extras::addDrawToolbar(
      targetGroup='draw0',
      #position = T,
      polygonOptions = T,
      circleOptions =FALSE,
      rectangleOptions =TRUE,
      markerOptions =FALSE,
      singleFeature =FALSE,
      circleMarkerOptions	= FALSE,
      #editOptions =FALSE
      editOptions = leaflet.extras::editToolbarOptions(selectedPathOptions = leaflet.extras::selectedPathOptions())
      )

  #if(input$dataset_dynMap=="gbif_dataset" && input$search_gbif_data==0) return(map)

  #if(!is.null(myPolygon_gbif()))

  #  map <- map %>% addPolygons(data=myPolygon_user(),color = "red")

  return(map)
})



leafMapDynamic <- reactive({


  map <- leafMapDynamic_base()


  if(input$dataset_dynMap == "gbif_dataset" &&
     input$search_gbif_data == 0)
    return(map)
  if(is.null(data_set()))
    return(map)


  if(input$dataset_dynMap == "gbif_dataset" &&
     !is.null(dataDynamic())){
    map_info <- paste0(dataDynamic()$leaflet_info,
                       "<br/><b>Map ID: </b> ",
                       dataDynamic()$dataID)


  } else{
    map_info <- dataDynamic()$dataID
  }
  if(input$define_M == 0 && is.data.frame(dataDynamic())){


    map <- map %>%
      addCircleMarkers(lng = dataDynamic()[,data_set()$longitude],
                       lat = dataDynamic()[,data_set()$latitude],
                       popup = map_info)

    if(class(myPolygon()) == "SpatialPolygonsDataFrame" && input$define_M){
      map <- map %>% addPolygons(data=myPolygon(),col="darkgreen")
    }

    return(map)

  }
  if(input$define_M == 1 && input$points_in_poly > 0L && is.data.frame(data_poly())){

    if(dim(data_poly())[1] > 0L){
      map <- leafMapDynamic_base()
      if(class(myPolygon()) == "SpatialPolygonsDataFrame" && input$define_M){
        map <- map %>% addPolygons(data=myPolygon(),col="darkgreen")
      }
      if(input$dataset_dynMap == "gbif_dataset" &&
         !is.null(data_poly())){
        map_info <- paste0(data_poly()$leaflet_info,
        "<br/><b>Map ID: </b> ",data_poly()$dataID)
      } else{
        map_info <- data_poly()$dataID
      }

      map <- map %>%
        addCircleMarkers(lng = data_poly()[,data_set()$longitude],
                         lat = data_poly()[,data_set()$latitude],
                         popup = map_info)

    }
    return(map)
  }
  if(input$define_M == 1 && is.data.frame(dataDynamic()) && input$points_in_poly==0){

    if(input$dataset_dynMap == "gbif_dataset" &&
       !is.null(dataDynamic())){
      map_info <- paste0(dataDynamic()$leaflet_info,
                         "<br/><b>Map ID: </b> ",
                         dataDynamic()$dataID)
    } else{
      map_info <- dataDynamic()$dataID
    }

    map <- map %>%
      addCircleMarkers(lng = dataDynamic()[,data_set()$longitude],
                 lat = dataDynamic()[,data_set()$latitude],
                 popup = map_info)
    if(class(myPolygon()) == "SpatialPolygonsDataFrame" && input$define_M){
      map <- map %>% addPolygons(data=myPolygon(),col="darkgreen")
    }
    return(map)
  }
  else
    map <- leafMapDynamic_base()


  return(map)

})

# Dynamic map to select polygon

output$dyMap_cas <- renderLeaflet({
  map <- leafMapDynamic()
  return(map)
})


output$polyfeatures <- renderPrint({
  print(myPolygon())
})



data_set <- reactive({

  if(input$dataset_dynMap == "gbif_dataset" && input$search_gbif_data &&
     is.data.frame(data_gbif())){
    data <- data_gbif()
    data$ID_ntb <- 1:dim(data)[1]
    return(list(data=data,
                longitude=input$xLongitudeGBIF,
                latitude=input$yLatitudeGBIF))
  }
  if(input$dataset_dynMap == "user_dataset" && !is.null(data_user_clean())){
    data <- data_user_clean()
    data$ID_ntb <- 1:dim(data)[1]
    return(list(data=data,
                longitude=input$xLongitudeUser,
                latitude=input$yLatitudeUser))
  }
  else
    return(NULL)
})


# Reactive to clean data from dynamic Map

trashDynamic <- reactive({
  if(is.null(data_set())) return(NULL)
  data <- data_set()$data
  if(!is.null(data)){
    data$ID_ntb <- 1:dim(data)[1]
    input$cleanDynamic
    isolate({
      index <- NULL
      index <- c(index,input$pointsDynamic)
      if(is.null(index))
        index <- (1:length(data$ID_ntb))*-1

    })
    return(as.numeric(index))
  }
  else
    return(NULL)

})


dataDynamic <- reactive({
  if(is.null(data_set())) return(NULL)

  dataDM <- data_set()$data

  if(!is.null(dataDM)){

    if(is.null(trashDynamic())){

      # Pop ups
      dataDM$dataID <- paste0("ID: ",dataDM$ID_ntb,
                              "\n Name: ",dataDM$name)

    }

    if(!is.null(trashDynamic())) {

      index <- unlist(sapply(trashDynamic(),
                             function(x)
                               which(x==dataDM$ID_ntb)))

      if(length(index)==0L){
        index <- (1:length(dataDM$ID_ntb))*-1
      }
      dataDM<- dataDM[-index,]

      # Pop ups
      dataDM$dataID <- paste0("ID: ", dataDM$ID_ntb)

    }

    return(dataDM)
  }
  else
    return(NULL)
})

#data_poly <- reactive({
#  dataDM <- dataDynamic()
#  if(!is.null(dataDM)){
#    if(!is.null(myPolygon()) && data_set()$longitude %in% names(dataDM)){
#      input$points_in_poly
#      isolate({
#        if(input$points_in_poly > 0 ){
#          sp_data_frame <- SpatialPointsDataFrame(dataDM[,c(data_set()$longitude,
#                                                            data_set()$latitude)],dataDM)
#          proj4string(sp_data_frame) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
#          poly_user <- spTransform(myPolygon(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#          data_poly <- over( sp_data_frame , poly_user , fn = NULL)
#          ids_poly <- which(!is.na(data_poly[,1]))
#          dataDM <- dataDM[ids_poly,]
#
#        }
#      })
#    }
#  }
#  return(dataDM)
#})


data_poly <- eventReactive(input$points_in_poly,{

  dataDM <- dataDynamic()
  #if(!is.null(dataDM)){
    if(!is.null(myPolygon()) && data_set()$longitude %in% names(dataDM)){
      sp_data_frame <- SpatialPointsDataFrame(dataDM[,c(data_set()$longitude,
                                                        data_set()$latitude)],dataDM)

      proj4string(sp_data_frame) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      poly_user <- spTransform(myPolygon(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
      data_poly <- over( sp_data_frame , poly_user , fn = NULL)
      ids_poly <- which(!is.na(data_poly[,1]))
      dataDM <- dataDM[ids_poly,]

      return(dataDM)
    }
  #}
  return(dataDM)
})

output$downDatDyn <- downloadHandler(
  filename = function() "data_dynamicMap.csv",
  content = function(file) {
    if(!is.null(dataDynamic())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(dataDynamic(),file=file,row.names=FALSE)
    }
  }
)



# History data of reported records

dataTS <- reactive({

  if(!is.null(dataDynamic()) && input$define_M == 0)
    data <- dataDynamic()[,input$yearVarDynamic]
  if(!is.null(dataDynamic()) && input$define_M == 1)
    data <- data_poly()[,input$yearVarDynamic]
  if(!is.null(data)){
    ff <- data.frame(table(data),stringsAsFactors = FALSE)
    names(ff) <- c("year","n")
    ff$year <- as.numeric(as.character(ff$year))
    dts <- ts(ff$n,start = min(ff$year),end = max(ff$year),frequency = 1)
    return(list(dts=dts,ff=ff))
  }
  else
    return()

})

# History plot of reported records

output$RecordsHistDynamic <- renderDygraph({

  if(!is.null(dataTS())){

    dts <- dataTS()$dts
    ff <- dataTS()$ff
    if(input$plotTSDynamic){
      tsPlot <-dygraph(dts, main = "Reported records by year",
                       ylab = "nrecords",xlab = "year") %>%
        dyRangeSelector(dateWindow = c(paste0(min(ff$year),"-01-01"),
                                       paste0(max(ff$year),"-01-01"))) %>%
        dySeries(c("V1"), label = "records")
      return(tsPlot)
    }

  }
})



# -----------------------------------------------------------------------
# Observer for cleaning records using the dynamic map
# -----------------------------------------------------------------------

observe({

  if(!is.null(dataDynamic())){

    updateSelectInput(session,"pointsDynamic",
                      # Choice based on reactive data_set
                      choices = data_set()$data$ID_ntb,
                      selected = trashDynamic())
    updateSelectInput(session,"yearVarDynamic",
                      choices = names(dataDynamic()),
                      selected="year")
  }

})




observeEvent(input$define_M,{
  if(class(myPolygon()) == "SpatialPolygonsDataFrame" && input$define_M){
    print_poly <- FALSE
    #print(print_poly)
  }
})

# Observer (saves polygon when user click action button)

observeEvent(input$save_poly,{
  # Save polygon
  if(!is.null(myPolygon()) && nchar(workflowDir()) > 0L){
    data_dir_path <- paste0(workflowDir(),"OccDataNicheToolBox/")
    if(!dir.exists(data_dir_path))
      dir.create(data_dir_path)
    file_dir <- paste0(data_dir_path,"M_Shapefiles_",input$dataset_dynMap)
    if(!dir.exists(file_dir))
      dir.create(file_dir)
    poly_name <- input$polygon_name
    poly_name_ext <- ".shp"
    poly <- paste0(poly_name,poly_name_ext)
    #if(poly %in% list.files(file_dir)){
    #  poly_name <- paste0(poly_name,"B_RandNUM",sample(1:1000,1))
    #}
    writeOGR(myPolygon(), file_dir,
             poly_name,
             driver="ESRI Shapefile",overwrite_layer = T)

  }


})
