# Global data frame for GBIF data search
data_gbif_search <- eventReactive(input$search_gbif_data,{

  # Test if the API is working
  #test <- ntbox::searh_gbif_data(genus = "Ambystoma",
  #                               species = "tigrinum",
  #                               occlim = 5,
  #                               writeFile = FALSE)


  #if(is.null(test))
  #  return(0)



  data <- ntbox::searh_gbif_data(genus = input$genus,
                                 species = input$species,
                                 occlim = input$occlim,
                                 writeFile = FALSE)


  if(is.null(data))
    return("No occurrences found")
  return(data)

})


# Global data frame for GBIF data (clean)

data_gbif <- reactive({
  data <- data_gbif_search()
  if(is.data.frame(data)){
    input$clean_dup_gbif

    isolate({
      if(input$clean_dup_gbif){
        longitude <- input$xLongitudeGBIF
        latitude <-  input$yLatitudeGBIF
        threshold <- as.numeric(input$threshold_gbif)
        data_clean <- clean_dup(data,longitude = longitude,
                                latitude = latitude,
                                threshold= threshold)
      }
      else
        return(data)
    })

    # Clean duplicates by grouping variable
    input$clean_dup_gbif_group
    isolate({
      if(input$clean_dup_gbif_group){
        if(!is.null(input$groupLevelsGBIF)){
          selection <- unlist(lapply(input$groupLevelsGBIF,
                                     function(x)
                                       which(x == data_clean[,input$groupGBIF])))

          #data_clean <- data_clean[-selection,]
          data_clean <- clean_dup(data_clean[selection,],
                                  longitude,longitude,
                                  threshold=threshold)
          }
        }
      })
      data_clean$ID_ntb <- 1:dim(data_clean)[1]
      return(data_clean)
    }
    else
      return(NULL)
})


# Show the dimension of species data (number of rows and columns)

output$nRcordsGBIF <- renderPrint({

  if(!is.null(data_gbif())){
    dimen <- dim(data_gbif())
    cat(dimen[1], 'rows and ',dimen[2],' columns')
  }
  else
    cat("Search for a species...")
})


# Display gbif data

output$gbif_table <- renderDataTable({
  df0 <- data_gbif_search()
  df1 <- data_gbif()


  if(is.null(df1) && is.null(df0)){
    warn <- "Enter species genus (or family) and species name in the left panel"
    nulo <- " "
    data_null <- c(warn,nulo)
    data_null <- data.frame(Data=data_null)
    return(data_null)
  }

  if(is.data.frame(df1))
    return(df1)

  # Test if GBIF API is working
  if(df0 == 0){
    warn <- "GBIF API is not working, try later :("
    data_null <- data.frame(Data=warn)
    return(data_null)
  }

  else{
    warn <- ": No ocurrences found"
    dat <- paste(input$genus, input$species, warn,sep=" ")
    nulo <- " "
    no_occ <- c(dat,nulo)
    no_occ <- data.frame(Data=no_occ)
    return(no_occ)
  }

},
options = list(aLengthMenu = c(5, 10, 25,
                               50, 100, 500),
               iDisplayLength = 10))


# Download GBIF data

output$downGBIF <- downloadHandler(
  filename = 'data_GBIF.csv',
  content = function(file) {
    if(!is.null(data_gbif())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(data_gbif(),file=file,row.names=FALSE)
    }
  }
)
#----------------------------------------------------------------------
# GBIF visualizations
#----------------------------------------------------------------------

# GBIF history of reccords and pie chart
GBIF_vis <- reactive({
  if(!is.null(data_gbif())){
    dfGBIF_vis <- occs_history(data_gbif())
    d <- dfGBIF_vis$mot
    gD <- dfGBIF_vis$data[,c("country","year")]
    gD <- gD %>% group_by(country) %>% summarise(count1 = n())
    gD <- gD[,c("country","count1")]
    Pie <- gvisPieChart(gD,options=list(legend="All time %"))
    GT <- gvisMerge(d,Pie, horizontal=TRUE)
    return(list(pie=Pie,motion=d,pieMotion=GT))
  }
})

output$gbifMotion <- renderGvis({
  GBIF_vis()$motion
})

output$gbifVis <- renderGvis({
  GBIF_vis()$pieMotion
})

# Calendar data

calData <- reactive({
  d1 <- data_gbif()
  if(!is.null(d1)){
    d1 <- occs_history(data_gbif())$data
    d1$date <- as.Date(paste(d1$year,d1$month,d1$day,sep="/"),format = '%Y/%m/%d')
    datos <- d1[with(d1,order(date)),]
    datos <- datos[!is.na(datos$year),]
    if(!is.null(input$GBIFYears)){
      yearsALL <- format(datos$date,'%Y')
      indexY <- sapply(input$GBIFYears,
                       function(x) return(which(x==yearsALL)))
      indexY <- unlist(indexY)
      datos <- datos[indexY,]
      datPie <- datos
    }
    datos <- datos %>% group_by(date) %>% summarise(records=n())
    datos <- datos[,c("date","records")]
    datos <- na.omit(datos)

    return(list(data=datos,datPie=datPie))
  }
  else
    return(NULL)

})

# Select a year in the calendar

selectYear <- reactive({

  d1 <- data_gbif()

  if(!is.null(d1)){
    d1$date <- as.Date(paste(d1$year,d1$month,d1$day,sep="/"),format = '%Y/%m/%d')
    datos <- d1[with(d1,order(date)),]
    datos <- datos[!is.na(datos$year),]
    years <- unique(datos$year)
    return(years)
  }
  else
    return(NULL)

})

# Plot Calendar

calendar <- reactive({
  if(!is.null(calData())){
    datos <- calData()$data

    datos$date <- as.Date(datos$date)
    nyears <- length(unique(format(datos$date,'%Y')))

    Cal <- gvisCalendar(datos,
                        datevar="date",
                        numvar="records",
                        options=list(
                          title="Occs records Calendar & % of records by country",
                          height=140*nyears,
                          calendar="{yearLabel: { fontName: 'Times-Roman',
                          fontSize: 32, color: '#1A8763', bold: true},
                          cellSize: 10,
                          cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                          focusedCellColor: {stroke:'red'}}"))
    datPie <- calData()$datPie
    gD <- datPie[,c("country","year")]
    gD <- gD %>% group_by(country) %>% summarise(count1 = n())
    gD <- gD[,c("country","count1")]
    Pie <- gvisPieChart(gD,options=list(legend="All time %"))
    CalPie <- gvisMerge(Cal,Pie, horizontal=TRUE)
    return(CalPie)
    }

  })

output$calendarG <- renderGvis({
  if(!is.null(calendar())){
    input$showGBIF
    isolate({
      if(input$showGBIF){
        return(calendar())
      }
    })

  }
})


# Create a GBIF animation


# File to save the GIF
temGBIF <- reactive({
  MHmakeRandomString <- function(n=1, lenght=12)
  {
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
      randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                      lenght, replace=TRUE),
                               collapse="")
    }
    return(randomString)
  }

  return(paste0(input$genus,"_",input$species,"_animation",".gif"))
})
# Generte animated Map
animatedGBIF <- reactive({
  if(!is.null(data_gbif())) {
    d1 <- data_gbif()
    d1$date <- as.Date(paste(d1$year,d1$month,d1$day,sep="/"),format = '%Y/%m/%d')
    datos <- d1[with(d1,order(date)),]
    datos <- datos[!is.na(datos$year),]
    nD <- unique(datos$year)
    n <- length(nD)
    namesSp <- levels(d1$name)
    if(length(namesSp) == 1) sps <- bquote(bold("GBIF"~"occs"~"for")~bolditalic(.(namesSp)))
    else sps <- paste0("GBIF data")

    saveGIF({
      for(i in 1:n) {

        maps::map("world", fill=TRUE, col="white",
                  bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
        title(sps,cex=40,cex.main = 2)
        ## Poniendo ejes al mapa
        axis(1,las=1)
        axis(2,las=1)
        toP <- which(datos$year<=nD[i])
        legend("topleft", legend = paste0(nD[i]),cex=2)

        colores <- as.numeric(datos$year[toP])
        leyenda <- unique(as.character(datos$year))
        colL <-   unique(as.numeric(datos$year))


        points(datos$longitude[toP],datos$latitude[toP], col=colores,cex=1.5,pch=20)
        legend("topright",legend = leyenda,col=colL,pch=20,ncol = 2,cex=0.95)
        #colores <- as.numeric(datos$name[1:i])


      }
    }, interval = 0.4, movie.name = temGBIF(), ani.width = 1200, ani.height = 800)

  }
  else
    return(NULL)

})

# Animated GBIF data Map
output$ani_GBIF = downloadHandler(
  filename = function() paste0(input$genus,'_',input$species,'_animatedMapNTB','.gif'),
  content  = function(file) {
    if(!is.null(animatedGBIF())){
      anifile <- paste0(tempdir(),"/",temGBIF())
      #file.remove(temGBIF())
      file.copy(from = anifile,to = file)
    }
  })






# Create an observer for updating selectInput of GBIF data

observe({
  # Data frame with gbif records
  df_gbif_search <- data_gbif_search()
  if (is.data.frame(df_gbif_search)) {

    # Regular expression to look for longitude and latitude

    lon_gbif <- grep(pattern = "longitude",
                     x = names(df_gbif_search),value=TRUE)[1]
    lat_gbif <- grep(pattern = "latitude",
                     x = names(df_gbif_search),value=TRUE)[1]

    ifelse(test = !is.null(lon_gbif),
           longitud_gbif <- lon_gbif ,
           longitud_gbif <- names(df_gbif_search)[1])
    ifelse(test = !is.null(lat_gbif),
           latitud_gbif <- lat_gbif ,
           latitud_gbif  <- names(df_gbif_search)[1])

    # Update select input for longitude
    updateSelectInput(session, 'xLongitudeGBIF',
                      choices = names(df_gbif_search),
                      selected = longitud_gbif)
    # Update select input for latitude
    updateSelectInput(session, 'yLatitudeGBIF',
                      choices = names(df_gbif_search),
                      selected = latitud_gbif)
    # Update select input for GBIF grouping variable
    updateSelectInput(session, 'groupGBIF',
                      choices = names(df_gbif_search),
                      selected = names(df_gbif_search))


  }
})

# Observer for updating the levels of the grouping variable

observe({
  df_gbif_search <- data_gbif_search()
  if (is.data.frame(df_gbif_search)) {
    if(input$groupGBIF != "Search for a species" && input$groupGBIF %in% df_gbif_search){
      niveles <- levels(as.factor(df_gbif_search[,input$groupGBIF]))
      updateSelectInput(session, 'groupLevelsGBIF', choices =niveles)
    }
  }

})

observe({
  if(!is.null(selectYear()))
    updateSelectInput(session,"GBIFYears",choices = selectYear())
})


