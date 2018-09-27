# Read user uploaded data

data_user <- reactive({

  if (is.null(input$fileUser))
    return(NULL)
  else if (identical(input$format, 'CSV'))
    dat <- read.csv(input$fileUser$datapath,fill = T)
  #else if (identical(input$format, 'XLSX'))
  #  dat <- read.xls(input$fileUser$datapath, input$sheet)
  else
    dat <- read.delim(input$fileUser$datapath,fill = TRUE)

  if(class(dat) == "data.frame")
    return(dat)
  else
    return(NULL)

})

# Clean user data

data_user_clean <- reactive({
  data <- data_user()
  if(!is.null(data)){
    input$clean_dup_user
    isolate({
      if(input$clean_dup_user){
        longitude <- input$xLongitudeUser
        latitude <-  input$yLatitudeUser
        threshold <- as.numeric(input$threshold_user)
        if(is.numeric(data[,longitude]) && is.numeric(data[,latitude])){
          data_clean <- clean_dup(data,longitude = longitude,
                                  latitude = latitude,
                                  threshold= threshold)
        }
      }
      else
        return(data)
    })

    # Clean duplicates by grouping variable
    input$clean_dup_user_group
    isolate({
      if(input$clean_dup_user_group){
        if(!is.null(input$groupLevelsUser)){
          selection <- unlist(lapply(input$groupLevelsUser,
                                     function(x)
                                       which(x == data[,input$groupUser])))

          longitude <- input$xLongitudeUser
          latitude <-  input$yLatitudeUser
          threshold <- as.numeric(input$threshold_user)
          if(is.numeric(data[,longitude]) && is.numeric(data[,latitude])){
            data_clean <- clean_dup(data[selection,],
                                    longitude,longitude,
                                    threshold=threshold)
          }
        }
      }
    })

    data_clean$ID_ntb <- 1:dim(data_clean)[1]
    return(data_clean)
    }
  else
    return(NULL)
})



# Display gbif data

output$user_table <- renderDataTable({
  df1 <- data_user_clean()
  if(!is.null(df1))
    return(df1)
  else{
    warn <- "Upload your data..."
    nulo <- " "
    data_null <- c(warn,nulo)
    data_null <- data.frame(Data=data_null)
    return(data_null)
  }
},
options = list(aLengthMenu = c(5, 10, 25,
                               50, 100, 500),
               iDisplayLength = 10))

# Show the dimension of species data (number of rows and columns)

output$nRcordsUser <- renderPrint({

  if(!is.null(data_user_clean())){
    dimen <- dim(data_user_clean())
    cat(dimen[1], 'rows and ',dimen[2],' columns')
  }
  else
    cat("Upload your data...")
})


# Download GBIF data

output$dataUClean <- downloadHandler(
  filename = 'data_User.csv',
  content = function(file) {
    if(!is.null(data_user())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(data_user_clean(),file=file,row.names=FALSE)
    }
  }
)


# Oberver to update input$xLongitudeUser and input$yLatitudeUser

observe({

  # Data frame with gbif records
  data <- data_user()
  if (!is.null(data)) {

    # Regular expression to look for longitude and latitude

    lon_user <- grep(pattern = "[Ll][Oo][Nn]",
                     x = names(data),value=TRUE)[1]
    lat_user <- grep(pattern = "[Ll][Aa][Tt]",
                     x = names(data),value=TRUE)[1]

    ifelse(test = !is.na(lon_user),
           longitud_user <- lon_user ,
           longitud_user <- names(data)[1])
    ifelse(test = !is.na(lat_user),
           latitud_user <- lat_user ,
           latitud_user  <- names(data)[1])

    # Update select input for longitude
    updateSelectInput(session, 'xLongitudeUser',
                      choices = names(data),
                      selected = longitud_user)
    # Update select input for latitude
    updateSelectInput(session, 'yLatitudeUser',
                      choices = names(data),
                      selected = latitud_user)
    # Update select input for User grouping variable
    updateSelectInput(session, 'groupUser',
                      choices = names(data))

  }
  #if(!is.null(selectYear())){
  #  updateSelectInput(session,"GBIFYears",choices =selectYear())
  #}

})


# Observer for updating the levels of the grouping variable

observe({
  df_user <- data_user()
  if (!is.null(df_user)) {
    if(input$groupUser != "Upload your data" && input$groupUser %in% names(df_user)){
      niveles <- levels(as.factor(df_user[,input$groupUser]))
      updateSelectInput(session, 'groupLevelsUser', choices =niveles)
    }
  }

})


