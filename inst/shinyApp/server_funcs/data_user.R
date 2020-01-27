# Read user uploaded data

data_user <- reactive({

  if (is.null(input$fileUser))
    return(NULL)
  else if (identical(input$format, 'CSV'))
    dat <- rio::import(input$fileUser$datapath)
  #else if (identical(input$format, 'XLSX'))
  #  dat <- read.xls(input$fileUser$datapath, input$sheet)
  else
    dat <- rio::import(input$fileUser$datapath)

  if(class(dat) == "data.frame")
    return(dat)
  else
    return(NULL)

})


# Oberver to update input$xLongitudeUser and input$yLatitudeUser

observe({

  # Data frame with gbif records
  data <- data_user()
  if (!is.null(data)) {

    # Regular expression to look for longitude and latitude

    lon_user <- grep(pattern = "[Ll][Oo][Nn]|[x]",
                     x = names(data),value=TRUE)[1]
    lat_user <- grep(pattern = "[Ll][Aa][Tt]|[y]",
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


# Global data frame for species data

data_user_sp <- shiny::eventReactive(input$clean_dup_user,{
  data <- data_user()
  if(is.data.frame(data)){
    longitude <- input$xLongitudeUser
    latitude <-  input$yLatitudeUser
    threshold <- as.numeric(input$threshold_user)
    data_clean <- ntbox::clean_dup(data,longitude = longitude,
                                   latitude = latitude,
                                   threshold= threshold)
    data_clean <- data.frame(ID_ntb= 1:nrow(data_clean),
                             data_clean)
    return(data_clean)
  }
})

# Global data frame for grupped data

data_user_group <- shiny::eventReactive(input$clean_dup_user_group,{
  data <- data_user()
  if(is.data.frame(data) && input$groupUser != "Upload your data"){
    longitude <- input$xLongitudeUser
    latitude <-  input$yLatitudeUser
    threshold <- as.numeric(input$threshold_user)
    dataL <- data %>% split(.[,input$groupUser])
    data_clean <- dataL[input$groupLevelsUser] %>%
      purrr::map_df(~ntbox::clean_dup(.x,longitude = longitude,
                                      latitude = latitude,
                                      threshold= threshold))
    return(data_clean)
  }
})

values_user <- reactiveValues(counter_sp=0,counter_group=0)

observeEvent(input$clean_dup_user,{
  if(is.data.frame(data_user_sp())){
    values_user$counter_sp <- 1
    values_user$counter_group <- 0
  }
})

observeEvent(input$clean_dup_user_group,{
  if(is.data.frame(data_user_group())){
    values_user$counter_group <-  1
    values_user$counter_sp <-  0
  }
})


data_user_clean <- reactive({
  data <- data_user()

  if(is.data.frame(data)){
    if(values_user$counter_sp){
      data <- data_user_sp()
    }

    if(values_user$counter_group){
      data <- data_user_group()
    }
    return(data)
  }
  else
    return()
})


# Display gbif data

output$user_table <- DT::renderDataTable({
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
})

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



