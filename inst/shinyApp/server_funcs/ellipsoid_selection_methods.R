
observe({
  niche_proj  <- NULL
  if(!is.null(occ_extract())){
    niche_proj <- c(niche_proj,"All raster extent"="wWorld")
  }

  if(!is.null(occ_extract_from_mask())){
    niche_proj <- c(niche_proj,"Your polygon of M"="mLayers")
  }

  if(!is.null(input$biosEllipS)){
    nvarsfit <- 2:length(input$biosEllipS)
    updateSelectInput(session, "nvars",choices = nvarsfit,
                      selected = nvarsfit[1])
  }
  updateSelectInput(session, "selectShapeS",choices = niche_proj)


})


observe({
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()$descriptors))
      var_suggest1 <- summs_corr_var()$descriptors
    else
      var_suggest1 <- names(data_extraction())
    updateSelectInput(session,"biosEllipS",
                      choices = names(data_extraction()),
                      selected = var_suggest1)
    #updateSelectInput(session,)

  }
})

output$esrand <- renderUI({
  if(!is.null(data_partition()$type)){
    checkboxInput("espartition","Use train data generated in ntbox to fit the model",
                  value = TRUE)
  }

})

output$filesel <- renderUI({
  if(isFALSE(input$espartition)){
    tagList(
      h4("Upload your train and test datasets"),
      p("The data must be in the following format:"),
      br(),
      HTML('<table style="width:100%">
                                      <tr>
                                      <th>Species</th>
                                      <th>long</th>
                                      <th>lat</th>
                                      </tr>
                                      <tr>
                                      <td>sp_name</td>
                                      <td>-65.38</td>
                                      <td>-10.38</td>
                                      </tr>
                                      </table>'),
      br(),

      fileInput("estrain", "Choose CSV File of your training data",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      fileInput("estest", "Choose CSV File of your testing data",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))

    )
  }
})



data_etrain <- reactive({

  if (is.null(input$estrain))
    return(NULL)

  dat <- rio::import(input$estrain$datapath)
  if(class(dat) == "data.frame")
    return(dat[,1:3])
  else
    return(NULL)

})


data_etest <- reactive({

  if (is.null(input$estest))
    return(NULL)

  dat <- rio::import(input$estest$datapath)
  if(class(dat) == "data.frame")
    return(dat[,1:3])
  else
    return(NULL)

})


# Environmental train

env_train_ntbox <- reactive({

  if(!is.null(data_partition()$type) && input$espartition){
    df1 <- data_partition()
    df1 <- df1[which(df1$type=="train"),]
    return(df1)
  }

})

# Environmental train

env_test_ntbox <- reactive({

  if(!is.null(data_partition()$type) && input$espartition){
    df1 <- data_partition()
    df1 <- df1[which(df1$type=="test"),]
    return(df1)
  }

})


env_train_user <-reactive({
  if(isFALSE(input$espartition) && !is.null(data_etrain())){
    data_env <- data.frame(raster::extract(rasterLayers(),
                                           data_etrain()[,2:3]))
    data_env <- na.omit(data_env)
    return(data_env)
  }
})


env_test_user <-reactive({
  if(isFALSE(input$espartition) && !is.null(data_etrain())){
    data_env <- data.frame(raster::extract(rasterLayers(),
                                           data_etest()[,2:3]))
    data_env <- na.omit(data_env)
    return(data_env)
  }
})



env_bgM <- eventReactive(input$run_bgM,{
  if(!is.null(define_M_raster()) && input$selectShapeS=="mLayers"){

    if(input$rseed)
      rsM <- 12345
    else
      rsM <- NULL
    env_bg <- ntbox::sample_envbg(define_M_raster(),
                                  nbg = as.numeric(input$bg_number),
                                  rseed = rsM)
    return(env_bg)
  }

  })


env_bgW <- eventReactive(input$run_bg,{
  if(!is.null(rasterLayers()) && input$selectShapeS=="wWorld"){
    if(input$rseed)
      rsW <- 12345
    else
      rsW <- NULL
    env_bg <- ntbox::sample_envbg(rasterLayers(),
                                  nbg = as.numeric(input$bg_number),
                                  rseed =rsW)
    return(env_bg)
  }
})


ell_cal_sel_m <- eventReactive(input$run_selectionM,{
  if(input$run_bgM && !is.null(env_bgM())){
    env_bg <- env_bgM()
    # Train data
    if(isFALSE(input$espartition) && !is.null(env_train_user())){
      env_train <- env_train_user()
    }
    if(input$espartition && !is.null(env_train_ntbox())){
      env_train <- env_train_ntbox()
    }
    if(isFALSE(input$espartition) && !is.null(env_test_user())){
      env_test <- env_test_user()
    }
    if(input$espartition && !is.null(env_test_ntbox())){
      env_test <- env_test_ntbox()
    }
    env_vars <- as.character(input$biosEllipS)
    nvarstest <- as.numeric(as.character(input$nvars))
    level	<- as.numeric(input$prop_pointsS)
    mve <- input$se_mve
    omr_criteria <- as.numeric(input$omr)
    parallel	<- input$eparallel
    comp_each <- input$ecomp_each
    proc <- input$eproc
    proc_iter <- as.numeric(input$prociter)
    rseed <- input$rseed

    e_selct <- ntbox::ellipsoid_selection(env_train = env_train,
                                          env_test = env_test,
                                          env_vars = env_vars,
                                          nvarstest = nvarstest,
                                          level = level,
                                          mve = mve,
                                          env_bg = env_bg,
                                          omr_criteria=omr_criteria,
                                          parallel = parallel,
                                          comp_each = comp_each,
                                          proc = proc,
                                          proc_iter = proc_iter,
                                          rseed = rseed)
    return(e_selct)
  }

})

ell_cal_sel_w <- eventReactive(input$run_selectionW,{
  if(input$run_bg && !is.null(env_bgW())){
    env_bg <- env_bgW()

    # Train data
    if(isFALSE(input$espartition) && !is.null(env_train_user())){
      env_train <- env_train_user()

    }
    if(input$espartition && !is.null(env_train_ntbox())){
      env_train <- env_train_ntbox()

    }
    if(isFALSE(input$espartition) && !is.null(env_test_user())){
      env_test <- env_test_user()
    }
    if(input$espartition && !is.null(env_test_ntbox())){
      env_test <- env_test_ntbox()
    }
    env_vars <- as.character(input$biosEllipS)
    nvarstest <- as.numeric(as.character(input$nvars))
    level	<- as.numeric(input$prop_pointsS)
    mve <- input$se_mve
    omr_criteria <- as.numeric(input$omr)
    parallel	<- input$eparallel
    comp_each <- input$ecomp_each
    proc <- input$eproc
    proc_iter <- as.numeric(input$prociter)
    rseed <- input$rseed


    e_selct <- ntbox::ellipsoid_selection(env_train = env_train,
                                          env_test = env_test,
                                          env_vars = env_vars,
                                          nvarstest = nvarstest,
                                          level = level,
                                          mve = mve,
                                          env_bg = env_bg,
                                          omr_criteria=omr_criteria,
                                          parallel = parallel,
                                          comp_each = comp_each,
                                          proc = proc,
                                          proc_iter = proc_iter,
                                          rseed = rseed)
    return(e_selct)
  }

})

#observe({
#  print(input$run_selectionW)
#  print(ell_cal_sel_w ())
#})

output$env_bgT <- DT::renderDataTable({

  if(!is.null(env_bgW()) && input$selectShapeS=="wWorld")
    return(env_bgW())
  if(!is.null(env_bgM()) && input$selectShapeS=="mLayers")
    return(env_bgM())
})

output$userV <-DT::renderDataTable({

  if(!is.null(data_etrain()))
    return(data_etrain())
})

output$eselecTable <-  DT::renderDataTable({
  if(input$run_selectionW && !is.null(ell_cal_sel_w()) &&
     input$selectShapeS =="wWorld"){
    return(ell_cal_sel_w())
  }
  if(input$run_selectionM && !is.null(ell_cal_sel_m()) &&
     input$selectShapeS =="mLayers"){
    return(ell_cal_sel_m())
  }
  #return(data.frame(a=1,b2=2))

})

tipo_mod <- reactive({
  if(input$selectShapeS=="wWorld")
    return("_all_area.csv")
  if(input$selectShapeS=="mLayers")
    return("_m_area.csv")
})
output$downEselection <- downloadHandler(
  filename <- function() {paste0("ellipsoid_selection",tipo_mod())},
  content <- function(file){
    if(input$run_selectionW && !is.null(ell_cal_sel_w()) &&
       input$selectShapeS =="wWorld")
      write.csv(ell_cal_sel_w(),file = file,row.names = F)
    else if(input$run_selectionM && !is.null(ell_cal_sel_m()) &&
            input$selectShapeS =="mLayers")
      write.csv(ell_cal_sel_m(),file = file,row.names = F)
  }
)

