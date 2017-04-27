observe({
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()))
      var_suggest <- summs_corr_var()$descriptors
    else
      var_suggest <- NULL
    updateSelectInput(session,"biosEllip",
                      choices = names(data_extraction()),selected = var_suggest)
  }
  niche_proj  <- NULL
  if(!is.null(occ_extract())){
    niche_proj <- c(niche_proj,"All raster extent"="wWorld")
  }

  if(!is.null(occ_extract_from_mask())){
    niche_proj <- c(niche_proj,"Your polygon of M"="mLayers")
  }
  updateSelectInput(session, "selectM",choices = niche_proj)
  updateSelectInput(session, "selectShape",choices = niche_proj)
})



# ---------------------------------------------------------------------
# Fit the ellispoid model All raster area
# ---------------------------------------------------------------------

# 1. Compute the minimum volume ellipsoid all Raster area


mve_obj_all <- reactive({
  if(!is.null(occ_extract()) && !is.null(input$biosEllip) && input$selectShape == "wWorld"){
    prop_points <- as.numeric(input$prop_points)
    niche_data <- na.omit(occ_extract())
    cov_centroid <- cov_center(niche_data,
                               level=prop_points,
                               vars=input$biosEllip)

    return(cov_centroid)

  }
  else
    return()
})

# 1' Compute the minimum volume ellipsoid M Raster area

mve_obj_m <- reactive({
  if(!is.null(occ_extract_from_mask()) && !is.null(input$biosEllip) && input$selectShape == "mLayers"){
    prop_points <- as.numeric(input$prop_points)
    niche_data <- na.omit(occ_extract_from_mask()$data)
    cov_centroid <- cov_center(niche_data,
                               level=prop_points,
                               vars=input$biosEllip)

    return(cov_centroid)

  }
  else
    return()
})


# Choose the minimum volume ellipsoid (all or M raster area)

mve_obj <- reactive({
  if(!is.null(mve_obj_all()) && input$selectShape == "wWorld")
    return(mve_obj_all())
  if(!is.null(mve_obj_m()) && input$selectShape == "mLayers")
    return(mve_obj_m())
  else
    return()
})

# 2. Fit and project the model All raster area and trained in All raster area


ellip_model_all_rast_all_train <- eventReactive(input$selectBios_all_all_train,{
  cov_centroid <- mve_obj_all()
  if(!is.null(rasterLayers()) && input$selectM == 'wWorld' && !is.null(cov_centroid) && input$selectShape == "wWorld"){

    model <- ellipsoidfit(rasterLayers()[[input$biosEllip]],
                          cov_centroid$centroid,
                          cov_centroid$covariance,level = 0.95,
                          threshold = 0.001,plot = FALSE)

    return(model)
  }
  else
    return()
})


ellip_model_all_rast_m_train <- eventReactive(input$selectBios_all_m_train,{
  cov_centroid <- mve_obj_m()
  if(!is.null(rasterLayers()) && input$selectM == 'wWorld' && !is.null(cov_centroid) && input$selectShape == "mLayers"){

    model <- ellipsoidfit(rasterLayers()[[input$biosEllip]],
                          cov_centroid$centroid,
                          cov_centroid$covariance,level = 0.95,
                          threshold = 0.001,plot = FALSE)


    return(model)
  }
  else
    return()
})


# Plot the model in Enverionmental Space (All raster area)

plot_ellipsoid_all_all_train <- eventReactive(input$selectBios_all_all_train,{
  if(!is.null(ellip_model_all_rast_all_train()) && input$selectM == 'wWorld' && input$selectShape == 'wWorld'){
    suits <- ellip_model_all_rast_all_train()$suits[,"suitability"]
    data <- ellip_model_all_rast_all_train()$suits[,input$biosEllip]
    covar <- mve_obj_all()$covariance
    centroid <- mve_obj_all()$centroid
    ellipsoid_plot_3d(suits = suits,
                      data = data,covar = covar,
                      centroid = centroid,
                      xlab1 = input$biosEllip[1],
                      ylab1 = input$biosEllip[2])
  }
  else
    return()

})

# Plot the model in Enverionmental Space (All raster area)

plot_ellipsoid_all_m_train <-eventReactive(input$selectBios_all_m_train,{
  if(!is.null(ellip_model_all_rast_m_train()) && input$selectM == 'wWorld' && input$selectShape == 'mLayers'){
    suits <- ellip_model_all_rast_m_train()$suits[,"suitability"]
    data <- ellip_model_all_rast_m_train()$suits[,input$biosEllip]
    covar <- mve_obj_m()$covariance
    centroid <- mve_obj_m()$centroid
    ellipsoid_plot_3d(suits = suits,
                      data = data,covar = covar,
                      centroid = centroid,
                      xlab1 = input$biosEllip[1],
                      ylab1 = input$biosEllip[2])
  }
  else
    return()

})


# Plot the model in Enverionmental Space (All raster area)

output$Ellip3D_all_all_train <- renderRglwidget({
  plot_ellipsoid_all_all_train()
  rglwidget()
})

output$Ellip3D_all_m_train <- renderRglwidget({
  plot_ellipsoid_all_m_train()
  rglwidget()
})



output$Ellip2D_all_all_train <- renderPlot({
  plot_ellipsoid_all()
})


output$Ellip2D_all_m_train <- renderPlot({
  plot_ellipsoid_all_m_train()
})


# Normal Response curves

response_ell_all_all_train <- eventReactive(input$selectBios_all_all_train,{
  if(!is.null(ellip_model_all_rast_all_train())){
    if(input$selectM=="wWorld" && input$selectShape == "wWorld"){
      multi.hist(occ_extract()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})

output$reponse_curves_all_all_train <- renderPlot({
  if(!is.null(response_ell_all_all_train()))
    response_ell_all_all_train()
})

response_ell_all_m_train <- eventReactive(input$selectBios_all_m_train,{
  if(!is.null(ellip_model_all_rast_m_train())){
    if(input$selectM=="wWorld" && input$selectShape == "mLayers"){
      multi.hist(occ_extract_from_mask()$data[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})

output$reponse_curves_all_m_train <- renderPlot({
  if(!is.null(response_ell_all_m_train()))
    response_ell_all_m_train()
})



# ---------------------------------------------------------------------
# Fit the ellispoid model M area All data trian
# ---------------------------------------------------------------------



ellip_model_m_rast_all_train <- eventReactive(input$selectBios_m_all_train,{
  cov_centroid <- mve_obj_all()
  if(!is.null(cov_centroid) && !is.null(define_M_raster()) && input$selectM == 'mLayers' && input$selectShape == 'wWorld'){

    model <- ellipsoidfit(define_M_raster()[[input$biosEllip]],
                          cov_centroid$centroid,
                          cov_centroid$covariance,level = 0.95,
                          threshold = 0.001,plot = FALSE)
    return(model)
  }
  else
    return()

})



# Plot the model in Enverionmental Space (M raster area)

plot_ellipsoid_m_all_train <- eventReactive(input$selectBios_m_all_train,{
  if(!is.null(ellip_model_m_rast_all_train()) && input$selectM == 'mLayers' && input$selectShape == 'wWorld'){
    suits <- ellip_model_m_rast_all_train()$suits[,"suitability"]
    data <- ellip_model_m_rast_all_train()$suits[,input$biosEllip]
    covar <- mve_obj_all()$covariance
    centroid <- mve_obj_all()$centroid
    ellipsoid_plot_3d(suits = suits,
                      data = data,covar = covar,
                      centroid = centroid,
                      xlab1 = input$biosEllip[1],
                      ylab1 = input$biosEllip[2])
  }
  else
    return()

})


# Plot the model in Enverionmental Space (M raster area)

output$Ellip3D_m_all_train <- renderRglwidget({

  plot_ellipsoid_m_all_train()
  rglwidget()
})



# ---------------------------------------------------------------------
# Fit the ellispoid model M area M data trian
# ---------------------------------------------------------------------



ellip_model_m_rast_m_train <- eventReactive(input$selectBios_m_m_train,{
  cov_centroid <- mve_obj_m()
  if(!is.null(cov_centroid) && !is.null(define_M_raster()) && input$selectM == 'mLayers' && input$selectShape == 'mLayers'){

    model <- ellipsoidfit(define_M_raster()[[input$biosEllip]],
                          cov_centroid$centroid,
                          cov_centroid$covariance,level = 0.95,
                          threshold = 0.001,plot = FALSE)
    return(model)
  }
  else
    return()

})



# Plot the model in Enverionmental Space (M raster area)

plot_ellipsoid_m_m_train <- eventReactive(input$selectBios_m_m_train,{
  if(!is.null(ellip_model_m_rast_m_train()) && input$selectM == 'mLayers' && input$selectShape == 'mLayers'){
    suits <- ellip_model_m_rast_m_train()$suits[,"suitability"]
    data <- ellip_model_m_rast_m_train()$suits[,input$biosEllip]
    covar <- mve_obj_m()$covariance
    centroid <- mve_obj_m()$centroid
    ellipsoid_plot_3d(suits = suits,
                      data = data,covar = covar,
                      centroid = centroid,
                      xlab1 = input$biosEllip[1],
                      ylab1 = input$biosEllip[2])
  }
  else
    return()

})


# Plot the model in Enverionmental Space (M raster area)

output$Ellip3D_m_m_train <- renderRglwidget({
  plot_ellipsoid_m_m_train()

  rglwidget()
})




output$Ellip2D_m_m_train <- renderPlot({

  plot_ellipsoid_m_m_train()

})



response_ell_m_all_train <- eventReactive(input$selectBios_m_all_train,{
  if(!is.null(ellip_model_all_rast_all_train())){
    if(input$selectM=="mLayers" && input$selectShape == "wWorld"){
      multi.hist(occ_extract()[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})


response_ell_m_m_train <- eventReactive(input$selectBios_m_m_train,{
  if(!is.null(ellip_model_all_rast_all_train())){
    if(input$selectM=="mLayers" && input$selectShape == "mLayers"){
      multi.hist(occ_extract_from_mask()$data[,input$biosEllip],
                 dcol= c("blue","red"),dlty=c("dotted", "solid"))
    }
    else
      return()
  }

})

output$reponse_curves_m_all_train <- renderPlot({
  if(!is.null(response_ell_m_all_train()))
    response_ell_m_all_train()
})


output$reponse_curves_m_m_train <- renderPlot({
  if(!is.null(response_ell_m_m_train()))
    response_ell_m_m_train()
})

# Download Ellipsoid Raster

output$downEllipRas <- downloadHandler(
  filename <- function() {paste0("EllipsoidModelNTB",
                                 input$selectM,"_var_matrix_",
                                 input$selectShape,".asc")},
  content <- function(file){
    if(!is.null(ellip_model_all_rast_all_train()) && input$selectM == "wWorld" && input$selectShape== "wWorld"){
      writeRaster(ellip_model_all_rast_all_train()$suitRaster,file)
    }

    else if(!is.null(ellip_model_all_rast_m_train()) && input$selectM == "wWorld" && input$selectShape== "mLayers"){
      writeRaster(ellip_model_all_rast_m_train()$suitRaster,file)
    }
    else if(!is.null(ellip_model_m_rast_all_train()) && input$selectM == "mLayers" && input$selectShape== "wWorld"){
      writeRaster(ellip_model_m_rast_all_train()$suitRaster,file)
    }
    else if(!is.null(ellip_model_m_rast_m_train()) && input$selectM == "mLayers" && input$selectShape== "mLayers"){
      writeRaster(ellip_model_m_rast_m_train()$suitRaster,file)
    }
  }
)



# Download Ellipsoid distances

output$downEllipDistance <- downloadHandler(
  filename <- function() {paste0("EllipsoidDistancesNTB",input$selectM,"_var_matrix_",
                                 input$selectShape,".csv")},
  content <- function(file){
    if(!is.null(ellip_model_all_rast_all_train()) && input$selectM == "wWorld" && input$selectShape =="wWorld"){
      ndistTable <- data.frame(ellip_model_all_rast_all_train()$suits,
                               ellip_model_all_rast_all_train()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }
    else if(!is.null(ellip_model_all_rast_m_train()) && input$selectM == "wWorld" && input$selectShape =="mLayers"){
      ndistTable <- data.frame(ellip_model_all_rast_m_train()$suits,
                               ellip_model_all_rast_m_train()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }
    else if(!is.null(ellip_model_m_rast_all_train()) && input$selectM == "mLayers" && input$selectShape =="wWorld"){
      ndistTable <- data.frame(ellip_model_m_rast_all_train()$suits,
                               ellip_model_m_rast_all_train()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }
    else if(!is.null(ellip_model_m_rast_m_train()) && input$selectM == "mLayers" && input$selectShape =="mLayers"){
      ndistTable <- data.frame(ellip_model_m_rast_m_train()$suits,
                               ellip_model_m_rast_m_train()$ncentedist)
      write.csv(ndistTable,file,row.names = FALSE)
    }

  }
)


# Download meta data

output$downShapMat <- downloadHandler(
  filename <- function() {paste0("EllipsoidMetaData_",
                                 input$selectM,"_","_var_matrix_",
                                 input$selectShape,".txt")},
  content <- function(file){
    if(input$selectM=="wWorld")
      capture.output(mve_obj_all(),file = file)
    else if(input$selectM=="mLayers")
      capture.output(mve_obj_m(),file = file)
  }
)



output$ellip_models_plots <- renderUI({
  if(length(input$biosEllip) == 3 && input$selectM == 'wWorld' && input$selectShape=='wWorld'){
    return(rglwidgetOutput("Ellip3D_all_all_train",
                           width =  "650px",
                           height  = "650px"))
  }
  if(length(input$biosEllip) == 3 && input$selectM == 'mLayers' && input$selectShape=='wWorld'){
    return(rglwidgetOutput("Ellip3D_m_all_train",
                           width =  "650px",
                           height  = "650px"))
  }
  if(length(input$biosEllip) == 3 && input$selectM == 'wWorld' && input$selectShape=='mLayers'){
    return(rglwidgetOutput("Ellip3D_all_m_train",
                           width =  "650px",
                           height  = "650px"))
  }
  if(length(input$biosEllip) == 3 && input$selectM == 'mLayers' && input$selectShape=='mLayers'){
    return(rglwidgetOutput("Ellip3D_m_m_train",
                           width =  "650px",
                           height  = "650px"))
  }
  if(length(input$biosEllip) > 3 && input$selectM == 'wWorld' && input$selectShape=='wWorld'){
    return(plotOutput("reponse_curves_all_all_train"))
  }
  if(length(input$biosEllip) > 3 && input$selectM == 'wWorld' && input$selectShape=='mLayers'){
    return(plotOutput("reponse_curves_all_m_train"))
  }

  if(length(input$biosEllip) == 2 && input$selectM == 'wWorld' && input$selectShape=='wWorld'){
    return(plotOutput("Ellip2D_all_all_train"))
  }
  if(length(input$biosEllip) == 2 && input$selectM == 'wWorld' && input$selectShape=='mLayers'){
    return(plotOutput("Ellip2D_all_m_train"))
  }
  if(length(input$biosEllip) == 2 && input$selectM == 'mLayers' && input$selectShape=='wWorld'){
    return(plotOutput("Ellip2D_m_all_train"))
  }
  if(length(input$biosEllip) == 2 && input$selectM == 'mLayers' && input$selectShape=='mLayers'){
    return(plotOutput("Ellip2D_m_m_train"))
  }
  if(length(input$biosEllip) > 3 && input$selectM == 'mLayers' && input$selectShape=='wWorld'){
    return(plotOutput("reponse_curves_m_all_train"))
  }
  if(length(input$biosEllip) > 3 && input$selectM == 'mLayers' && input$selectShape=='mLayers'){
    return(plotOutput("reponse_curves_m_m_train"))
  }
})

