observe({
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()))
      var_suggest <- summs_corr_var()$descriptors
    else
      var_suggest <- names(data_extraction())
    updateSelectInput(session,"biosBioclim",
                      choices = names(data_extraction()),selected = var_suggest)
  }
})


# ---------------------------------------------------------------------
# Fit Bioclim model
# ---------------------------------------------------------------------


bioclim_model_all_all_train <- eventReactive(input$run_bioclim_all_all_train,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract()) && input$selectMBio == "wWorld" && input$trainBio == "wWorld"){
      model_train <- dismo::bioclim(occ_extract()$data[,input$biosBioclim])
      model <- predict(rasterLayers()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})


output$bio_response_all_all_train <- renderPlot({
  if(!is.null(bioclim_model_all_all_train()) && input$selectMBio == "wWorld" && input$trainBio == "wWorld")
    return(response(bioclim_model_all_all_train()$train))
})


bioclim_model_all_m_train <- eventReactive(input$run_bioclim_all_m_train,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract_from_mask()) && input$selectMBio == "wWorld" && input$trainBio == "mLayers"){
      model_train <- bioclim(occ_extract_from_mask()$data[,input$biosBioclim])
      model <- predict(rasterLayers()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})


output$bio_response_all_m_train <- renderPlot({
  if(!is.null(bioclim_model_all_m_train()) && input$selectMBio == "wWorld" && input$trainBio == "mLayers")
    return(response(bioclim_model_all_m_train()$train))
})


# ---------------------------------------------------------------------
# Fit Bioclim model m raster
# ---------------------------------------------------------------------


bioclim_model_m_all_train <- eventReactive(input$run_bioclim_m_all_train,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract()) && input$selectMBio == "mLayers" && input$trainBio == "wWorld"){
      model_train <- bioclim(occ_extract()$data[,input$biosBioclim])
      model <- predict(define_M_raster()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})



bioclim_model_m_m_train <- eventReactive(input$run_bioclim_m_m_train,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract_from_mask()) && input$selectMBio == "mLayers" && input$trainBio == "mLayers"){
      model_train <- bioclim(occ_extract_from_mask()$data[,input$biosBioclim])
      model <- predict(define_M_raster()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})



output$bio_response_m_all_train <- renderPlot({
  if(!is.null(bioclim_model_m_all_train()) && input$selectMBio == "mLayers" && input$trainBio == "wWorld")
    return(response(bioclim_model_m_all_train()$train))
})

output$bio_response_m_m_train <- renderPlot({
  if(!is.null(bioclim_model_m_m_train()) && input$selectMBio == "mLayers" && input$trainBio == "mLayers")
    return(response(bioclim_model_m_m_train()$train))
})


output$downBiclimRas <- downloadHandler(
  filename <- function() return(paste0("BioclimModelNTB_trainArea_",
                                       as.character(input$trainBio),"projected_area_",
                                       as.character(input$selectMBio),".asc")),
  content <- function(file){
    if(!is.null(bioclim_model_all_all_train()) && input$selectMBio == "wWorld" && input$trainBio == "wWorld"){
      return(writeRaster(bioclim_model_all_all_train()$prediction,file))
    }
    if(!is.null(bioclim_model_m_all_train()) && input$selectMBio == "mLayers" && input$trainBio == "wWorld"){
      return(writeRaster(bioclim_model_m_all_train()$prediction,file))

    }
    if(!is.null(bioclim_model_all_m_train()) && input$selectMBio == "wWorld" && input$trainBio == "mLayers"){
      return(writeRaster(bioclim_model_all_m_train()$prediction,file))
    }
    if(!is.null(bioclim_model_m_m_train()) && input$selectMBio == "mLayers" && input$trainBio == "mLayers"){
      return(writeRaster(bioclim_model_m_m_train()$prediction,file))
    }
  }
)
