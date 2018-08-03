# SDM user raster

sdm_raster <- reactive({
  if (is.null(input$fileBin))
    return(NULL)
  else if(identical(input$formatBin,'.asc'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$formatBin,'.tif'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$formatBin,'.bil'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$formatBin,'.nc'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$formatBin,'.sdat'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$formatBin,'.img'))
    path_ras <- input$fileBin$datapath
  return(raster(path_ras))
})


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Presence & Absence data (validation data) for the confussion matrix

dat_Validation <- reactive({

  if (is.null(input$fileVal))
    return(NULL)
  else if (identical(input$formatVal, 'CSV')){
    data <- read.csv(input$fileVal$datapath,header=TRUE)
    return(data)
  }
  return()
})

# Update Range for threshold search (matrix optimaztion method)

observe({
  if(!is.null(dat_Validation()) && !is.null(sdm_raster())){
    min_range <- cellStats(sdm_raster(),min)
    max_range <- cellStats(sdm_raster(),max)
    value_range <- mean(c(min_range,max_range))
    step_sugg <-  max_range/100
    updateSliderInput(session,"thRange",
                      min = min_range,max = value_range)
    updateNumericInput(session, "partStep",
                       value = step_sugg,
                       min = min_range,max = max_range)
    updateSliderInput(session,"thresholdBin",
                      min = min_range,max = value_range)
  }
})


# (Matrix optimization search) Search the best threshold cut for SDM

threshold_search <- reactive({
  sdm_raster <- sdm_raster()
  dat_Val <- dat_Validation()
  if(!is.null(sdm_raster) && input$valdata_type == 'pres_abs_data'){
    if(!is.null(dat_Val)){
      input$searchTh
      isolate({
        if(input$searchTh){
          longitude <- 1
          latitude <- 2
          pres_aus <- 3
          optim_by <- input$optim_by1
          bestTH <- confu_mat_optim(sdm_raster = sdm_raster,
                                    valData = dat_Val,
                                    longitude = longitude,
                                    latitude = latitude,
                                    pres_aus = pres_aus,
                                    optim_by = optim_by,
                                    th_range = as.numeric(input$thRange),
                                    step = as.numeric(input$partStep))
          return(bestTH)
        }
      })
    }
  }
  if(!is.null(dat_val_mtp()) && input$valdata_type != 'pres_abs_data'){
    dat_Val <- data.frame(dat_val_mtp()[,-1])
    values <- raster::extract(sdm_raster, dat_Val[, c(1,  2)])

    if(input$valdata_type=='user_threshold'){

      threshold <- as.numeric(input$thresholdBin)
    }
    if(input$valdata_type=='min_traing_pres' | input$valdata_type=='percentil_bin'){
      threshold <- mtp_threshold()
    }
    reclass <- values >= threshold
    a <- length(which(reclass))
    b <- NA
    c <- length(which(!reclass))
    d <- NA
    conf <- data.frame(a,b,c,d)
    return(conf)
  }
  else
    return(NULL)

})

threshold_search2 <- reactive({
  if(!is.null(threshold_search()) && input$valdata_type=='pres_abs_data'){
    df_threshold <- threshold_search()
    df_threshold <- df_threshold[order(-df_threshold[,input$optim_by1]),]
    return(df_threshold)
  }
  if(!is.null(threshold_search()) && input$valdata_type !='pres_abs_data'){
    return(threshold_search())
  }
  else
    return()
})

# Data table of threshold search

output$threshold_sims <- renderDataTable({
  resTh <- threshold_search2()
  if(!is.null(resTh)){
    return(resTh)
  }
  else
    return(NULL)
},options = list(aLengthMenu = c(5, 10, 25,
                                 50, 100, 500),
                 iDisplayLength = 10))


# Update values of confusion matrix accoding to Confusion matrix optimazation


observe({
  resTh <- threshold_search2()
  if(!is.null(resTh)){
    a <- resTh$a[1]
    b <- resTh$b[1]
    c <- resTh$c[1]
    d <- resTh$d[1]
    updateNumericInput(session,"a",value = a)
    updateNumericInput(session,"b",value = b)
    updateNumericInput(session,"c",value = c)
    updateNumericInput(session,"d",value = d)
  }

})


# Print confusion matrix


confu_matrix <- function(a,b,c,d){

  m <- matrix(c(paste0("a = ", a),paste0("b = ",b),paste0("c = ", c),paste0("d = ", d)),ncol=2)
  colnames(m) <- c("Predicted presence","Prediceted absence")
  m <- as.data.frame(m)
  rownames(m) <- c("Observed presence", "Observed absence")
  m <- t(m)
  return(m)

}


output$con_tableBin <- renderTable({
  a <- input$a
  b <- input$b
  c <- input$c
  d <- input$d
  return(confu_matrix(a=a,b=b,c=c,d=d))
})


# Plot binary map for confusin matrix optimization

binary_cm_method <- reactive({
  resTh <- threshold_search2()

  model <- sdm_raster()

  if(!is.null(resTh) && !is.null(model)){
    min_range <- min(input$thRange)
    max_range <- cellStats(model,max)
    threshold <-  resTh$threshold[1]
    rbin <- model >= threshold
    #reclass_matrix <- matrix(c(min_range,threshold,
    #                           0,threshold,max_range,1),
    #                         ncol=3,byrow=TRUE)
    #rbin <- reclassify(model,rcl = reclass_matrix)
    return(rbin)
  }
  else
    return()

})

# Compute the total area (in km^2) of distribution given a SDModel

distribution_area <- function(binaryMap){
  area1 <- raster::area(binaryMap)
  area_Map <- area1*binaryMap
  area_tot <- cellStats(area_Map,"sum")
  return(area_tot)
}

binary_area_cm_method <- reactive({
  if(!is.null(binary_cm_method())){
    sdm_area <- distribution_area(binary_cm_method())
    return(sdm_area)
  }
})

output$area_cm_method <- renderUI({
  resTh <- threshold_search2()
  if(!is.null(binary_area_cm_method())){
    threshold <-  resTh$threshold[1]
    area <- round(binary_area_cm_method(),4)
    h3(paste("Species Distribution Area",area,"in km^2", "(at",threshold,", threshold)"))
  }
})

meta_data_cm_method <- reactive({
  if(!is.null(binary_area_cm_method())){
    resTh <- threshold_search2()
    threshold <-  resTh$threshold[1]
    area <- binary_area_cm_method()
    model <- binary_cm_method()
    xmin <- model@extent@xmin
    ymin <- model@extent@ymin
    xmax <- model@extent@xmax
    ymax <- model@extent@ymax
    pixel_res <- res(model)[1]
    area_df <- data.frame(SDM_area = area,
                          cutoff_threshold=threshold,
                          xmin,ymin,xmax,ymax,pixel_res)
    return(area_df)
  }
  else
    return()
})

output$downloadBinary_metadata_mc_opt <- downloadHandler(
  filename <- function() paste0("binaryMap_metadata_",
                                round(threshold_search2()[1,1],4),
                                "_mc_opt.csv"),
  content <- function(file){
    if(!is.null(meta_data_cm_method())){
      write.csv(meta_data_cm_method(),file, row.names = FALSE)
    }
  }
)


output$binary_CM_optim <- renderPlot({
  if(!is.null(binary_cm_method()))
    return(plot(binary_cm_method()))

})

output$downloadBinary_mc_opt <- downloadHandler(
  filename <- function() paste0("binaryMap_",
                                round(threshold_search2()[1,1],4),
                                "_mc_opt.asc"),
  content <- function(file){
    if(!is.null(binary_cm_method())){
      writeRaster(binary_cm_method(),file)
    }
  }
)


#------------------------------------------------------------------------------
# Metrics for the confusion Martrix of the binary Map (confusin matrix method)



f_error_com <- function(b,d)    return(b/(b+d))
f_error_om <- function(a,c)  return( c/(a+c))
sensibilidad <- function(a,c)  return(a/(a+c))
especificidad <- function(b,d)  return(d/(b+d))
tas_fals_pos <- function(b,d) return(b/(b+d))
tas_fals_neg <- function(a,c) return(c/(a+c))
posit_pre_pow <- function(a,b) return(a/(a+b))
nega_pre_pow <- function(c,d) return(d/(c+d))
miss_cla_rate <- function(a,b,c,d) return((b+c)/(a+b+c+d))
prevalencia <- function(a,b,c,d) return((b + d)/(a+b+c+d))
correct_class_rate <- function(a,b,c,d) return((a + d)/(a+b+c+d))
tss <- function(a,b,c,d) return(sensibilidad(a,c)+especificidad(b,d)-1)
kappa <- function(a,b,c,d){
  N <- a+b+c+d
  term1 <- ((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  term2 <- (N-(((a+c)*(a+b)+(b+d)*(c+d))/N))
  return(term1/term2)
}



all_result <- function(a,b,c,d){

  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("                           Symbols                                       \n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("a = Correctly predicted presences\n")
  cat("b = Predicted present but actually absent\n")
  cat("c = Predicted absent but actually present\n")
  cat("d = Correctly predicted absences\n\n\n\n")

  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("                     Confusion Matrix                                    \n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

  print(confu_matrix(a,b,c,d))
  cat("\n\n\n\n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("                     Evaluation metrics for SDMs                         \n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat("Kappa: ", kappa(a=a,b=b,c=c,d=d),   "\n")
  cat("TSS: ", tss(a=a,b=b,c=c,d=d),       "\n")
  cat("Prevalence: ", prevalencia(a=a,b=b,c=c,d=d),"\n")
  cat("Correct classification rate: ", correct_class_rate(a=a,b=b,c=c,d=d),"\n")
  cat("Misclassification rate: ", miss_cla_rate(a=a,b=b,c=c,d=d),"\n")
  cat("Negative predictive power: ", nega_pre_pow(c = c, d=d),   "\n")
  cat("Positive predictive power: ", posit_pre_pow(a = a,b = b),   "\n")
  cat("False negative rate: ", tas_fals_neg(a = a,c = c),   "\n")
  cat("False positive rate: ",tas_fals_pos(b = b, d = d),   "\n")
  cat("Specificity: ", especificidad(b = b,d = d),   "\n")
  cat("Sensitivity: ", sensibilidad(a = a,c = c),   "\n")
  cat("Omission error (fraction):", f_error_om(a= a, c = c), "\n")
  cat("Commission error (fraction):", f_error_com(b = b, d = d), "\n")
  cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
}


all_result_down <- function(a,b,c,d){

  message1 <- capture.output({
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("                           Symbols                                       \n")
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("a = Correctly predicted presences\n")
    cat("b = Predicted present but actually absent\n")
    cat("c = Predicted absent but actually present\n")
    cat("d = Correctly predicted absences\n\n\n\n")

    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("                     Confusion Matrix                                    \n")
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

    print(confu_matrix(a,b,c,d))
    cat("\n\n\n\n")
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("                     Evaluation metrics for SDMs                         \n")
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    cat("Kappa: ", kappa(a=a,b=b,c=c,d=d),   "\n")
    cat("TSS: ", tss(a=a,b=b,c=c,d=d),       "\n")
    cat("Prevalence: ", prevalencia(a=a,b=b,c=c,d=d),"\n")
    cat("Correct classification rate: ", correct_class_rate(a=a,b=b,c=c,d=d),"\n")
    cat("Misclassification rate: ", miss_cla_rate(a=a,b=b,c=c,d=d),"\n")
    cat("Negative predictive power: ", nega_pre_pow(c = c, d=d),   "\n")
    cat("Positive predictive power: ", posit_pre_pow(a = a,b = b),   "\n")
    cat("False negative rate: ", tas_fals_neg(a = a,c = c),   "\n")
    cat("False positive rate: ",tas_fals_pos(b = b, d = d),   "\n")
    cat("Specificity: ", especificidad(b = b,d = d),   "\n")
    cat("Sensitivity: ", sensibilidad(a = a,c = c),   "\n")
    cat("Omission error (fraction):", f_error_om(a= a, c = c), "\n")
    cat("Commission error (fraction):", f_error_com(b = b, d = d), "\n")
    cat("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
  })
  return(message1)
}




cm_metrics <- reactive({

  a <- input$a
  b <- input$b
  c <- input$c
  d <- input$d

  if(input$elecc1=="kappa1"){
    #output$en <-TRUE
    return(kappa(a=a,b=b,c=c,d=d))
  }
  else if(input$elecc1 == "tss1"){
    #output$en <-FALSE
    return(tss(a=a,b=b,c=c,d=d))
  }
  #else if(input$binomial_test_==TRUE){
  #  return(binomial_test(a=a,b=b,c=c,d=d,prop=prop_area))
  #}
  else if(input$elecc1 == "prev1"){
    return(prevalencia(a=a,b=b,c=c,d=d))

  }
  else if(input$elecc1 == "tcc1"){
    return(correct_class_rate(a=a,b=b,c=c,d=d))
  }
  else if (input$elecc1 == "tcin1"){
    return(miss_cla_rate(a=a,b=b,c=c,d=d))
  }
  else if(input$elecc1 == "npp1"){
    return(nega_pre_pow(c = c, d=d))
  }
  else if(input$elecc1 == "ppp1"){
    return(posit_pre_pow(a = a,b = b))
  }
  else if(input$elecc1 == "tfn1"){
    return(tas_fals_neg(a = a,c = c))
  }
  else if(input$elecc1 == "tfp1"){
    return(tas_fals_pos(b = b, d = d))
  }
  else if(input$elecc1 == "espe1"){
    return(especificidad(b = b,d = d))
  }
  else if(input$elecc1 == "sens1"){
    return(sensibilidad(a = a,c = c))
  }
  else if(input$elecc1 == "feom1"){
    return(f_error_om(b= b, d = d))
  }
  else if(input$elecc1 == "fecom1"){
    return(f_error_com(a = a, c = c))
  }
  else if(input$elecc1 == "all1"){
    return(all_result(a=a,b=b,
               c=c,d=d))
  }

})


output$todoCM <- downloadHandler(
  filename <- function() paste0("conf_matrix_results",
                                ".txt"),
  content <- function(file){
    capture.output(all_result_down(a = input$a,
                                   input$b,input$c,
                                   input$d),file = file)
  }
)

output$cm_method_metrics <- renderPrint({
  cm_metrics()
})





# ----------------------------------------------------------------------------------
# Minimum training presence threshold


dat_val_mtp <- reactive({

  if (is.null(input$val_data_mtp))
    return(NULL)
  else if (identical(input$formatVal_mtp, 'CSV')){
    data <- read.csv(input$val_data_mtp$datapath,header=TRUE)
    return(data)
  }
  return()
})

mtp_threshold <- reactive({
  if(!is.null(dat_val_mtp()) && !is.null(sdm_raster())){
    coorde <- dat_val_mtp()[,2:3]
    threshold <- raster::extract(sdm_raster(),coorde)
    if(input$valdata_type=="min_traing_pres")
      return(min(threshold,na.rm = TRUE))
    if(input$valdata_type=="percentil_bin"){
      percentil <- as.numeric(input$percentil_th)
      thr <- quantile(threshold,  probs =  percentil/100)
      return(thr)
    }
  }
  else
    return()
})

#percentil_threshold <- reactive({
#    if(!is.null(dat_val_mtp()) && !is.null(sdm_raster()) && input$valdata_type == ""){
#      coorde <- dat_val_mtp()[,2:3]
#      threshold <- raster::extract(sdm_raster(),coorde)
#      thr <- quantile(threshold,  probs = 0.5)
#      return(thr)
#    }
#})


binary_mtp_method <- reactive({
  threshold <- mtp_threshold()
  model <- sdm_raster()

  if(!is.null(threshold) && !is.null(model)){
    #min_range <- cellStats(model,min)
    #max_range <- cellStats(model,max)
    #reclass_matrix <- matrix(c(min_range,threshold,
    #                           0,threshold,max_range,1),
    #                         ncol=3,byrow=TRUE)
    #rbin <- reclassify(model,rcl = reclass_matrix)
    rbin <- model > threshold
    return(rbin)
  }
  else
    return()

})

#output$binarymap_mtp <- renderPlot({
#  if(!is.null(binary_mtp_method()))
#    plot(binary_mtp_method())
#})

output$binary_mtp <- renderPlot({
  if(!is.null(binary_mtp_method()))
    return(plot(binary_mtp_method()))

})

binary_area_mtp_method <- reactive({
  if(!is.null(binary_mtp_method())){
    sdm_area <- distribution_area(binary_mtp_method())
    return(sdm_area)
  }
  else
    return()
})


output$area_mtp_method <- renderUI({
  area <- binary_area_mtp_method()
  if(!is.null(area)){
    threshold <-  mtp_threshold()
    area <- round(area,4)
    h3(paste("Species Distribution Area",area,"in km^2",
             "(at",threshold,", threshold)"))
  }
})




meta_data_mtp_method <- reactive({
  if(!is.null(binary_area_mtp_method())){
    threshold <-  mtp_threshold()
    area <- binary_area_mtp_method()
    model <- binary_mtp_method()
    xmin <- model@extent@xmin
    ymin <- model@extent@ymin
    xmax <- model@extent@xmax
    ymax <- model@extent@ymax
    pixel_res <- res(model)[1]
    area_df <- data.frame(SDM_area = area,
                          cutoff_threshold=threshold,
                          xmin,ymin,xmax,ymax,pixel_res)
    return(area_df)
  }
  else
    return()
})

output$downloadBinary_metadata_mtp <- downloadHandler(
  filename <- function() paste0("binaryMap_metadata_",
                                round(mtp_threshold(),4),
                                "_mtp.csv"),
  content <- function(file){
    if(!is.null(meta_data_mtp_method())){
      write.csv(meta_data_mtp_method(),file, row.names = FALSE)
    }
  }
)





output$downloadBinary_mtp <- downloadHandler(
  filename <- function() paste0("binaryMap_",
                                round(mtp_threshold(),4),
                                "mtp.asc"),
  content <- function(file){
    if(!is.null(binary_mtp_method())){
      writeRaster(binary_mtp_method(),file)
    }
  }
)


# ----------------------------------------------------------------------------------
# User defined threshold


binary_user_method <- eventReactive(input$compBin,{
  threshold <- input$thresholdBin
  model <- sdm_raster()

  if(!is.null(model)){

    min_range <- cellStats(model,min)
    max_range <- cellStats(model,max)
    reclass_matrix <- matrix(c(min_range,threshold,
                               0,threshold,max_range,1),
                             ncol=3,byrow=TRUE)
    rbin <- reclassify(model,rcl = reclass_matrix)
    return(rbin)
  }
  else
    return()
})


binary_area_user_method <- reactive({
  if(!is.null(binary_user_method())){
    sdm_area <- distribution_area(binary_user_method())
    return(sdm_area)
  }
  else
    return()
})

output$area_user_method <- renderUI({
  area <- binary_area_user_method()
  if(!is.null(area)){
    threshold <- input$thresholdBin
    area <- round(area,4)
    h3(paste("Species Distribution Area",area,"in km^2",
             "(at",threshold,", threshold)"))
  }
})




meta_data_user_method <- reactive({
  if(!is.null(binary_area_user_method())){
    threshold <- input$thresholdBin
    area <- binary_area_user_method()
    model <- binary_user_method()
    xmin <- model@extent@xmin
    ymin <- model@extent@ymin
    xmax <- model@extent@xmax
    ymax <- model@extent@ymax
    pixel_res <- res(model)[1]
    area_df <- data.frame(SDM_area = area,
                          cutoff_threshold=threshold,
                          xmin,ymin,xmax,ymax,pixel_res)
    return(area_df)
  }
  else
    return()
})

output$downloadBinary_metadata_user <- downloadHandler(
  filename <- function() paste0("binaryMap_metadata_",
                                input$thresholdBin,
                                "_user.csv"),
  content <- function(file){
    if(!is.null(meta_data_user_method())){
      write.csv(meta_data_user_method(),file, row.names = FALSE)
    }
  }
)



output$binary_user <- renderPlot({
  if(!is.null(binary_user_method()))
    return(plot(binary_user_method()))

})


output$downloadBinary_user <- downloadHandler(
  filename <- function() paste0("binaryMap_",
                                round(input$thresholdBin,4),
                                "user.asc"),
  content <- function(file){
    if(!is.null(binary_user_method())){
      writeRaster(binary_user_method(),file)
    }
  }
)
