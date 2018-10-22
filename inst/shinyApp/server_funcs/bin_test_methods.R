btest_raster <- reactive({
  if (is.null(input$sdm_map))
    return(NULL)
  else if(identical(input$bin_format,'.asc'))
    path_ras <- input$sdm_map$datapath
  else if(identical(input$bin_format,'.tif'))
    path_ras <- input$sdm_map$datapath
  else if(identical(input$bin_format,'.bil'))
    path_ras <- input$sdm_map$datapath
  else if(identical(input$bin_format,'.nc'))
    path_ras <- input$sdm_map$datapath
  else if(identical(input$bin_format,'.sdat'))
    path_ras <- input$sdm_map$datapath
  else if(identical(input$bin_format,'.img'))
    path_ras <- input$sdm_map$datapath
  return(raster::raster(path_ras))
})


binomial_raster <- eventReactive(input$run_binomial,{
  if(input$model_type == 'bin_conti' && !is.null(btest_raster())){
    mthreshold <- as.numeric(input$bin_threshold)
    binary_map <- btest_raster() >= mthreshold
    return(binary_map)
  }
  if(input$model_type == 'bin_binary' && !is.null(btest_raster())){
    return(btest_raster())
  }
  else
    return()
})



d_testBin <- reactive({
  if (is.null(input$occ_binomial))
    return(NULL)
  else if (identical(input$testdata_format, 'CSV'))
    return(read.csv(input$occ_binomial$datapath))
})


output$bintest_map <- renderPlot({
  if(!is.null(binomial_raster()))
    plot(binomial_raster())
})

binomial_testDF <- reactive({
  if(!is.null(binomial_raster()) && is.data.frame(d_testBin())){
    bin_res <- binomial_test(binomial_raster(), d_testBin()[,-1])
    return(bin_res)
  }
})

output$binomal_results <- renderDataTable({
  if(is.data.frame(binomial_testDF()))
    return(binomial_testDF())
})

output$binomial_down <- downloadHandler(
  filename = function() return(paste0("binomial_test_results.csv")),
  content = function(file) {
    if(is.data.frame(binomial_testDF())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(binomial_testDF(),file,row.names = FALSE)
    }
  }
)

