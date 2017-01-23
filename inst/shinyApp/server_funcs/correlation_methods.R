# Correlation Analysis

#---------------------------------------------------------------------------------
# Correlation table

observe({
  cor_dataset <- NULL
  if(!is.null(occ_extract()))
    cor_dataset <- c(cor_dataset, "All raster extent"="wWorld")
  if(!is.null(occ_extract_from_mask()))
    cor_dataset <- c(cor_dataset,"Your polygon of M"="mLayers")
  updateSelectInput(session, inputId = "cor_data_from",choices = cor_dataset)
})

corr_table <- reactive({
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract()) && input$cor_data_from == "wWorld"){
      niche_data <- occ_extract()
      cor_table <- cor(niche_data,
                       use = "pairwise.complete.obs")
      return(cor_table)
    }

    if(!is.null(occ_extract_from_mask()) && input$cor_data_from == "mLayers"){
      niche_data <- occ_extract_from_mask()$data
      cor_table <- cor(niche_data,
                       use = "pairwise.complete.obs")
      return(cor_table)
    }



  }
  else return(NULL)

})


output$corr_table <- renderDataTable({
  withProgress(message = 'Doing Computations', value = 0, {
    if(!is.null(corr_table())){
      niche_bivar_corr <- corr_table()
      var_names <- colnames(niche_bivar_corr)
      corTable <- cbind(var_names,niche_bivar_corr)
      return(corTable)
    }
    else{
      message <- "No niche data: extract niche values from layers!
                       (go to Niche space -> Niche data extraction)"
      df <- data.frame(NoNicheData = message)
      return(df)
    }
  })
})

output$download_cor_table <- downloadHandler(
  #filename = function() return(paste0(input$genus,"_",input$species,"M_rasters.tar")),
  filename = "ntb_correlation_table.csv",
  content = function(file) {
    if(!is.null(corr_table())){
      niche_bivar_corr <- corr_table()
      var_names <- colnames(niche_bivar_corr)
      corTable <- cbind(var_names,niche_bivar_corr)
      write.csv(corTable,file,row.names = FALSE)
    }
  }
)


#---------------------------------------------------------------------------------
# Correlation plot


corr_plot <- reactive({

  if(!is.null(corr_table())){
    # Color palette
    col1 <- colorRampPalette(
      c("#7F0000","red","#FF7F00","yellow","white",
        "cyan", "#007FFF", "blue","#00007F")
    )

    niche_bivar_corr <- corr_table()

    return(corrplot(niche_bivar_corr,
                    method="ellipse",
                    col=col1(200),order = "AOE"))
  }
  else return(NULL)


})


output$corr_plot <- renderPlot({
  #datos <- data()
  withProgress(message = 'Doing computations', value = 0, {
    if(!is.null(corr_plot()))
      corr_plot()
    else{
      message <- "No niche data: extract\nniche values from layers!\n(go to Niche space -> Niche data extraction)"
      x <- -10:10
      y <- x
      plot(x,y,type="n")
      text(0,1,message,cex=2.5)
    }
  })
})

output$download_cor_plot <- downloadHandler(
  #filename = function() return(paste0(input$genus,"_",input$species,"M_rasters.tar")),
  filename = "ntb_correlation_plot.pdf",
  content = function(file) {
    if(!is.null(corr_table())){
      pdf(file,width = 8,height = 8)
      corr_plot()
      dev.off()
    }
  }
)


#-----------------------------------------------------------
# Correlation finder
#-----------------------------------------------------------

print_corfinder <- function(descriptors,list_cor,threshold){

  message <- capture.output({

    cat('*****************************************************************\n\n')
    cat(' Here is a list of variables that can summarize your niche\n')
    cat(' information, according to the threshold of',threshold,":\n\n")
    cat(' ',descriptors,'\n\n')
    cat('*****************************************************************\n\n')
    cat('----------------------------------------------------------------\n\n')
    cat('Correlation list:\n\n')

    for(i in 1:length(list_cor)){
      cat("Variable",names(list_cor)[i],"is strongly correlated with:\n\n")
      print(list_cor[[i]])
      cat('----------------------------------------------------------------\n\n')
    }
  })
  return(message)
}

summs_corr_var<- reactive({

  if(!is.null(corr_table())){
    cor_mat <- corr_table()

    cor_vars  <- correlation_finder(cor_mat = cor_mat,
                                    threshold = input$cor_threshold,
                                    verbose = input$verbose_cor)

    cor_vars2  <- correlation_finder(cor_mat = cor_mat,
                                    threshold = input$cor_threshold,
                                    verbose = FALSE)
    cor_vars_summary <- print_corfinder(cor_vars2$descriptors,
                                        cor_vars2$list_cor,
                                        input$cor_threshold)

    return(list(cor_vars_summary=cor_vars_summary,cor_vars=cor_vars,descriptors=cor_vars2$descriptors))

  }
  else
    return(NULL)
})

output$big_cor <- renderPrint({
  if(!is.null(corr_table())){
    cor_mat <- corr_table()
    return(correlation_finder(cor_mat = cor_mat,
                              threshold = input$cor_threshold,
                              verbose = input$verbose_cor))
  }
  else{
    cat("No niche data: extract niche values from layers!
        \n(go to Niche space -> Niche data extraction)")
  }

})

output$download_stcor <- downloadHandler(
  filename = "strongcors.txt",
  content = function(file) {
    if(!is.null(summs_corr_var())){
      cor_mat <- corr_table()
      capture.output(correlation_finder(cor_mat = cor_mat,
                                threshold = input$cor_threshold,
                                verbose = input$verbose_cor),file=file)
    }
  }
)

