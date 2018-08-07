
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Uploaded Raster Data
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat_raster <- reactive({
  if (is.null(input$sdm_mod))
    return(NULL)
  else if(identical(input$format2,'.asc'))
    path_ras <- input$sdm_mod$datapath
  else if(identical(input$format2,'.tif'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$format2,'.bil'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$format2,'.nc'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$format2,'.sdat'))
    path_ras <- input$fileBin$datapath
  else if(identical(input$format2,'.img'))
    path_ras <- input$fileBin$datapath
  return(raster::raster(path_ras))

})
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Uploaded validation data
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dat_presence <- reactive({

  if (is.null(input$occ_proc))
    return(NULL)
  else if (identical(input$format3, 'CSV'))
    return(input$occ_proc$datapath)
})


# Partial ROC settings  (global variable)

partialRoc <- reactive({
  sims <- as.numeric(input$iter)
  error <-as.numeric(input$omission)*100
  randper <- as.numeric(input$randper)

  if(!is.null(dat_presence()) && !is.null(dat_raster())){

    test_data <- read.csv(dat_presence())
    pRoc <- ntbox::pROC(continuos_mod = dat_raster(),
                        test_data = test_data[,-1],
                        n_iter = sims,
                        E_percent = error,
                        boost_percent = randper,
                        parallel = TRUE)
    pRoc <- pRoc[[2]]
    pRoc <- data.frame(Iteration=1:dim(pRoc)[1],pRoc )
    names(pRoc) <- c("Iteration","AUC_partial","AUC_prandom","AUC_ratio")

    return(pRoc)
  }
  return()
})

pRoc_null <- reactive({
  ifelse(!is.null(dat_raster()) == TRUE,
         m_ras <- "Raster uploaded",
         m_ras <- "Upload prediction raster")
  ifelse(!is.null(dat_presence()),
         m_pres <- "Presences uploaded",
         m_pres <- "Upload validation data")
  mensaje <- data.frame(Raster=c(m_ras),Presences =c(m_pres))
  return(mensaje)
})

# Data table for partialRoc simulations

output$rocPart <- renderDataTable({

  if(!is.null(partialRoc())){
    return(partialRoc())
  }
  else{
    return(pRoc_null())
  }
},options = list(aLengthMenu = c(5, 10, 25,
                                 50, 100, 500),
                 iDisplayLength = 5))

# Partial RocPlot

pRoc_distribution <- reactive({

  dataP <- partialRoc()

  if(!is.null(dataP)){

    aucRatio <- dataP$AUC_ratio
    aucRand <- random <- rnorm(n=500,mean=1,sd=sd(aucRatio))
    dens_rnd <- density(aucRand,adjust=2)
    dens_ratio <- density(aucRatio,adjust=2)

    return(list(aucRatio=aucRatio,dens_rnd=dens_rnd,dens_ratio=dens_ratio))



  }
  else
    return(NULL)
})

output$rocPartPlot <- renderPlot({
  auc_res <- pRoc_distribution()
 if(!is.null(auc_res)){

   hist(auc_res$aucRatio,prob=TRUE,col="grey",xlim=c(min(auc_res$dens_rnd$x),max(auc_res$aucRatio)),
        main="Partial AUC distribution", xlab="AUC ratio")
   lines(auc_res$dens_rnd,col='red')
   lines(auc_res$dens_ratio,col='blue')
 }
  else{
   ifelse(!is.null(dat_raster()) == TRUE,
          m_ras <- "Raster uploaded",
          m_ras <- "Upload prediction raster")
   ifelse(!is.null(dat_presence()),
          m_pres <- "Presences uploaded",
          m_pres <- "Upload validation data")
   plot(-5:5,-5:5,type="n")
   legend("center",paste0(m_ras,"/",m_pres),cex = 2.3,bty = "n")
 }


})

# Partial Roc statistics

pRocStats <- reactive({

  dataP <- partialRoc()

  if(!is.null(dataP)){
    cat('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
    cat('                         Statistics for Partial Roc                                 \n')
    cat('                          after',input$iter, 'simulations                          \n')
    cat('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

    cat('The mean value for AUC ratio at, ',1 - input$omission,' is:',mean(dataP[,4]),'\n\n')
    cat('The mean value for partial AUC at, ',1 - input$omission,' is:',mean(dataP[,2]),'\n\n')
    cat('The mean value for partial AUC at random is:',mean(dataP[,3]),'\n\n')


    cat('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
    cat('                         Statistics for the difference between                         \n')
    cat('                       AUC random and AUC from model prediction                      \n')
    cat('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')
    cat('      Ho: The difference between AUC from  model prediction and AUC at random is <=0\n')
    cat('      Ha: The difference between AUC from  model prediction and AUC at random is  >0\n')
    cat('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

    print(describe(dataP$AUC_ratio)[c(2,3,4,5,7,8,9,10,11,12)])
    #mod <- t.test(y = dataP$AUC_at_0.5,x = dataP[,2],alternative = 'greater')
    mod <- 1 - (length(which(dataP[,4]>1))/length(dataP[,4]))
    mod <- list(p.value=mod)
    cat('\n\n')
    if(mod$p.value<0.001) pval <- paste0(round(mod$p.value,4),' ***')
    if(mod$p.value< 0.01 && mod$p.value> 0.001) pval <- paste0(round(mod$p.value,4),' **')
    if(mod$p.value< 0.05 && mod$p.value > 0.01) pval <- paste0(round(mod$p.value,4),' *')
    if(mod$p.value > 0.05) pval <- round(mod$p.value,4)

    cat("The p-value for the difference between means (AUC random and AUC partial) is: ",pval)
    if(mod$p.value<0.05)
      cat('\n\nReject Ho and Accept Ha: The difference between AUC from  model prediction and AUC at random is  >0')
    else
      cat('\n\nAccept Ho and Reject Ha')
    cat('\n\n\n')
    return(mod)
  }
  else
    return()

})


output$pStats <- renderPrint({
  if(!is.null(pRocStats()))
    return(pRocStats())
  else
    return()
})


output$partRocT <- downloadHandler(
  filename = function() return(paste0("pROC_results.csv")),
  content = function(file) {
    if(!is.null(partialRoc())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(partialRoc(),file,row.names = FALSE)
    }
  }
)
