options(rgl.useNULL=TRUE)
observe({
  cor_dataset <- NULL
  if(!is.null(occ_extract()))
    cor_dataset <- c(cor_dataset, "All raster extent"="wWorld")
  if(!is.null(occ_extract_from_mask()))
    cor_dataset <- c(cor_dataset,"Your shapefile of M"="mLayers")
  updateSelectInput(session, inputId = "kmeans_data_from",choices = cor_dataset)
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()))
      var_suggest <- summs_corr_var()$descriptors
    else
      var_suggest <- NULL
    updateSelectInput(session,"cluster_vars",
                      choices = names(data_extraction()),
                      selected = var_suggest)
  }
})



# K-means clustering

# K-means niche data input

#niche_data_k_means <- reactive({
#  input$load_kmeas_vars
#  if(input$load_kmeas_vars){
#    if(!is.null(data_extraction()) && length(input$cluster_vars)>2){
#      if(input$kmeans_data_from == "wWorld")
#        return(data_extraction())
#      if(input$kmeans_data_from == "mLayers")
#        return(occ_extract_from_mask()$data)
#    }
#    else
#      return(NULL)
#  }
#})

niche_data_k_means <- eventReactive(input$load_kmeas_vars,{
  if(!is.null(data_extraction()) && length(input$cluster_vars)>2){
    if(input$kmeans_data_from == "wWorld")
      return(data_extraction())
    if(input$kmeans_data_from == "mLayers")
      return(occ_extract_from_mask()$data)
  }
  else
    return(NULL)
})


# Geographic data

geographic_data <- reactive({
  if(input$datasetM == "gbif_dat" && !is.null(data_gbif()) && input$kmeans_data_from == "wWorld")
    return(data_gbif())
  if(input$datasetM == "updata" && !is.null(data_user_clean()) && input$kmeans_data_from == "wWorld")
    return(data_user_clean())
  if(input$datasetM == "gbif_dat" && !is.null(occ_extract_from_mask()) && input$kmeans_data_from == "mLayers")
    return(data_gbif()[occ_extract_from_mask()$xy_data_index,])
  if(input$datasetM == "updata"  && !is.null(occ_extract_from_mask()) && input$kmeans_data_from == "mLayers")
    return(data_user_clean()[occ_extract_from_mask()$xy_data_index,])
  else
    return(NULL)
})

# Niche Groups may reflect local adaptations

kmeans_df <- reactive({
  if(!is.null(niche_data_k_means())){
    if(input$kmeans_data_from == "mLayers"){

      niche_data_xy <- occ_extract_from_mask()$data_env_xy
      niche_data_xy <- na.omit(niche_data_xy)

      niche_data <- niche_data_xy[,-(1:2)]
      na_index <- attr( niche_data_xy,"na.action")
      level <- input$kmeans_level
      nclus <- as.numeric(input$nclust)
      #if(length(na_index) > 0L) niche_data <- niche_data[-na_index,]

      km <- kmeans(niche_data,centers=nclus,iter.max=100,trace=F)
      cluster <- km$cluster

      #if(length(na_index)>0L)
      #  geo_dat <- niche_data_xy[-na_index,1:2]
      #else
      geo_dat <- niche_data_xy[,(1:2)]

      kmeans_data <- data.frame(geo_dat,cluster = cluster,niche_data)
      return(kmeans_data)
    }
    if(input$kmeans_data_from == "wWorld"){

      niche_data <- niche_data_k_means()
      na_index <- which(is.na(niche_data))
      level <- input$kmeans_level
      nclus <- as.numeric(input$nclust)
      if(length(na_index) > 0L) niche_data <- niche_data[na_index,]

      km <- kmeans(na.omit(niche_data),centers=nclus,iter.max=100,trace=F)
      cluster <- km$cluster

      if(length(na_index)>0L)
        geo_dat <- occ_extract()$xy_data[-na_index,]
      else
        geo_dat <-occ_extract()$xy_data

      emessage <-  try({
        kmeans_data <- data.frame(geo_dat,
                                  cluster = cluster,
                                  niche_data)
      },silent = TRUE)

      if(class(emessage)=="try-error")
        return()


      return(kmeans_data)

    }


  }
  else
    return(NULL)
})


# Niche Groups may reflect local adaptations

kmeans_3d_plot_data <- reactive({
  if(!is.null(kmeans_df())){

    withProgress(message = 'Doing computations', value = 0, {
      niche_data <- niche_data_k_means()
      not_dup_niche_space <- which(!duplicated(niche_data))
      cluster_ids <- kmeans_df()$cluster
      cluster_ids <- cluster_ids[not_dup_niche_space]
      dat_clus <-  niche_data[not_dup_niche_space,c(input$x1,input$y1,input$z1)]
      vgrupo <-  geographic_data()[,input$vgrupo]
      if(input$kmeans_data_from == "mLayers")
        lat_long <- data_to_extract()[occ_extract_from_mask()$xy_data_index,]
      else
        lat_long <- data_to_extract()
      lat_long <- lat_long[not_dup_niche_space,]
      vgrupo <- vgrupo[not_dup_niche_space]
      d_b1 <- na.omit(dat_clus)
      d_b1 <- data.frame(d_b1)
      return(list(data=d_b1,cluster_ids=cluster_ids,vgrupo=vgrupo,lat_long=lat_long))
    })
  }
  else
    return(NULL)
})

# Kmeans metadata and plot

kmeans_3d_plot <- shiny::reactive({
  if(!is.null(kmeans_3d_plot_data())){
    k3d <- ntbox::ellipsoid_cluster_plot_3d(niche_data = kmeans_3d_plot_data()$data,
                                            cluster_ids = kmeans_3d_plot_data()$cluster_ids,
                                            mve = input$km_mve,
                                            vgrupo = kmeans_3d_plot_data()$vgrupo,
                                            x = input$x1,y = input$y1,
                                            z = input$z1,
                                            alpha = input$alpha,
                                            ellips = input$ellips,
                                            grupos=input$grupos,
                                            input$cex1,level=input$kmeans_level)
    return(k3d)

  }
  return()
})

output$kmeansmeta <- renderPrint({
  kmeansmetada <- kmeans_3d_plot()
  if(!is.null(kmeansmetada)){
    cat("----------------------------------------------------------\n")
    cat("       Ellipsoid metadata for each cluster\n")
    print(kmeansmetada)
    cat("----------------------------------------------------------\n")
  }
})

#observe({
#  if(!is.null(kmeans_3d_plot()))
#    print(kmeans_3d_plot())
#})

output$downloadKmetada <- downloadHandler(
  filename = "kmeans_metadata.txt",
  content = function(file) {
    if(!is.null(kmeans_3d_plot())){
      kmclust <- kmeans_3d_plot()
      capture.output({
        kmclust
      },file=file)
    }
  }
)

output$kmeans_clust_3d <- renderRglwidget({
  open3d(windowRect=c(100/2,100/2,700/2,700/2))
  if(!is.null(kmeans_3d_plot_data())){

    withProgress(message = 'Doing computations', value = 0, {

      kmeans_3d_plot()

    })
  }
  else if(!is.null(data_extraction()) && is.null(kmeans_3d_plot_data())){
    message <- "Press Go!!! button"
    text3d(x = 0,y = 0,texts = message)
  }
  else{
    message <- "No niche data: extract niche values from layers! (go to Niche space -> Niche data extraction)"
    text3d(x = 0,y = 0,texts = message)

  }
  rglwidget()
})

# Cluster projection in Geographic space

output$kmeans_geo <- renderPlot({
  if(!is.null(kmeans_3d_plot_data())){
    maps::map("world", fill=TRUE, col="white",
               ylim=c(-60, 90), mar=c(0,0,0,0))
    titlemap <- bquote(bold("K-means"~"clustering"~"projection"~"in"~"G-space"))
    title(titlemap,cex=40,cex.main = 2)
    axis(1,las=1)
    axis(2,las=1)
    cluster_legend <- unique(kmeans_3d_plot_data()$cluster_ids)
    colL <-   unique(kmeans_3d_plot_data()$cluster_ids)
    colores <- kmeans_3d_plot_data()$cluster_ids
    legend("topright",legend = cluster_legend,col=colL,pch=20,ncol = 2,cex=0.95)
    points(kmeans_3d_plot_data()$lat_long, col=colL,cex=1.5,pch=20)
  }
  else
    return(NULL)

})

leaflet_cluster_map <- reactive({
  if(!is.null(kmeans_3d_plot_data())){
    spatial_df <- sp::SpatialPointsDataFrame(
      kmeans_df()[,1:2],
      data.frame(cluster_ids=factor(
        kmeans_df()$cluster))
    )
    colores <- c("black","brown4","blue","cyan",
                 "darkgoldenrod","darkmagenta",
                 "darkgreen","chocolate4","azure3",
                 "chartreuse4","aquamarine","brown1",
                 "chocolate1","coral1")

    pal <-colorFactor(colores,
                      domain =1:length(colores))

    cluster_map <- leaflet(spatial_df) %>% addTiles() %>%
      addCircleMarkers(
        color = ~pal(cluster_ids),
        stroke = FALSE, fillOpacity = input$alpha + 0.27
      )
    return(cluster_map)
  }
  else
    return(NULL)
})

output$kmeans_geo_leaflet <- renderLeaflet({
  if(!is.null(leaflet_cluster_map())){
    leaflet_cluster_map()
  }
})


output$downloadKmeans <- downloadHandler(
  filename = function() return(paste0(input$genus,"_",input$species,"kmeans_data.csv")),
  content = function(file) {
    if(!is.null(kmeans_3d_plot_data())){
      #kmeans_df <- data.frame(kmeans_3d_plot_data()$lat_long,
      #                        kmeans_cluster=kmeans_3d_plot_data()$cluster_ids,
      #                        kmeans_3d_plot_data()$data)
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(kmeans_df(),file,row.names = FALSE)
    }
  }
)

observe({

  if(!is.null(data_extraction())){
    var_choices <- input$cluster_vars
    updateSelectInput(session,"x1",
                      choices = var_choices,
                      selected = var_choices[1])
    updateSelectInput(session,"y1",
                      choices = var_choices,
                      selected = var_choices[2])
    updateSelectInput(session,"z1",
                      choices = var_choices,
                      selected = var_choices[3])
    }
if(!is.null(geographic_data())){
  updateSelectInput(session,"vgrupo",
                    choices = names(geographic_data()))
}

})
