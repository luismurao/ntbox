
# MOP M layeres directory

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$m_layers_directory
  },
  handlerExpr = {
    if (input$m_layers_directory > 0) {
      # condition prevents handler execution on initial app launch

      # launch the directory selection dialog with initial path read from the widget
      path = choose.dir(default = readDirectoryInput(session, 'm_layers_directory'))

      # update the widget value
      updateDirectoryInput(session, 'm_layers_directory', value = path)

    }
  }
)

# MOP G layeres directory


observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$g_layers_directory
  },
  handlerExpr = {
    if (input$g_layers_directory > 0) {
      # condition prevents handler execution on initial app launch

      # launch the directory selection dialog with initial path read from the widget
      path = choose.dir(default = readDirectoryInput(session, 'g_layers_directory'))

      # update the widget value
      updateDirectoryInput(session, 'g_layers_directory', value = path)

    }
  }
)




# M layers directory
MLayersDir <- reactive({
  path <- readDirectoryInput(session, 'm_layers_directory')
  if(length(path)>0L)
    return(path)
  else
    return(NULL)
})

M_ras_Layers <- eventReactive(input$loadMLayers,{
  layers_dir <- MLayersDir()
  isolate({
    if(length(layers_dir) > 0L)
      return(rlayers_ntb(layers_dir))
    else
      return(NULL)
  })
})

# read G layers

GLayersDir <- reactive({
  path <- readDirectoryInput(session, 'g_layers_directory')
  if(length(path)>0L)
    return(path)
  else
    return(NULL)
})

G_ras_Layers <- eventReactive(input$loadGLayers,{
  layers_dir <- GLayersDir()
  isolate({
    if(length(layers_dir) > 0L)
      return(rlayers_ntb(layers_dir))
    else
      return(NULL)
  })
})

observe({
  if(!is.null(M_ras_Layers())){
    updateSelectInput(session,inputId = "mlayers_select",
                      choices = names(M_ras_Layers()),
                      selected =  names(M_ras_Layers()))
  }

  if(!is.null(G_ras_Layers())){
    updateSelectInput(session,inputId = "glayers_select",
                      choices = names(G_ras_Layers()),
                      selected =  names(G_ras_Layers()))
  }


})



mop_comp <- eventReactive(input$run_mop,{
  m_layers <- M_ras_Layers()[[input$mlayers_select]]
  g_layers <- G_ras_Layers()[[input$glayers_select]]
  mop_names <- all(names(m_layers)==names(g_layers))

  per_prop <- as.numeric(as.character(input$ref_percent))
  if(mop_names){

    mop_anlysis <- mop(m_stack =  m_layers,
                       g_stack = g_layers,
                       percentil_prop= per_prop
                       )
    return(mop_anlysis)

  }
  else
    return(NULL)

})



output$show_m_g_layers <- renderPlot({

  if(!is.null(M_ras_Layers()) & is.null(G_ras_Layers())){
    plot(M_ras_Layers()[[1]])
  }
  if(!is.null(M_ras_Layers()) & !is.null(G_ras_Layers())){
    par(mfrow=c(1,2))
    plot(M_ras_Layers()[[1]])
    plot(G_ras_Layers()[[1]])
  }
  else{
    messages <- "Load M and G layers"
    x <- -10:10
    y <- x
    plot(x,y,type="n", xlab="No Data", ylab="No data",cex=2)
    text(0,0,messages,cex=3 )
  }

})

output$mop_plot <- renderPlot({
  if(!is.null(mop_comp())){
    #colramp <- colorRampPalette(c("#2cd81c","#385caa",
    #                              "#1825df","black"))(226)
    colramp <- colorRampPalette(c("#1210d9","#7605e0",
                                  "#a618d1","#d3168c",
                                  "#ea1136"))(226)
    mop_raster<- mop_comp()

    if(!input$normalized_mop)
      plot(mop_raster[[1]],col=colramp)


    if(input$normalized_mop){
      colramp <- rev(colramp)
      plot(mop_raster[[2]],col=colramp)
    }



  }

})

output$mop_raster <- downloadHandler(
  filename <- function() {paste0("mop_results_",
                                 as.numeric(input$ref_percent),
                                 "_percent_normalized_",
                                 input$normalized_mop,".asc")},
  content <- function(file){
    if(!is.null(mop_comp())){
      if(input$normalized_mop)
        writeRaster(mop_comp()[[2]],filename = file)
      else
        writeRaster(mop_comp()[[1]],filename = file)
      }
    })


