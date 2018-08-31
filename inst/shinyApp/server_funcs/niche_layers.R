# Read raster layer with the same extent and resolution,
# no matter if they are in different formats (reads all `raster`accepted formats

output$niche_layers <- renderPlot({

  if(!is.null(rasterLayers())){
    plot(rasterLayers()[[1]])
  }
  else{
    messages <- "Load niche layers"
    x <- -10:10
    y <- x
    plot(x,y,type="n", xlab="No Data", ylab="No data",cex=2)
    text(0,0,messages,cex=3 )
  }

})



output$niche_layers_proj <- renderPlot({

  if(!is.null(proj_rasterLayers())){
    plot(proj_rasterLayers()[[1]])
  }
  else{
    messages <- "Load projection layers"
    x <- -10:10
    y <- x
    plot(x,y,type="n", xlab="No Data", ylab="No data",cex=2)
    text(0,0,messages,cex=3 )
  }

})
