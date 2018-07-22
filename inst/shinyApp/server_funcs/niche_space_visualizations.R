options(rgl.useNULL=TRUE)
# Visualizations in niche space

niche_data <- reactive({
  if(!is.null(data_extraction()))
    return(data_extraction())
  else
    return(NULL)
})


## Enviromental niche space plot

source("scatter3d/scatterplot.R")
options(rgl.useNULL=TRUE)
# Visualizations in niche space

niche_plot <- function(data,x,y,z,gtype,ajus,ellip,prop){

    if(gtype=="corre"){
      ifelse(!is.null(ajus),surf <- TRUE,no = surf <-FALSE)
      formula <- as.formula(paste0(y,"~",x,"+",z))
      scatter3d(formula,point.col="gray8",level = prop,grid = TRUE,
                xlab = x,ylab = z,zlab = y,data=data,
                surface = surf,ellipsoid = ellip,fit=ajus,
                ellipsoid.alpha = 0.2,cex = 3,col="darkgreen")

    }
    else if(gtype=="disp"){
      rgl::plot3d(data,col=rainbow(1000),cex = 3)

    }
  }


# Vew the 3D Enviromental space

output$nicho <- renderRglwidget({

  withProgress(message = 'Doing computations', value = 0, {
    gtype <- input$gtype

    x <- input$x
    y <- input$y
    z <- input$z
    ajus <- input$fit
    ellip <- input$ellip
    prop <- as.numeric(input$ellipsoid_vol)
    open3d(windowRect=c(100,100,700,700))
    if(!is.null(niche_data())){

      niche_plot(data = niche_data(),x = x,y = y,z = z,prop =prop ,
                 gtype = gtype,ajus = ajus,ellip = ellip)

    }
    else{
      message <- "No niche data: extract niche values from layers! (go to Niche space -> Niche data extraction)"
      text3d(x = 0,y = 0,texts = message)

    }
    rglwidget()
  })

})


# Download data table with niche vars values

output$downloadLatLongNiche <- downloadHandler(
  filename = function() return(paste0(input$genus,"_",input$species,"niche_lat_long_data.csv")),
  content = function(file) {
    if(!is.null(data_extraction())){
      ## Leyendo los datos de la especie e escriendolos en un .csv
      write.csv(cbind(data_to_extract() ,data_extraction()),file,row.names = FALSE)
    }
  }
)




observe({
  if(!is.null(niche_data())){
    updateSelectInput(session,"x",choices = names(niche_data()),
                      selected = names(niche_data())[1])
    updateSelectInput(session,"y",choices = names(niche_data()),
                      selected =  names(niche_data())[2])
    updateSelectInput(session,"z",choices = names(niche_data()),
                      selected =  names(niche_data())[3])
  }
})
