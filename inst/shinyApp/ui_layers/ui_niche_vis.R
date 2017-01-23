layer_names <- c("Extract niche values "=1)
niche_view <- sidebarLayout(position = "left",
                            sidebarPanel(
                              busyIndicator("Computation In progress",wait = 0),
                              titlePanel("NicheToolBox"),
                              helpText("Here you can display the ellipsoid of your niche data."),
                              #selectInput("niche_data_set","Select the dataset that you want to visualize",
                              #            choices=c("GBIF data"="gbif_dataN")),
                              helpText("Choose the niche variables that you want to plot"),
                              selectInput(inputId = "x",label = "Select the variable in the x-axis",choices = layer_names),
                              selectInput(inputId = "y",label = "Select the variable in the y-axis",choices = layer_names),
                              selectInput(inputId = "z",label = "Select the variable in the z-axis",choices = layer_names),
                              selectInput(inputId = "gtype",label = "Choose one kind of plot",
                                          choices = c("Scatter plot"="disp",
                                                      "Ellipsoid plot"="corre"),selected="corre"),

                              conditionalPanel("input.gtype=='corre'",
                                               numericInput("ellipsoid_vol",
                                                            "Proportion of data to be included in the ellipoid",
                                                            min = 0.5,max = 1,value = 0.95,step = 0.01),
                                               selectInput(inputId = "fit",
                                                           label = "Want to see if there is trend? Select one model",
                                                           selected = "Ninguno",choices = c("Linear"="linear",
                                                                                            "Nothing"=NULL,
                                                                                            "Quadratic"="quadratic",
                                                                                            "Smooth"="smooth",
                                                                                            "Additive"="additive"),multiple = T)

                              ),
                              checkboxInput(inputId = "ellip",value = TRUE,label = "Display nichosphera"),
                              downloadButton(outputId = 'downloadLatLongNiche',label = 'Download Bios & Coords data')
                              #conditionalPanel(condition = "input.ellip==true", numericInput(""))

                            ),

                            mainPanel(
                              rglwidgetOutput("nicho",width = "800px",height = "800px")
                            ))
