
source("load_install_pkgs.R")
ellipsoid_models_c <- sidebarLayout(position = "left",
                            sidebarPanel(
                              titlePanel("NicheToolBox"),
                              h3("Ellipsoid Model"),
                              busyIndicator("Computation In progress",wait = 0),
                              selectInput("selectShape","Select a region to train the model",
                                          choices =NULL),

                              selectInput("selectM","Select a region to project the ellipsoid",
                                          choices = NULL),
                              selectInput(inputId = "biosEllip","Select the variables",choices = NULL,multiple = TRUE),
                              numericInput("prop_points",
                                           "Proportion of niche points inside the ellipsoid",
                                           value = 0.95,min = 0.5,max=0.99),
                              #h5("Train the model"),
                              #shiny::actionButton("train_ellips",label = "Train model"),
                              h5("Run your model"),
                              conditionalPanel("input.selectM == 'wWorld' && input.selectShape == 'wWorld'",
                                               shiny::actionButton("selectBios_all_all_train",label = "Run model 1")
                              ),
                              conditionalPanel("input.selectM=='wWorld' && input.selectShape== 'mLayers'",
                                               shiny::actionButton("selectBios_all_m_train",label = "Run model 2")
                              ),
                              conditionalPanel("input.selectM=='mLayers' && input.selectShape == 'wWorld'",
                                               shiny::actionButton("selectBios_m_all_train",label = "Run model 3")
                              ),
                              conditionalPanel("input.selectM=='mLayers' && input.selectShape == 'mLayers'",
                                               shiny::actionButton("selectBios_m_m_train",label = "Run model 4")
                              ),

                              busyIndicator("Computation In progress",wait = 0),
                              h5("Download Ellipsoid Meta-Data"),
                              downloadButton("downShapMat","Download"),
                              h5("Download your model in .asc"),
                              downloadButton(outputId = "downEllipRas",label = "Download model"),
                              #h5("Download plot"),
                              #downloadButton(outputId = "EllipRasterPlot",label = "Download plot"),
                              h5("Enviromental distances to the centroid table"),
                              downloadButton(outputId = "downEllipDistance",label = "Distance DataTable")

                            ),
                            mainPanel(
                              uiOutput("ellip_models_plots")
                            ))
