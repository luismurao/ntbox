# Bliclim to NicheToolBox Methods
# ui interface

bioclim_ui <- sidebarLayout(position = "left",
                              sidebarPanel(
                                titlePanel("NicheToolBox"),
                                h3("Bioclim"),
                                busyIndicator("Computation In progress",wait = 0),
                                selectInput("trainBio","Select a region to train the model",
                                            choices = c("All raster extent"="wWorld","Your shapefile of M"="mLayers")),
                                selectInput("selectMBio","Select a region to project the model",
                                            choices = c("All raster extent"="wWorld",
                                                        "Your shapefile of M"="mLayers")),
                                selectInput(inputId = "biosBioclim","Select the variables",choices = NULL,multiple = TRUE),
                                uiOutput("brand"),
                                conditionalPanel("input.trainBio == 'wWorld' && input.selectMBio == 'wWorld'",
                                                 actionButton("run_bioclim_all_all_train",
                                                              label = "Run Bioclim 1")
                                                 ),
                                conditionalPanel("input.trainBio == 'wWorld' && input.selectMBio == 'mLayers'",
                                                 actionButton("run_bioclim_m_all_train",
                                                              label = "Run Bioclim 2")
                                ),
                                conditionalPanel("input.trainBio == 'mLayers' && input.selectMBio == 'wWorld'",
                                                 actionButton("run_bioclim_all_m_train",
                                                              label = "Run Bioclim 3")
                                ),
                                conditionalPanel("input.trainBio == 'mLayers' && input.selectMBio == 'mLayers'",
                                                 actionButton("run_bioclim_m_m_train",
                                                              label = "Run Bioclim 4")
                                ),

                                h5("Download your model in .asc"),
                                downloadButton(outputId = "downBiclimRas",label = "Download model")

                              ),
                              mainPanel(
                                conditionalPanel("input.selectMBio == 'wWorld' && input.trainBio == 'wWorld'",
                                                 plotOutput("bio_response_all_all_train")),
                                conditionalPanel("input.selectMBio == 'wWorld' && input.trainBio == 'mLayers'",
                                                 plotOutput("bio_response_all_m_train")),
                                conditionalPanel("input.selectMBio == 'mLayers' && input.trainBio == 'wWorld'",
                                                 plotOutput("bio_response_m_all_train")),
                                conditionalPanel("input.selectMBio == 'mLayers' && input.trainBio == 'mLayers'",
                                                 plotOutput("bio_response_m_m_train"))
                              ))
