#---------------------------------------------------------------------------------
# MOP

ui_mop <- sidebarLayout(position = 'left',
                             sidebarPanel(
                               titlePanel("NicheToolBox"),
                               br(),
                               wellPanel(
                                 h5("M layers data"),
                                 directoryInput('m_layers_directory',
                                                label = 'Select M layers directory'),
                                 bsButton(inputId = "loadMLayers","Load M layers",
                                          icon = icon("upload", lib = "glyphicon"),style = "primary"),
                                 busyIndicator("Computation In progress",wait = 0),
                                 br(),
                                 selectInput(inputId = "mlayers_select","Select M layers to compute MOP"
                                             ,choices = c(NULL),multiple = TRUE)
                                 ),
                               wellPanel(
                                 h5("G layers data"),
                                 directoryInput('g_layers_directory',
                                                label = 'Select G layers directory'),
                                 bsButton("loadGLayers","Load G layers",
                                          icon = icon("upload", lib = "glyphicon"),style = "primary"),
                                 busyIndicator("Computation In progress",wait = 0),
                                 br(),
                                 selectInput(inputId = "glayers_select","Select M layers to compute MOP"
                                             ,choices = c(NULL),multiple = TRUE)
                               ),
                               wellPanel(
                                 numericInput(inputId = "ref_percent",
                                              label = "Percent of points sampled from te reference region (G)",
                                              value = 10,min = 1,max = 100,step = 0.1),
                                 checkboxInput("normalized_mop",label = "Normalized",value = TRUE),
                                 bsButton("run_mop","Run MOP")

                               ),
                               wellPanel(
                                 downloadButton("mop_raster")

                               )
                               ),
                             mainPanel(#plotOutput("show_m_g_layers"),
                                       h3("MOP results"),
                                       plotOutput("mop_plot")
                                       )
                             )
