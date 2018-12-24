ui_mop <- sidebarLayout(position = 'left',
                        sidebarPanel(
                          titlePanel("NicheToolBox"),
                          wellPanel(
                            h4("Mobility-Oriented Parity analysis (MOP)"),
                            p("MOP is an extrapolation risk analysis for model transfer see:"),
                            p("Owens et al. (2013; https://doi.org/10.1016/j.ecolmodel.2013.04.011).")
                          ),
                          br(),
                          wellPanel(
                            h5("M layers data"),
                            busyIndicator("Computation In progress",wait = 0),
                            br(),
                            selectInput(inputId = "mlayers_mop","Select the layers",choices = "Upload layers in AppSettings"),
                            selectInput(inputId = "mlayers_select","Select M layers to compute MOP"
                                        ,choices = c(NULL),multiple = TRUE)
                          ),
                          wellPanel(
                            h5("G layers data"),
                            #bsButton("loadGLayers","Load G layers",
                            #busyIndicator("Computation In progress",wait = 0),
                            br(),
                            selectInput(inputId = "glayers_mop","Select the layers",choices = "Upload layers in AppSettings"),
                            selectInput(inputId = "glayers_select","Select G layers to compute MOP"
                                        ,choices = c(NULL),multiple = TRUE)
                          ),
                          wellPanel(
                            numericInput(inputId = "ref_percent",
                                         label = "Percent of points sampled from te reference region (G)",
                                         value = 10,min = 1,max = 100,step = 1),
                            checkboxInput("normalized_mop",label = "Normalized",
                                          value = TRUE),
                            checkboxInput("parallel_comp",label = "Parallel computation",
                                          value=FALSE),
                            #numericInput("ncores_mop","Number of cores:",
                            #             value = 4,min=2,max = 1000000),
                            #conditionalPanel("input.parallel_comp==true",
                            numericInput(inputId = "comp_each",
                                         label = "Compute each",
                                         value = 2000,min=100,max=10000),
                            #                 ),
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
