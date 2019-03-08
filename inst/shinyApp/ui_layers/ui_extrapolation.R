ui_extrapo <- sidebarLayout(position = 'left',
                            sidebarPanel(
                              titlePanel("NicheToolBox"),
                              h3("Extrapolation Risk assessment"),
                              wellPanel(
                                shiny::radioButtons("extra_mod","Choose a test",
                                                    choices = c("MOP"='mop',
                                                                "MESS"="mess",
                                                                "EXDET"='exdet')),
                                conditionalPanel("input.extra_mod == 'mop'",
                                                 h4("Mobility-Oriented Parity analysis (MOP)"),
                                                 p("MOP is an extrapolation risk analysis for model transfer see:"),
                                                 p("Owens et al. (2013; https://doi.org/10.1016/j.ecolmodel.2013.04.011).")
                                ),
                                conditionalPanel("input.extra_mod =='mess'",
                                                 h4("Multivariate Environmental Similarity Surface"),
                                                 p("Elith, J. , Kearney, M. and Phillips, S. (2010), The art of modelling",
                                                   "range‐shifting species. Methods in Ecology and Evolution,",
                                                   "1: 330-342. doi:10.1111/j.2041-210X.2010.00036.x")
                                                 ),
                                conditionalPanel("input.extra_mod =='exdet'",
                                                 h4("Exdet Metric"),
                                                 p("Mesgaran, M.B., Cousens, R.D. & Webber, B.L. (2014) Here be dragons: a tool for quantifying novelty due to covariate range and correlation change when projecting species distribution models. Diversity & Distributions, 20: 1147–1159, DOI: 10.1111/ddi.12209")
                                )

                              ),
                              br(),
                              wellPanel(
                                h5("M layers data"),
                                busyIndicator("Computation In progress",wait = 0),
                                br(),
                                selectInput(inputId = "mlayers_extra","Select the layers",choices = "Upload layers in AppSettings"),
                                selectInput(inputId = "mlayers_select","Select M layers to compute MOP"
                                            ,choices = c(NULL),multiple = TRUE)
                              ),
                              wellPanel(
                                h5("G layers data"),
                                #bsButton("loadGLayers","Load G layers",
                                #busyIndicator("Computation In progress",wait = 0),
                                br(),
                                selectInput(inputId = "glayers_extra","Select the layers",choices = "Upload layers in AppSettings"),
                                selectInput(inputId = "glayers_select","Select G layers to compute MOP"
                                            ,choices = c(NULL),multiple = TRUE)
                              ),

                              wellPanel(

                                conditionalPanel("input.extra_mod == 'mop'",
                                                 numericInput(inputId = "ref_percent",
                                                              label = "Percent of points sampled from te reference region (M)",
                                                              value = 10,min = 1,max = 100,step = 1),
                                                 checkboxInput("normalized_mop",label = "Normalized",
                                                               value = TRUE),
                                                 checkboxInput("parallel_comp",label = "Parallel computation",
                                                               value=FALSE),
                                                 numericInput("ncores_mop","Number of cores:",
                                                              value = 4,min=2,max = 1000000),
                                                 #conditionalPanel("input.parallel_comp==true",
                                                 numericInput(inputId = "comp_each",
                                                              label = "Compute each",
                                                              value = 2000,min=100,max=10000),
                                                 bsButton("run_mop","Run MOP"),
                                                 downloadButton("mop_raster")

                                ),
                                conditionalPanel("input.extra_mod == 'mess'",
                                                 bsButton("run_mess","Run MESS"),
                                                 downloadButton("mess_raster")
                                                 ),

                                conditionalPanel("input.extra_mod == 'exdet'",
                                                 radioButtons(inputId = "exdet_choose","Select an option",
                                                             c("NT1"= "exdet_univar",
                                                               "NT2"= "exdet_mult")
                                                            ),

                                                 conditionalPanel("input.exdet_choose == 'exdet_univar'",
                                                                  bsButton("run_nt1","Run NT1"),
                                                                  downloadButton("nt1_raster")

                                                 ),


                                                 conditionalPanel("input.exdet_choose == 'exdet_mult'",
                                                                  bsButton("run_nt2","Run NT2"),
                                                                  downloadButton("nt2_raster")

                                                 )



                                )



                                #                 ),


                              )

                            ),
                            mainPanel(#plotOutput("show_m_g_layers"),
                              #h3("MOP results"),
                              plotOutput("mop_plot"),
                              plotOutput("mess_plot"),
                              plotOutput("exdet_univarC"),
                              plotOutput("exdet_multvarC")
                            )
)
