ui_maxent  <- sidebarLayout(position = "left",
                            sidebarPanel(
                              tags$head(

                                tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
                              ),
                              titlePanel("NicheToolBox"),
                              h3("MaxEnt"),
                              wellPanel(
                                h4("Main features"),
                                busyIndicator("Computation In progress",wait = 0),
                                selectInput("selectDataMaxEnt","Select a dataset to train the model",
                                            choices =NULL),
                                selectInput("selectM_MaxEnt","Select a region to project the model",
                                            choices = NULL),
                                selectInput(inputId = "biosMaxEnt","Select the variables",choices = NULL,multiple = TRUE),
                                #checkboxGroupInput("maxent_response","Features",choices = c("Linear features"="linear",
                                #                                                            "Quadratic features"="quadratic",
                                #                                                            "Product features"="product",
                                #                                                            "Threshold features"="threshold",
                                #                                                            "Hinge features"="hinge"),
                                #                   selected = c("linear","quadratic","product","threshold","hinge")),
                                strong(p("Features")),
                                checkboxInput("max_linear","Linear features",value = TRUE),
                                checkboxInput("max_quadratic","Quadratic features",value = TRUE),
                                checkboxInput("max_product","Product features",value = TRUE),
                                checkboxInput("max_threshold","Threshold features",value = TRUE),
                                checkboxInput("max_hinge","Hinge features",value = TRUE),
                                checkboxInput("max_logscale","Logscale pictures",value = TRUE),
                                checkboxInput("max_jackknife","Do Jackknife",value = TRUE),
                                checkboxInput("max_responsecurves","Create response curves",value=TRUE),
                                selectInput("maxent_output","Output format",choice=c("Logistic"="logistic",
                                                                                     "Cumulative"="cumulative",
                                                                                     "Raw"="raw")),

                                conditionalPanel("(input.selectDataMaxEnt== 'user_data' || input.selectDataMaxEnt== 'gbif_data') && input.selectM_MaxEnt == 'all_raster'",
                                                 actionButton("run_maxent_all_all","Run"),
                                                 downloadButton("max_model_all_all","Model")),

                                conditionalPanel("(input.selectDataMaxEnt== 'user_data' || input.selectDataMaxEnt== 'gbif_data') && input.selectM_MaxEnt == 'mLayers'",
                                                 actionButton("run_maxent_all_mLayers","Run"),
                                                 downloadButton("max_model_all_M","Model"))

                              ),
                              h4("Settings"),
                              wellPanel(h5("Basic"),
                                        checkboxInput("max_remv_dup","Remove duplicate presence records",value = TRUE),
                                        numericInput("max_rand_test_per","Random test percentage",value = 0,min = 0,max=100),
                                        numericInput("max_beta_multiplier","Regularization mulitplier",value = 1,min=0,max=100),
                                        numericInput("max_nbg","Max number of background points",value = 10000,min=100,max=1000000),
                                        numericInput("max_repli_numer","Replicates",value=1,min=1,max=10000),
                                        selectInput("max_repli_type",label = "Replicated run type",choices = c("Crossvalidate"="crossvalidate",
                                                                                                               "Bootstrap"="bootstrap",
                                                                                                               "Subsample"="Subsample"))
                                        #p("Select a format and upload your coordinate data"),
                                        #tags$div(title="Select the format of your occs data base",
                                        #         radioButtons('max_format_test', 'Format', c('CSV'))),
                                        #fileInput('max_testing_file', 'Testing file')
                            ),
                            wellPanel(h5("Advanced"),
                                      checkboxInput("max_add_samp_to_bg","Add samples to background",value=TRUE),
                                      checkboxInput("max_add_all_samp_to_bg","Add all samples to background",value=FALSE),
                                      checkboxInput("max_extrapolate","Extrapolate",value = TRUE),
                                      numericInput("max_num_iterations","Number of iterations",value = 500,min = 100,max = 10000),
                                      numericInput("max_threshold_conv","Convergence threshold",value = 0.00001,min=0.000005,max=0.0002,step = 0.000005),
                                      numericInput("max_prevalence","Prevalence",value=0.5,min = 0.05,max=1,step=0.05)
                                      ),
                            wellPanel(h5("Experimental"),
                                      numericInput("max_lq2lqptthreshold","Lq to Lqp threshold",value = 80,min=20,max=1000),
                                      numericInput("max_l2lqthreshold","Linear to lq threshold",value=10,min=1,max=1000),
                                      numericInput("max_hingethreshold","Hinge threshold",value=15,min=1,max=1000),
                                      numericInput("max_beta_threshold","Beta threshold",value=-1),
                                      numericInput("max_beta_categorical","Beta categorical",value=-1),
                                      numericInput("max_beta_lqp","Beta lqp",value=-1),
                                      numericInput("max_beta_hinge","Beta hinge",value=-1)
                                      #numericInput("max_no_data","Default no data value",value=-9999)
                                      )
                            ),
                            mainPanel(conditionalPanel("(input.selectDataMaxEnt== 'user_data' || input.selectDataMaxEnt== 'gbif_data') && input.selectM_MaxEnt == 'all_raster'",
                                                       uiOutput("maxent_html_all_all")),
                                      conditionalPanel("(input.selectDataMaxEnt== 'user_data' || input.selectDataMaxEnt== 'gbif_data') && input.selectM_MaxEnt == 'mLayers'",
                                                       uiOutput("maxent_html_all_M"))
                                      ))
