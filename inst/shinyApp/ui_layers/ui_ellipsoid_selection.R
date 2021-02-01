ui_eselect <- sidebarLayout(
  position = "left",
  sidebarPanel(
    titlePanel("NicheToolBox"),
    h3("Ellipsoid Model"),
    busyIndicator("Computation In progress",
                  wait = 0),

    selectInput("selectShapeS",
                "Select a region to train the model",
                choices =NULL),
    conditionalPanel("input.selectShapeS == 'wWorld'",
                     selectInput(inputId = "biosEllipS",
                                 "Select variables",
                                 choices = NULL,
                                 multiple = TRUE),
                     ),
    conditionalPanel("input.selectShapeS == 'mLayers'",
                     selectInput(inputId = "biosEllipS",
                                 "Select the variables",
                                 choices = NULL,
                                 multiple = TRUE)
    ),

    uiOutput("esrand"),
    uiOutput("filesel"),
    checkboxInput("se_mve","Minimum volume ellipsoid",value = T),
    conditionalPanel(
      "input.se_mve == true",
      numericInput("prop_pointsS",
                   "Proportion of niche points inside the ellipsoid",
                   value = 0.95,
                   min = 0.5,max=0.99)
    ),

    selectInput("nvars",
                "Select the number of variables to calibrate models",
                NULL,multiple=T),
    numericInput("bg_number",
                 "Select number of background points to compute pROC",
                 value = 10000),
    p("Generate environmental backgroud points"),
    conditionalPanel("input.selectShapeS == 'mLayers'",
                     actionButton("run_bgM","Run")
                     ),
    conditionalPanel("input.selectShapeS == 'wWorld'",
                     actionButton("run_bg","Run")
    ),
    numericInput("omr","Omission rate criteria",
                 value = 0.05,max = 1,min = 0.01),
    checkboxInput("eparallel","Run computations in parallel",value = T),

    numericInput("ecomp_each","Number of models to run in each job in the parallel computation",
                 value = 100,max = 10000,min=10),
    checkboxInput("eproc",label = "Partial ROC",value = TRUE),

    numericInput("prociter","Total number of iterations for the partial ROC bootstrap",
                 value = 100,max = 500,min=10),

    checkboxInput("rseed","Use a random seed",value = T),
    p("Run analysis"),
    conditionalPanel("input.selectShapeS == 'mLayers'",
                     actionButton("run_selectionM","Run")
    ),
    conditionalPanel("input.selectShapeS == 'wWorld'",
                     actionButton("run_selectionW","Run")
    ),
    br(),
    downloadButton("downEselection")

  ),
  mainPanel(
    DT::dataTableOutput("eselecTable"),
    DT::dataTableOutput("env_bgT")

  )

  )
