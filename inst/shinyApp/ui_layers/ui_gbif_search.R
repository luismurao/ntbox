datasetGBIF <- list('Search for a species'=c(1))
# GUI for searching GBIF data
mjax <-'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
gbif_data <- sidebarLayout(position = "left",
                           sidebarPanel(
                             tags$head(
                               tags$script(
                                 src = mjax,
                                 type = 'text/javascript')
                             ),
                             titlePanel("NicheToolBox"),
                             h3("GBIF data"),
                             HTML(
                               "<p align ='justify'>Here you can request data to the</p>"
                               ),
                             a(href="http://www.gbif.org/",
                               target='_blank',"GBIF"),
                             HTML("API using the"),
                             a(href="http://cran.r-project.org/web/packages/spocc/vignettes/spocc_vignette.html",
                               target='_blank',"spocc"),
                             HTML("package</p>"),
                             br(),
                             textInput(inputId= "genus",
                                       label="Enter the species genus:",
                                       value="",
                                       placeholder = HTML("i.e. Anolis")),
                             textInput(inputId= "species",
                                       label="Enter the name of species:",
                                       value="",
                                       placeholder = "i.e. trinitatis"),
                             numericInput("occlim","Enter a occ search limit",
                                          value = 500,min = 1,max = 10000000),
                             br(),
                             actionButton(input="search_gbif_data",
                                          label="Search GBIF",styleclass="primary"),
                             busyIndicator("Loading...",wait = 0),
                             br(),
                             br(),
                             h3("Cleaning duplicates"),
                             helpText("In this section you can clean your
                                      longitude and latitude data using a
                                      threshold distance between points to
                                      be considered as duplicates."),
                             tags$div(title="Coordinate data must be in
                                      decimal degrees",
                                      selectInput('xLongitudeGBIF',
                                                  'Select longitude',
                                                  names(datasetGBIF)[1])
                             ),
                             tags$div(title="Coordinate data must be in decimal degrees",
                                      selectInput('yLatitudeGBIF',
                                                  'Selct latitude', names(datasetGBIF)[1])
                             ),
                             tags$div(tile="Here you can see the dimensions of your data set",
                                      p('The data set has'),
                                      verbatimTextOutput('nRcordsGBIF')
                             ),
                             br(),
                             tags$div(title="distance between points to be considered duplicates (in grades)",
                                      numericInput(inputId = "threshold_gbif",
                                                   label = "Threshold distance for duplicates (\\(\\delta\\))",
                                                   value = 0.0,min = 0,
                                                   max = 100000,step = 0.05)
                             ),

                             tags$div(title="Cleans the duplicates on your dataset according to the threshold distance",
                                      actionButton(input="clean_dup_gbif",
                                                   label="Clean duplicates",
                                                   styleclass="primary")
                             ),
                             br(),

                             # Clean duplicates by group

                             h4("Clean duplicates by group"),
                             helpText("Sometimes you may want to clean your data on a specific location (country, continent), this option will clean you data given a grouping variable)"),
                             selectInput(inputId = 'groupGBIF',label = 'Select the grouping variable',choices = names(datasetGBIF)[1]),
                             selectInput(inputId = 'groupLevelsGBIF',label = 'Select the ids you want to clean',choices = NULL,multiple = TRUE),
                             br(),
                             actionButton(inputId = 'clean_dup_gbif_group',label = 'Clean duplicates by group',styleclass="primary"),
                             br(),
                             h4("Download your clean data"),
                             downloadButton(outputId="downGBIF",label="Download Data")),
                             mainPanel(DT::dataTableOutput("gbif_table")))
