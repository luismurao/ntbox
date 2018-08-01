# App Settings & Materials (tutorials and intro to the app)

materials <- sidebarLayout(position = "left",
                           sidebarPanel(
                             tags$head(

                               tags$script(src = 'http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
                               includeScript("ui_layers/style/google-analytics.js")
                             ),
                             titlePanel("NicheToolBox"),
                             #h3('Welcome to NTB'),
                             br(),
                             p("This application has some tools to analyze and visualize the niche. In this page you will find th NTB settings
                               of the app."),

                             #h3('Settings'),
                             br(),
                             h4("Niche layers"),
                             p("One ne of the most important things for your data analysis in NTB is to select the folder with the raster layer that you will use as niche variables."),
                             br(),
                             p("Please select the folder of raster layers and remember that all of them need to be in the same resolution and extension"),
                             br(),
                             #directoryInput('ras_layers_directory', label = 'Select a raster layers directory'),
                             #glyphicon glyphicon-folder-open
                             shinyFiles::shinyDirButton(id = "ras_layers_directory",
                                                        label =  "Select raster layers directory",
                                                        title = "Select a directory from the directories panel",
                                                        icon = icon("folder-open",
                                                                    lib = "glyphicon"),
                                                        buttonType = "info"),
                             br(),
                             HTML(paste('<div class="alert alert-info" role="alert">',
                                  verbatimTextOutput("layers_directory"),'</div>')),
                             #shiny::selectInput(inputId = )
                             #selectInput(inputId = 'res',label = 'Select a grid resolution for the bioclimatic vars',choices = c('10 Arc-minutes'='tenArcs','5 Arc-minutes'='fiveArcs','2.5 Arc-minutes'='twoArcs'),selected = 'tenArcs'),
                             bsButton("loadNicheLayers","Load niche layers",icon = icon("upload", lib = "glyphicon"),style = "primary"),
                             busyIndicator("Loading...",wait = 0),
                             br(),
                             br(),
                             h4("Workflow"),
                             HTML('In order to save what you have done in your session it is important that you select a folder to save a workflow of what you have done inside NicheToolBox'),
                             br(),
                             br(),
                             #fileInput(inputId = "worflowPath","Select a folder for your worflow"),
                             #shinyDirButton('worflowPath', 'Folder select', 'Please select a folder'),
                             #shinyDirButton("workflowPath",title = "Choose Download Directory",
                             #              label = "Browse Download Directory ", class = "btn-block btn-link"),
                            # directoryInput('wf_directory', label = 'Select workflow directory'),
                            shinyFiles::shinyDirButton(id = "wf_directory",
                                                       label =  "Select workflow directory",
                                                       title = "Select a directory from the directories panel",
                                                       icon = icon("folder-open",
                                                                   lib = "glyphicon"),
                                                       buttonType = "info"),
                            br(),
                            HTML(paste('<div class="alert alert-info" role="alert">',
                                       verbatimTextOutput("work_directory"),'</div>'))

                               #submitButton()


                             #h4('Tutorials'),
                             #HTML('In this part you can find some tutorials that show how to use the app.'),
                             #selectInput(inputId = 'tutorialsm',label = 'Select one of the turtorials',
                             #            choices =  c('About'='about1','1. Data'='datose'),
                              #           selected = "about1",multiple = FALSE)


                               ),
                           mainPanel(
                             #htmlOutput("page"),
                             plotOutput("niche_layers")

                           )
                           )
