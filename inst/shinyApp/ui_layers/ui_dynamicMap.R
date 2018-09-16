osSystem <- Sys.info()["sysname"]

DynamicMap <- div(class="outer",

                      tags$head(
                        # Include our custom CSS
                        includeCSS("ui_layers/style/styles.css"),
                        includeScript("ui_layers/style/gomap.js")
                      ),

                      leafletOutput(outputId = "dyMap_cas",width="100%", height="100%"),

                      # Shiny versions prior to 0.11 should use class="modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                    width = 340, height = "auto",

                                    h2("NicheToolBox"),
                                    h4("Dynamic Map"),

                                    #useShinyjs(),
                                    #extendShinyjs(text = "ui_layers/get_coordsPoly/get_coords.js"),

                                    selectInput("dataset_dynMap","Select a dataset",choices = c("GBIF data"="gbif_dataset",
                                                                                                "User data"="user_dataset")),
                                    busyIndicator("Computation In progress",wait = 0),
                                    selectInput(inputId = "pointsDynamic","Select the data point (data ID) you want to
                                                clean from the data base",choices = NULL,multiple = TRUE),
                                    shiny::actionButton("cleanDynamic",label = "Clean data points"),
                                    downloadButton(outputId = "downDatDyn",label = "Download data"),


                                    h4("Polygon of M"),
                                    checkboxInput("define_M",label = "Use the polygon of M to filter data",value = FALSE),
                                    conditionalPanel("input.define_M == 1",
                                                     tags$div(title="Select if you want to trace your polygon of M or uploaded it from directory",
                                                              #checkboxInput("poly_from",
                                                              #              label = "Trace polygon using dynamic map",value = T)
                                                              shiny::radioButtons(inputId ="poly_from","",choices = c("Trace polygon using dynamic map"=1,"Polygon from file"=0))
                                                     ),
                                                     conditionalPanel("input.poly_from == 1",
                                                                      p("You can use dynamic map to deffine the polygon of your study area. Trace a polygon using the tools of the dynamic map and then give it a name sucha as 'polygon1'"),
                                                                      textInput("polygon_name","Give a name for your polygon",value = NULL),
                                                                      verbatimTextOutput("polyfeatures")),
                                                                      #shiny::actionButton("save_poly",label = "Save Polygon")),
                                                     conditionalPanel("input.poly_from == 0",

                                                                      conditionalPanel(paste0("'",osSystem,"' != 'Darwin'"),
                                                                                       directoryInput('poly_dir',
                                                                                                      label = 'Select a directory of your shapefile of M',
                                                                                                     value = NULL)
                                                                      ),
                                                                      conditionalPanel(paste0("'",osSystem,"' == 'Darwin'"),
                                                                                       shinyFiles::shinyDirButton(id = "poly_dir",
                                                                                                                  label =  "Select a directory of your shapefile of M",
                                                                                                                  title = "Select a directory from the directories panel",
                                                                                                                  icon = icon("folder-open",
                                                                                                                              lib = "glyphicon"),
                                                                                                                  buttonType = "info")
                                                                                       ),

                                                                      br(),

                                                                      selectInput("poly_files","Select layer of M",
                                                                                  choices = NULL,multiple = FALSE,
                                                                                  selected = "Select directory")

                                                     ),
                                                     shiny::actionButton("points_in_poly",label = "Points in polygon")#,
                                                     #checkboxInput("hold_polygon",label = "Hold polygon",value = FALSE)
                                                     ),


                                    br(),
                                    h4("History of reccords"),
                                    selectInput("yearVarDynamic",label = "Time variable",choices = NULL,multiple = FALSE),

                                    #actionButton("plotTS","Plot History of records"),
                                    checkboxInput("plotTSDynamic","Plot History of records",value = FALSE),
                                    br(),
                                    conditionalPanel("input.plotTSDynamic==1",dygraphOutput(outputId = "RecordsHistDynamic",width = "300px",height = "300px"))

                                    )

)
