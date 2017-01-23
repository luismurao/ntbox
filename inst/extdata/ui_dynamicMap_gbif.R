# UI layer to display leaflet maps
nombres <- c(paste0(rep("bio0",9),1:9),paste0("bio",10:19))
source("load_install_pkgs.R")


DynamicMap_gbif <- div(class="outer",

                  tags$head(
                    # Include our custom CSS
                    includeCSS("ui_layers/style/styles.css"),
                    includeScript("ui_layers/style/gomap.js")
                  ),

                  leafletOutput(outputId = "dyMap_gbif",width="100%", height="100%"),

                  # Shiny versions prior to 0.11 should use class="modal" instead.
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                width = 340, height = "auto",

                                h2("NicheToolBox"),
                                h4("Dynamic Map"),

                                #checkboxInput("showHeatM",label = "Show Heat Map",value = TRUE),
                                #p("You can use dynamic map to deffine the polygon of your study area. Trace you polygon and then copy the text of the pop up window"),

                                selectInput(inputId = "pointsDynamic_gbif","Select the data point (data ID) you want to
                                            clean from the data base",choices = NULL,multiple = TRUE),
                                shiny::actionButton("cleanDynamic_gbif",label = "Clean data points"),
                                busyIndicator("Computation In progress",wait = 0),
                                downloadButton(outputId = "downDatDynamic_gbif",label = "Download data"),
                                br(),
                                #checkboxInput("showBios1","Show bioclim layer",value = FALSE),
                                #conditionalPanel("input.showBios1 == true",
                                #                 selectInput("Bioclim1", "Choose a Bioclim layer",
                                #                             choices = nombres,selected = nombres[1])),
                                h4("Define a polygon of M"),
                                tags$div(title="Select if you want to trace your polygon of M or uploaded it from directory",
                                         checkboxInput("gbif_poly_from",
                                                       label = "Trace polygon using dynamic map",value = T)
                                         ),


                                conditionalPanel("input.gbif_poly_from == 1",
                                                 p("You can use dynamic map to deffine the polygon of your study area. Trace a polygon using the tools of the dynamic map and then give it a name sucha as 'polygon1'"),
                                                 textInput("polygon_name_gbif","Give a name for your polygon",value = NULL),
                                                 shiny::actionButton("save_poly_gbif",label = "Save Ploygon")),
                                conditionalPanel("input.gbif_poly_from == 0",
                                                 directoryInput('gbif_poly_dir', label = 'Select a directory of your shapefile of M',value = NULL),
                                                 selectInput("gbif_poly_files","Select dns of your shapefile",choices = NULL,multiple = FALSE)
                                                 #shiny::actionButton("load_user_poly_gbif",label = "Load shapefile")
                                                 #fileInput(inputId = "gbif_poly_files","Select your shapefile and associated files",multiple = T)
                                ),

                                shiny::actionButton("points_in_poly_gbif",label = "Points in polygon"),
                                br(),
                                h4("History of reccords"),
                                selectInput("yearVarDynamic_gbif",label = "Time variable",choices = NULL,multiple = FALSE),

                                #actionButton("plotTS","Plot History of records"),
                                checkboxInput("plotTSDynamic_gbif","Plot History of records",value = FALSE),
                                br(),
                                conditionalPanel("input.plotTSDynamic_gbif==1",dygraphOutput(outputId = "RecordsHistDynamic_gbif",width = "300px",height = "300px"))

                                )

)
