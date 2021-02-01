
gistools <- div(class="outer",

                  tags$head(
                    # Include our custom CSS
                    includeCSS("ui_layers/style/styles.css"),
                    includeScript("ui_layers/style/gomap.js")
                  ),

                  leafletOutput(outputId = "dyMap_gis",width="100%", height="100%"),

                  # Shiny versions prior to 0.11 should use class="modal" instead.
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                width = 420, height = "auto",

                                titlePanel("NicheToolBox"),
                                h3('GIS Tools'),
                                br(),
                                p("In this section you can perform some basic GIS operations on your Niche layers."),
                                busyIndicator("Computation In progress",wait = 0),
                                checkboxInput("up_polygon","Upload your polygon",value = FALSE),

                                conditionalPanel("input.up_polygon==true",
                                                 shinyFilesButton("polygis_user", "Choose a shp file" ,
                                                                  title = "Please select a shapefile:",
                                                                  multiple = FALSE,
                                                                  buttonType = "default",
                                                                  class = NULL)
                                                 ),

                                #HTML('
                                #     <ul style="list-style-type:square">
                                #     <li>PCA</>
                                #     <li>Crop</li>
                                #     <li>Mask</li>
                                #     <li>Map Algebra</li>
                                #     </ul>'),

                                shiny::radioButtons("gis_options",label = "Select a tool:",
                                                    choices = c("Write my niche layers in other raster format"="reformatLayers",
                                                                "Crop / Mask my niche layers"="CropMaskLayers",
                                                                #"Mask my niche layers"= "maskLayers",
                                                                "PCA tranform my niche layers"="pca_transform")),
                                conditionalPanel("input.gis_options == 'CropMaskLayers'",
                                                 shiny::radioButtons("CropMask",label = "Select an operation:",
                                                                    choices = c("Crop"="gis_crop",
                                                                                "Mask"="gis_mask"),
                                                                    selected = "gis_mask"),
                                                 selectInput("layers2CropMask","Select layers to be processed:",
                                                             choices = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                             selected = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                             multiple = FALSE),
                                                 uiOutput("go_MaskCrop"),
                                                # uiOutput("save_cuts"),
                                                actionButton(inputId = "save_gtoolspoly",label = "Save Polygon",styleclass="primary"),

                                                 verbatimTextOutput("coords_gis_polygon"),
                                                radioButtons('raster_format2', 'Select a format to write your layers:',
                                                             c('.asc','.bil',
                                                               '.tif','.nc',
                                                               '.sdat','.img'))

                                                 ),

                                conditionalPanel("input.gis_options=='reformatLayers'",
                                                 selectInput("layers2reformat","Select layers to be reformated:",
                                                             choices = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                             selected = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                             multiple = TRUE),

                                                 radioButtons('raster_format', 'Select a format to write your layers:',
                                                              c('.asc','.bil',
                                                                '.tif','.nc',
                                                                '.sdat','.img')),
                                                 uiOutput("reformatDir"),
                                                 shiny::actionButton("go_reformat",label = "Go"),
                                                 busyIndicator("Computation In progress",wait = 0),
                                                 verbatimTextOutput("printLayersInfo")

                                                 ),
                                conditionalPanel("input.gis_options=='pca_transform'",
                                                 h4("Principal Component Analysis"),
                                                 radioButtons("pca_by",label = "Select a way to do a PCA:",
                                                              choices = c("From my niche layers"="from_ntblayer",
                                                                          "From rds file created by ntbox (see spca help):"="from_rds")),
                                                 conditionalPanel("input.pca_by=='from_ntblayer'",
                                                                  selectInput("layers2pc","Select layers to be analyzed:",
                                                                              choices = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                                              selected = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                                              multiple = TRUE)

                                                 ),
                                                 conditionalPanel("input.pca_by=='from_rds'",

                                                                  fileInput('rds_file',
                                                                            'Select the rds file that has the PCA metadata (see spca help)')

                                                 ),

                                                 uiOutput("check_projLayers"),
                                                 conditionalPanel("input.run_pca_proj==true",
                                                                  selectInput("layers2proj","Select layers to be analyzed. ntbox will assume that the layers that have the same position in the stack represent the same kind of variables:",
                                                                              choices = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                                              selected = "Upload your niche layers and Set a workflow direcotry on the AppSettings section",
                                                                              multiple = TRUE),
                                                                  textInput("projLayers_wd","Enter a name for PCA projection layeres directory")
                                                 ),

                                                 radioButtons('pca_format', 'Select a format to write the pca layers:',
                                                              c('.asc','.bil',
                                                                '.tif','.nc',
                                                                '.sdat','.img')),
                                                 shiny::actionButton("dopca", "Go"),
                                                 busyIndicator("Computation In progress",wait = 0),
                                                 plotOutput(outputId = "pca_plot",width = "400px",height = "400px")
                                                 )



                                )

)
