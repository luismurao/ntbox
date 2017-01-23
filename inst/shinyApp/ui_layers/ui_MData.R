# M data
define_M <- sidebarLayout(position = "left",
                          sidebarPanel(
                            tags$head(

                              tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
                            ),
                            titlePanel("NicheToolBox"),
                            p("This application has some nice tools to analyze, visualize the niche"),
                            #code('library(shiny)'),
                            p("Select a format and upload your coordinate data"),

                            wellPanel(

                              selectInput("datasetM","Select the data set you want to work with",
                                          choices = c("GBIF data"="gbif_dat",
                                                      "User data"="updata",
                                                      #"M data from dynamic map" = "M_data",
                                                      "Just clip my layers"="clipLayers"), selected ="gbif_dat"),
                              selectInput("extracted_area", "Select an area to make the extract",
                                          choices = c("All raster area"= "all_area", "Use the M polygon"="polygon_of_M")),
                              actionButton("run_extract","Go!!!",styleclass = "primary"),
                              br(),
                              # fileInput('shapefolder', 'Upload the a zip with your shapefile'),
                              # checkboxInput(inputId = "showMap", "Show map of M",value = T),
                              # checkboxInput(inputId = "showTab", "Show data table",value = T),
                              downloadButton(outputId = "downloadExtraction",label = "Download data"),
                              busyIndicator("Loading..",wait = 0),
                              conditionalPanel("input.extracted_area == 'polygon_of_M'", downloadButton(outputId = "downClippedR",label = "Clipped Raster"))

                            )),
                          mainPanel(
                            #conditionalPanel(condition = "input.showMap==true",h3("Map of M and data points"),plotOutput("poligonoM")),
                            dataTableOutput("dataM")
                          ))
