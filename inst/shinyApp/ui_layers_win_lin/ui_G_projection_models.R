# Leaflet maps for nichetoolbox models

ntb_models <-  div(class="outer",

                   tags$head(
                     # Include our custom CSS
                     includeCSS("ui_layers/style/styles.css"),
                     includeScript("ui_layers/style/gomap.js")
                   ),

                   #plotOutput("EllipRaster",width = "1000px",height = "600px"),
                   leafletOutput(outputId = "ras_models",width="100%", height="100%"),
                   # Shiny versions prior to 0.11 should use class="modal" instead.
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                 draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                 width = 340, height = "auto",

                                 h2("NicheToolBox"),
                                 h4("ENModels projections\n in G"),
                                 busyIndicator("Computation In progress",wait = 0),

                                 selectInput("proj_model1",label = "Select model",choices = NULL)
                                 #uiOutput("showMapGo")

                   )
)
