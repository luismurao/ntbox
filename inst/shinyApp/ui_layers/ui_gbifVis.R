gibif_vis <- sidebarLayout(position = "left",
                           sidebarPanel(
                             tags$head(

                               tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML',
                                           type = 'text/javascript')
                             ),
                             titlePanel("NicheToolBox"),

                             wellPanel(
                               h3("GBIF visualizations"),
                               h5("GBIF data Calendar"),
                               selectInput(inputId = "GBIFYears","Select a year",choices = NULL,selected = ""),
                               actionButton("showGBIF","Show me calendar!",styleclass = "primary"),
                               h5("Create an animated map"),
                               uiOutput("animation_gif")

                             )),

                           mainPanel(htmlOutput("gbifMotion"),htmlOutput("gbifVis"),htmlOutput("calendarG"))
)
