#-----------------------------------------------------------------------------------
# Niche groups: Groups may reflect local adaptations.
layer_names_kmeans <- c("Extract niche values "=1)
dataset_cluster <- c("Extract niche values "=1)
ui_kmeans <- sidebarLayout(position = "left",
                              sidebarPanel(
                                titlePanel("Niche ToolBox"),
                                helpText("In development: The main objective of this function is to see if there are
                                         groups in the niche. The groups could be reflecting local adaptations..."),
                                selectInput("kmeans_data_from",label = "Select a data set",choices = NULL),
                                selectInput("cluster_vars","Select at least 3 niche variables",choices = layer_names_kmeans,multiple = TRUE),
                                numericInput("Suggest a number of clusters",inputId = "nclust",value = 3,min = 2,max=100),
                                numericInput("kmeans_level", "Select a conffidence level",value = 0.95,min = 0.5,max=0.99),
                                busyIndicator("Computation In progress",wait = 0),
                                actionButton("load_kmeas_vars","Go!!!",styleclass = "primary"),
                                selectInput(inputId = "x1",label = "Select a variable for the x-axis",
                                            choices = layer_names_kmeans,selected = layer_names_kmeans),
                                selectInput(inputId = "y1",label = "Select a variable for the y-axis",
                                            choices = layer_names_kmeans,selected = layer_names_kmeans),
                                selectInput(inputId = "z1",label = "Select a variable for the z-axis",
                                            choices = layer_names_kmeans,selected = layer_names_kmeans),
                                #selectInput(inputId = "algoritmo",label = "Select one algorithm",choices=c("Hartigan-Wong", "Lloyd", "Forgy",
                                #                                                                           "MacQueen")),
                                checkboxInput("ellips",label = "Show ellipsoids",value = TRUE),
                                #selectInput(inputId ="grupos" ,"Gruop by",choices = c("K-means"="kmeans","An ID in the Database"="bd")),
                                checkboxInput(inputId ="grupos" ,"Group by an ID in the data base:",value = FALSE),
                                conditionalPanel(condition = "input.grupos==true",
                                                 selectInput("vgrupo",label = "Select the variable",choices = names(dataset_cluster)[1])),
                                numericInput(inputId = "cex1","Font size",min = 0.05,max = 1,step = 0.01,value = 0.35),
                                sliderInput("alpha",label = "Ellipsoid transparency",min = 0,max=1,value = 0.1,step = 0.05),
                                downloadButton("downloadKmeans",label = "Download K-means")
                                #downloadButton("down3dClust",label = "Download plot")),
                                ),
                              mainPanel(
                                leafletOutput(outputId = "kmeans_geo_leaflet"),
                                rglwidgetOutput("kmeans_clust_3d",width = "800px",height = "800px")
                              ))
