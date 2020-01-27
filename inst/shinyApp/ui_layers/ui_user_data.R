# Help var (is going to chage when the user uploads his data)
dataset <- list('Upload your data'= c(1))
user_data <- sidebarLayout(position = "left",
                           sidebarPanel(
                             tags$head(

                               tags$script(src = 'http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
                             ),
                             titlePanel("NicheToolBox"),
                             p("This application has some tools to analyze and visualize the niche"),
                             #code('library(shiny)'),
                             h3("User data"),
                             p("Select a format and upload your coordinate data"),
                             #helpText('Hit: It would be helpful (but not necessary) if you have your coordinates data in columns called long and lat'),
                             tags$div(title="Select the format of your occs data base",
                                      radioButtons('format', 'Format', c('CSV', 'TSV'))),
                             #uiOutput("radio"),
                             fileInput('fileUser', 'Data file'),
                             busyIndicator("Loading...",wait = 0),

                             h3("Data cleaning"),

                             tags$div(title="Coordinate data must be in decimal degrees",
                                      selectInput('xLongitudeUser', 'Select longitude', names(dataset)[1])
                             ),
                             tags$div(title="Coordinate data must be in decimal degrees",
                                      selectInput('yLatitudeUser', 'Selct latitude', names(dataset)[1])
                             ),
                             tags$div(tile="Here you can see the dimensions of your data set",
                                      p('The data set has'),
                                      verbatimTextOutput('nRcordsUser')
                             ),

                             br(),
                             tags$div(title="distance between points to be considered duplicates (in grades)",
                                      numericInput(inputId = "threshold_user",
                                                   label = "Threshold distance for duplicates (\\(\\delta\\))",
                                                   value = 0.0,min = 0,
                                                   max = 100000,step = 0.05)
                             ),
                             br(),
                             tags$div(title="Cleans the duplicates on your dataset according to the threshold distance",
                                      actionButton(input="clean_dup_user",label="Clean duplicates",styleclass="primary")
                                      #busyIndicator("Cleaning duplicates",wait = 0)
                             ),
                             br(),
                             h4("Clean duplicates by group"),
                             helpText("Sometimes you may want to clean your data on a specific location (country, continent), this option will clean you data given a grouping variable)"),
                             selectInput(inputId = 'groupUser',label = 'Select the grouping variable',choices = names(dataset)[1]),

                             selectInput(inputId ='groupLevelsUser',label = 'Select the ids you want to clean',choices = NULL,multiple = TRUE),
                             #tags$div(title="If activated the data cleaned by the level variable will be shown along with the rest of the data. Otherwise (if not activated) only the data cleaned by the level variable will be shown ",
                             #        checkboxInput("showAll","Show all the data",value = TRUE)
                             #),
                             actionButton(inputId = 'clean_dup_user_group',label = 'Clean duplicates by group',styleclass="primary"),

                             br(), br(),
                             p("Download your clean data"),
                             downloadButton(outputId="dataUClean",label="Download Data")


                           ),
                           mainPanel(
                             DT::dataTableOutput("user_table")
                           ))
