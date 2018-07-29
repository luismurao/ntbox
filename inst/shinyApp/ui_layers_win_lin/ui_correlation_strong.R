#-----------------------------------------------------------------------------------
# Niche bivariate strong correlations output

strong_corre <- sidebarLayout(position = "left",
                              sidebarPanel(
                                titlePanel("NicheToolBox"),
                                helpText("Showing correlations acoording to a threshold given by the user.
                                 Bivariate correlations may help to detect which variables are redundant."),
                                selectInput("cor_data_from",label = "Select a data set",choices = NULL),
                                numericInput(inputId = "cor_threshold",
                                             label = "Correlation threshold",
                                             value = 0.85,  min=0.5, max=1,step=0.05),
                                busyIndicator("Computation In progress",wait = 0),
                                checkboxInput("verbose_cor","Verbose",value = TRUE),
                                downloadButton(outputId = "download_stcor","Download")),
                              mainPanel(
                                verbatimTextOutput("big_cor")
                              ))
