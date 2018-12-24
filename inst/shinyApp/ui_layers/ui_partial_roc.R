#---------------------------------------------------------------------------------
# PARTIAL ROC

partial_roc <- sidebarLayout(position = 'left',
                             sidebarPanel(
                               titlePanel("NicheToolBox"),
                               br(),
                               wellPanel(
                                 h5("Model prediction data"),
                                 p("Upload model prediction"),

                                 radioButtons('format2', 'Format', c('.asc','.bil',
                                                                       '.tif','.nc',
                                                                       '.sdat','.img')),
                                 uiOutput("radio2"),
                                 fileInput('sdm_mod', 'Data file'),
                                 h5("Presence data"),
                                 p("Upload validation data. The data must be in the following format:"),
                                 br(),
                                 HTML('<table style="width:100%">
                                      <tr>
                                      <th>Species</th>
                                      <th>long</th>
                                      <th>lat</th>
                                      </tr>
                                      <tr>
                                      <td>sp_name</td>
                                      <td>-65.38</td>
                                      <td>-10.38</td>
                                      </tr>
                                      </table>'),
                                 br(),
                                 radioButtons('format3', 'Format', c('CSV')),
                                 uiOutput("radio3"),
                                 fileInput('occ_proc', 'Data file'),
                                 busyIndicator("Computation In progress",wait = 0)
                                 ),
                               wellPanel(
                                 h4("Partial ROC parameters"),
                                 numericInput('omission','Proportion of error allowed  (\\(\\epsilon\\)):',
                                              value = 0.05,min = 0,max=1,step = 0.05),
                                 numericInput('randper','Random points percentage%',
                                              value = 50,min = 1,max = 100,step = 1),
                                 checkboxInput("parallel_roc",label = "Parallel computation (optimized for big rasters)",value = FALSE),
                                 numericInput("ncores_proc","Numer of cores to be used:",value = 4,min=2,max=100000000),
                                 selectInput('iter','Number of iterations for the bootstrap',selected = '500',
                                             choices =c('100'=100,'500'=500,'1000'=1000,'2000'=2000,
                                                        '5000'=5000,"10000"=10000))
                               ),
                               downloadButton(outputId="partRocT",label="Download table")
                               ),
                             mainPanel(dataTableOutput('rocPart'),h4('Distribution of AUC ratios'),plotOutput('rocPartPlot'),h4('Descriptive statistics for AUC ratio'),verbatimTextOutput('pStats'))
                             )
