#---------------------------------------------------------------------------------
# Binomial test

binomial <- sidebarLayout(position = 'left',
                             sidebarPanel(
                               titlePanel("NicheToolBox"),
                               br(),
                               wellPanel(
                                 h2("Binomial test"),
                                 p("Upload model prediction"),


                                 radioButtons('bin_format', 'Format', c('.asc','.bil',
                                                                     '.tif','.nc',
                                                                     '.sdat','.img')),

                                 fileInput('sdm_map', 'Data file'),

                                 radioButtons("model_type","Select an option",
                                              c("Continous map"="bin_conti","Binary map"="bin_binary"),
                                              selected = "bin_binary"),

                                 conditionalPanel("input.model_type == 'bin_conti'",
                                                  numericInput("bin_threshold",
                                                               label = "Select a threshold",
                                                               value = 0.1)
                                                  ),

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
                                 radioButtons('testdata_format', 'Format', c('CSV')),
                                 fileInput('occ_binomial', 'Data file'),
                                 busyIndicator("Computation In progress",wait = 0),
                                 actionButton("run_binomial","Run")
                                 ),
                               wellPanel(
                                 downloadButton(outputId="binomial_down",
                                                label="Test results"),
                                 downloadButton(outputId="binomial_val_down",
                                                label="Coordinates and values")
                               )
                               ),
                             mainPanel(
                               plotOutput("bintest_map"),
                               h3("Binomial test results"),
                               dataTableOutput('binomal_results'),
                               h3("Coordinates and prediction values"),
                               dataTableOutput('binomal_values'))
                             )
