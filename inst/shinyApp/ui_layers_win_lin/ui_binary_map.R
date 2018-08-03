ui_binary  <- sidebarLayout(position = "left",
                            sidebarPanel(
                              tags$head(

                                tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
                              ),
                              titlePanel("NicheToolBox"),


                              h3("Binary map conversion"),
                              wellPanel(
                                radioButtons("valdata_type",label = "Select a method to convert your map",
                                             choices = c("Minimum training presence"= "min_traing_pres",
                                                         "Confusion matrix optimization"="pres_abs_data",
                                                         "User threshold selection" = "user_threshold",
                                                         "Percentil selection" = "percentil_bin"
                                             ),selected ="pres_abs_data" )
                              ),
                              #p("In this section contains a set of functions that will
                              #  help you to find the best threshold cut-off."),
                              p("Upload the model prediction raster in ASCII format"),
                              radioButtons('formatBin', 'Format', c('.asc','.bil',
                                                                    '.tif','.nc',
                                                                    '.sdat','.img')),
                              fileInput('fileBin', 'Data file'),

                              conditionalPanel("input.valdata_type=='pres_abs_data'",
                                               h4("Real Presences and Absences"),
                                               p("Upload your validation data. The data must be in the following format:"),

                                               HTML('<table style="width:80%" border="1" align="center">
                                                    <tr>
                                                    <th> long</th>
                                                    <th> lat</th>
                                                    <th> PrecenseAbsence</th>
                                                    </tr>
                                                    <tr>
                                                    <td> -65.38</td>
                                                    <td> -10.38</td>
                                                    <td> 1</td>
                                                    </tr>
                                                    <tr>
                                                    <td> -62.11</td>
                                                    <td> -11.22</td>
                                                    <td> 0</td>
                                                    </tr>
                                                    </table>'),

                                               radioButtons('formatVal', 'Format', c('CSV')),
                                               fileInput('fileVal', 'Data file'),
                                               sliderInput("thRange","Select a threshold range to look for the best cut off",
                                                           min = 0,max = 1,step = 0.1,value = c(0,0.8)),
                                               numericInput("partStep",label = "Select a partition step for your range",
                                                            value = 0.01,min = 0.001,max = 0.02),
                                               radioButtons("optim_by1","Optimization by",choices = c("Kappa"= "kappa",
                                                                                                      "TSS" = "tss")),
                                               actionButton("searchTh","Search threshold",styleclass="primary"),

                                               h5("Download binary map"),
                                               downloadButton("downloadBinary_mc_opt",label = "Download Binary",class = "primary"),
                                               h5("Download binary metadata"),
                                               downloadButton("downloadBinary_metadata_mc_opt",label = "Download metadata",class = "primary"),
                                               shinysky::busyIndicator("Computation In progress",wait = 0)),

                              conditionalPanel("input.valdata_type=='user_threshold'",
                                               h4("User threshold cut-off"),
                                               wellPanel(
                                                 p("Please select a threshold:"),
                                                 numericInput("thresholdBin","Threshold",value = 0.08,step = 0.01),
                                                 actionButton("compBin",label = "Compute Binary",styleclass="primary"),
                                                 h5("Download binary map"),
                                                 downloadButton("downloadBinary_user",
                                                                label = "Download Binary",class = "primary"),
                                                 h5("Download binary metadata"),
                                                 downloadButton("downloadBinary_metadata_user",
                                                                label = "Download metadata",class = "primary")

                                               )),
                              conditionalPanel("input.valdata_type!='pres_abs_data'",
                                               h4("Presence data"),
                                               p("Upload your traning data. The data must be in the following format:"),
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
                                                    </table>'
                                               ),
                                               fileInput("val_data_mtp","Data file"),
                                               radioButtons('formatVal_mtp', 'Format', c('CSV')),
                                               conditionalPanel("input.valdata_type=='percentil_bin'",
                                                                numericInput("percentil_th",label = "Select a percent",value = 10,min = 1,max = 100)

                                               ),
                                               h5("Download binary map"),
                                               downloadButton("downloadBinary_mtp",
                                                              label = "Download Binary",class = "primary"),
                                               h5("Download binary metadata"),
                                               downloadButton("downloadBinary_metadata_mtp",
                                                              label = "Download metadata",class = "primary")
                                               ),
                              h3("Confusion matrix"),
                              HTML("Enter values for: a, b, c y d"),
                              numericInput(inputId="a",label="Correctly predicted\npresences (a)",value=28,min=0,max=1000000),
                              numericInput(inputId="b",label="Predicted as present but\nactually absent(b)",value=7,min=0,max=1000000),
                              numericInput(inputId="c",label="Predicted as absent but\nactually present (c)",value=11,min=0,max=1000000),
                              numericInput(inputId="d",label="Correctly predicted\nabsences (d)",value=62,min=0,max=1000000),
                              radioButtons(inputId = "elecc1",label = h4("Select a metric"),choices =
                                             list(
                                               "Show all metrics" = "all1",
                                               "Prevalence" = "prev1",
                                               "Specificity" = "espe1",
                                               "Sensitivity" = "sens1",
                                               "TSS"="tss1","Kappa"="kappa1",
                                               "Correct classification rate"= "tcc1",
                                               "Misclassification rate"= "tcin1",
                                               "Negative predictive power" = "npp1",
                                               "Positive predictive power" = "ppp1",
                                               "Omission error fraction" = "feom1",
                                               "Commission error fraction" = "fecom1",
                                               "False negative rate" = "tfn1",
                                               "False positive rate" = "tfp1"),selected = "all1"),


                              #checkboxInput("binomial_test_1",label = "Binomial test (in development)",value = FALSE),
                              conditionalPanel(condition = "input.binomial_test_1 == true",
                                               numericInput(inputId = "prop_area1",label = "Enter the proportion of the predicted area",
                                                            value = "0.5",min = 0.0,max = 1,step=0.01)),
                              downloadButton(outputId = "todoCM",label = "Download results")),
                            mainPanel(
                              conditionalPanel("input.valdata_type=='pres_abs_data'",
                                               dataTableOutput("threshold_sims"),
                                               uiOutput("area_cm_method"),
                                               plotOutput("binary_CM_optim")),
                              conditionalPanel("input.valdata_type=='min_traing_pres' || input.valdata_type=='percentil_bin'",
                                               uiOutput("area_mtp_method"),
                                               plotOutput("binary_mtp")
                              ),
                              conditionalPanel("input.valdata_type=='user_threshold'",
                                               uiOutput("area_user_method"),
                                               plotOutput("binary_user")),
                              tags$style(type='text/css', '#metric {background-color: rgba(255,255,0,0.40); color: green;}'),
                              br(),
                              h3("Confusion matrix"),
                              br(),
                              tableOutput('con_tableBin'),
                              conditionalPanel(condition = "input.elecc1=='kappa1'",
                                               h3("Kappa"),
                                               HTML("<p align = 'justify'>
                                                    $$Kappa = \\frac{((a+d)-(((a+c)*(a+b)+(b+d)*(c+d))/N))}{(N-(((a+c)*(a+b)+(b+d)*(c+d))/N))}$$</p>")),
                              conditionalPanel(condition = "input.elecc1 =='tss1'",h3("TSS"),
                                               HTML("<p align = 'justify'>
                                                    $$TSS = \\frac{a}{(a+c)} + \\frac{d}{(b+d)} - 1$$</p>")),

                              conditionalPanel(condition = "input.elecc1=='prev1'",h3("Prevalence"),

                                               HTML('<p align="center"><img src="prevalencia.png" alt="Prevalencia" width="800" height="400" align="middle"></p>'),
                                               br(),
                                               HTML("<p align = 'justify'>
                                                    $$Prevalencia = \\frac{(a + c)}{(a+b+c+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='tcc1'",h3("Correct classification rate"),
                                               HTML('<p align="center"><img src="posi_class_rate.png" alt="PCRate" width="800" height="400" align="middle"></p>'),
                                               HTML("<p align= 'justify'>
                                                    $$T.C.C = \\frac{(a+d)}{(a+b+c+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='tcin1'",h3("Misclassification rate"),

                                               HTML('<img src="miss_rate.png" alt="MissClassRate" width="800" height="400" align="middle">'),
                                               HTML("<p align = 'justify'>
                                                    $$T.C.I = \\frac{(b+c)}{(a+b+c+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='npp1'",h3("Negative predictive power"),

                                               HTML('<img src="nega_predi_pw.png" alt="NegativePredRate" width="800" height="400" align="middle">'),

                                               HTML("<p align = 'justify'>
                                                    $$N.P.P = \\frac{d}{(c+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='ppp1'",h3("Positive predictive power"),

                                               HTML('<img src="posi_pred_pw.png" alt="PosiPredRate" width="800" height="400" align="middle">'),
                                               HTML("<p align = 'justify'>
                                                    $$P.P.P = \\frac{a}{(a+b)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1 =='tfn1'",h3("False negative rate"),
                                               HTML("<p align = 'justify'>
                                                    $$T.F.N =\\frac{c}{(a+c)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='tfp1'", h3("False positive rate"),
                                               HTML("<p align = 'justify'>
                                                    $$T.F.P = \\frac{b}{(b+d)}$$ </p>")),
                              conditionalPanel(condition = "input.elecc1=='espe1'",h3("Specificity"),

                                               HTML('<img src="especifi.png" alt="Especificidad" width="800" height="400" align="middle">'),
                                               HTML("<p align = 'justify'>
                                                    $$Especificidad = \\frac{d}{(b+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='sens1'",h3("Sensibility"),

                                               HTML('<img src="sensi.png" alt="Sensibilidad" width="600" height="400" align="middle">'),
                                               HTML("<p align = 'justify'>
                                                    $$Sensibilidad = \\frac{a}{(a+c)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='feom1'",h3("Omission error"),

                                               HTML('<img src="feom.png" alt="FEOM" width="800" height="400" align="middle">'),
                                               HTML("<p align ='justify'>
                                                    $$F.E.O=\\frac{c}{(a+c)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='fecom1'",h3("Comission error"),

                                               HTML('<img src="fecom.png" alt="FECOM" width="800" height="400" align="middle">'),

                                               HTML("<p align = 'justify'>
                                                    $$F.E.C=\\frac{b}{(b+d)}$$</p>")),
                              conditionalPanel(condition = "input.elecc1=='all1'",h3("Evaluation metrics for SDMs")),
                              br(),
                              verbatimTextOutput(outputId = "cm_method_metrics"),
                              tags$style(type='text/css', '#cm_method_metrics {background-color: rgba(255,255,0,0.40); color: green;}')

                              #conditionalPanel(condition = "input.binomial_test_1 == true",
                              #                 br(),
                              #                 h3("Binomial Test"),
                              #                 tags$style(type='text/css', '#cm_method_metrics {background-color: rgba(255,255,0,0.40); color: green;}'),
                              #                 verbatimTextOutput("binomial1"))


                                               )
                                               )

