corre_plot_1 <- sidebarLayout(position = "left",
                            sidebarPanel(
                              titlePanel("NicheToolBox"),
                              helpText("You can visualize the bivariate correlations of the data.
                                 Bivariate correlations may help to detect which variables are redundant."),
                              helpText("In this kind of plot, darker colors represent stronger correlations.
                                       The oval shapes of the graphs, reflect data dispersion"),
                              busyIndicator("Computation In progress",wait = 0),
                              downloadButton(outputId="download_cor_plot",label="Download plot")
                            ),
                            mainPanel(
                              plotOutput("corr_plot1", width = 800,
                                         height = 800)
                            ))
