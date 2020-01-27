# UI bivariate correlations table

#-----------------------------------------------------------------------------------
corre_table_1 <- sidebarLayout(position = "left",
                             sidebarPanel(
                               titlePanel("NicheToolBox"),
                               helpText("Correlation table"),
                               busyIndicator("Computation In progress",wait = 0),
                               downloadButton(outputId="download_cor_table",label="Download table")
                             ),
                             mainPanel(
                               DT::dataTableOutput("corr_table1")
                             ))
