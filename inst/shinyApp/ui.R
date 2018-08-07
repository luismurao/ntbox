
osSystem <- Sys.info()["sysname"]
ui_files <- list.files(path = "ui_layers",pattern = "*.R$",full.names = TRUE)
load_ui <- sapply(ui_files,source)
shinyUI(
  navbarPage(theme = shinytheme("cerulean"),
             HTML('<div title="Saves state to workflow directory">
                        <strong>NicheToolBox</strong>
                            <button id="saveState" type="button" class="btn action-button btn-link">
                                        <i class="glyphicon glyphicon-hdd"></i>
                                           Save state</button></div>'),

             id = "nb",
             tabPanel("AppSettings",materials),
             navbarMenu("Data",
                        tabPanel("GBIF data",
                                 tabsetPanel(
                                   tabPanel(gbif_data,title = "Search occs"),
                                   tabPanel(gibif_vis,title = "GBIF visualizations")
                                 )),
                        tabPanel("User data",user_data),
                        tabPanel("Dynamic Map",DynamicMap)


                        ),
             navbarMenu("Niche space",
                        tabPanel("Niche data extraction",define_M),
                        tabPanel("Known niche",niche_view)
                        ),

             navbarMenu("Niche correlations",
                        tabPanel("Strong correlations",strong_corre),
                        tabPanel("Table",corre_table_1),
                        tabPanel("Plot",corre_plot_1)
                        ),
             navbarMenu("Niche clustering",
                        tabPanel("K-means",ui_kmeans)
             ),
             navbarMenu("ENM",
                        tabPanel("Ellipsoids",ellipsoid_models_c ),
                        tabPanel("Bioclim",bioclim_ui),
                        tabPanel("Model projection",ntb_models)
             ),
             navbarMenu("SDM performance",
                        tabPanel("Partial ROC",partial_roc),
                        tabPanel("Binary maps",ui_binary)),
             navbarMenu("Model transferability",
                        tabPanel("MOP",ui_mop)
                        )
  )
)
