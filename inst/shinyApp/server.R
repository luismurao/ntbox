options(shiny.maxRequestSize=550*1024^2)
source("load_install_pkgs.R")
shinyServer(function(input,output,session){
  source("workflow_events/settings_workflow_obs.R",local = TRUE)
  })
