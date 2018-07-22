options(shiny.maxRequestSize=1550*1024^2)
shinyServer(function(input,output,session){
  source("workflow_events/settings_workflow_obs.R",local = TRUE)
  })
