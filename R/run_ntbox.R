#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_ntbox <- function(){

  to_install <- system.file("shinyApp/load_install_pkgs.R",
                            package = "ntbox")
  source(to_install)

  app_path <- system.file("shinyApp",package = "ntbox")
  return(shiny::runApp(app_path))
}
