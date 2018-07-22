#' Function to run NicheToolBox shinyApp
#' @description This function initiates NicheToolBox GUI
#' @export
#' @examples
#' # Run nichetoolbox package
#' # run_nichetoobox()
run_ntbox <- function(){

  to_install <- system.file("shinyApp/load_install_pkgs.R",
                            package = "ntbox")

  cat("\n\n")
  cat("|----------------------------------------------------------------|\n")
  cat("|      **** Welcome to nichetoolbox version", paste0(packageVersion("ntbox")," ****         |\n"))
  cat("|----------------------------------------------------------------|\n")
  cat("|           Looking for additional nichetoolbox dependencies     |\n")
  cat("|           Note that the first time can take few time           |\n")
  cat("|----------------------------------------------------------------|\n\n\n")

  source(to_install)

  app_path <- system.file("shinyApp",package = "ntbox")
  return(shiny::runApp(app_path))
}
