#' Download and Extract CMIP5 Climate Data Layers
#'
#' This function downloads climate data layers from the CMIP5 archive based on
#' user specifications for variable, resolution, model, RCP, and year. It then
#' unzips the files and returns the relevant climate data layers as a `terra` raster object.
#'
#' @param var A character string specifying the climate variable to download. Choices are:
#'   "bio" (bioclimatic variables), "tmin" (temperature min), "tmax" (temperature max), or "prec" (precipitation). Default is "bio".
#' @param res A character string specifying the resolution of the data. Choices are:
#'   "10" (10 minutes), "5" (5 minutes), "2.5" (2.5 minutes), or "0.5" (0.5 minutes). Default is "10".
#' @param rcp A character string specifying the Representative Concentration Pathway (RCP) scenario. Choices are:
#'   "26", "45", "60", or "85". Default is "85".
#' @param model A character string specifying the Global Climate Model (GCM) to use. Choices are:
#'   'AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO'.
#'   These codes correspond to specific models (e.g., 'AC' = 'ACCESS1-0', 'BC' = 'BCC-CSM1-1', etc.). Default is "AC".
#' @param year A character string specifying the future year to use. Choices are "50" or "70" (for years 2050 and 2070). Default is "50".
#' @param save_path A character string specifying the path where the downloaded and unzipped files should be saved. Default is the current working directory (".").
#'
#' @return A `terra` raster object containing the downloaded and extracted climate data layers for the specified parameters.
#'
#' @import terra dplyr rio purrr stringr
#' @export
#'
#' @examples
#' # Example usage:
#' \dontrun{
#' wc_layers <- wc_cmip5(var = "bio", res = "10", rcp = "85", model = "AC",
#'                       year = "50", save_path = "data/")
#' }
wc_cmip5 <- function(var="bio", res="10", rcp="85", model="AC", year="50", save_path=".") {

  # Define the choices for each parameter
  var_choices <- c("bio", "tmin", "tmax", "prec")
  res_choices <- c("10", "5", "2.5", "0.5")
  model_choices <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG',
                     'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  names(model_choices) <- c('ACCESS1-0', 'BCC-CSM1-1', 'CCSM4', 'CESM1-CAM5-1-FV2', 'CNRM-CM5', 'GFDL-CM3',
                            'GFDL-ESM2G', 'GISS-E2-R', 'HadGEM2-AO', 'HadGEM2-CC', 'HadGEM2-ES', 'INMCM4',
                            'IPSL-CM5A-LR', 'MIROC-ESM-CHEM', 'MIROC-ESM', 'MIROC5', 'MPI-ESM-LR', 'MRI-CGCM3', 'NorESM1-M')
  rcp_choices <- c("26", "45", "60", "85")
  year_choices <- c("50", "70")

  # Match the input arguments with the available choices
  varia <- match.arg(var, choices = var_choices)
  resol <- match.arg(res, choices = res_choices)
  gcm <- match.arg(model, choices = model_choices)
  rcp <- match.arg(as.character(rcp), choices = rcp_choices)
  year <- match.arg(as.character(year), choices = year_choices)
  year <- as.numeric(paste0("20", year))

  # Path to the CMIP5 data file
  cmip_path <- system.file(package = "ntbox", "extdata/wc_cmip5.rds")
  wc_future_cmip5 <- rio::import(cmip_path)

  # Filter the dataset based on the input parameters
  wc_future_cmip5_select <- wc_future_cmip5 |>
    dplyr::filter(code == !!gcm, RCP == !!rcp,
                  Variable == !!varia,
                  Year == !!year,
                  Resolution == !!resol)

  # Define the file path for saving the downloaded file
  filename <- file.path(save_path, basename(wc_future_cmip5_select$URL))

  # Download the selected file if it exists
  if (nrow(wc_future_cmip5_select) > 0L) {
    download.file(wc_future_cmip5_select$URL, destfile = filename)
  } else{
    stop("No information for input parameters")
  }

  # Unzip the downloaded file(s)
  unzipped <- seq_along(filename) |> purrr::map(function(x) {
    unzip(zipfile = filename,
          exdir = save_path,
          overwrite = TRUE,
          junkpaths = TRUE)
  }) |> unlist()

  # Extract the numerical ID from the filename
  patt <- gsub(".zip", "", basename(wc_future_cmip5_select$URL))
  numbID <- as.numeric(stringr::str_extract(gsub(patt, "", unzipped), "[0-9]+"))

  # Load and return the raster layers in order
  wc_cmip5_layers <- terra::rast(unzipped[order(numbID)])
  return(wc_cmip5_layers)
}
