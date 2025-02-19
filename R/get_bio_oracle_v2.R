#' Get BioOracle v2 Data Layers
#'
#' This function retrieves environmental and climatic data layers from BioOracle v2 based on different combinations of parameters
#' such as the time period, RCP (emission scenarios), depth, variable, and statistics. Data can be downloaded in `tif` or `asc` format
#' and saved in the specified directory.
#'
#' @param period A character string specifying the time period for which to retrieve the data. Options are "Present", "2050", or "2100". The default is "Present".
#' @param rcp A character string or vector of characters specifying the greenhouse gas emission scenarios to consider.
#'            Can be one or more of: "RCP26", "RCP45", "RCP60", "RCP85". The default is `NULL`. It will only be considered for future periods (2050 and 2100).
#' @param depth A character string specifying the depth level for the data. Options are: "Benthic.Min", "Benthic.Max", "Benthic.Mean", "Surface". The default is "Benthic.Min".
#' @param variable A character string or vector of characters specifying the variable to retrieve data for. Options include:
#'                 "Temperature", "Salinity", "Primary.productivity", "pH", "Iron", "Ice.thickness", "Ice.cover", "Current.Velocity",
#'                 "Nitrate", "Phosphate", "Silicate", "Dissolved.oxygen", "Chlorophyll", "Phytoplankton", "Calcite", "Par",
#'                 "Diffuse.attenuation", "Cloud.cover", "Sea". The default is "Temperature".
#' @param stat A character string or vector of characters specifying the statistics to calculate for the selected variable. Options are:
#'             'Min', 'Max', 'Mean', 'Lt.max', 'Lt.min', 'Range'. The default is "all", which selects all statistics.
#' @param format A character string specifying the file format for saving the data. Options are "tif" or "asc". The default is "tif".
#' @param save_path A character string specifying the directory where the downloaded and processed files will be saved. The default is the current directory ("./").
#'
#' @return A `SpatRaster` object from the `terra` package containing the processed and downloaded data layers.
#'         If no data is found for the selected parameters, the function will stop and display an error message.
#'
#' @details This function filters the BioOracle v2 data based on the selected parameters and downloads the corresponding layers.
#'          If the specified `save_path` does not exist, the function will create the necessary folders to store the files.
#'          The downloaded files are unzipped, renamed according to the query parameters, and loaded into a `SpatRaster` object for further analysis.
#'
#'
#' @examples
#' \dontrun{
#' # Download data for the present period and the variable 'Temperature'
#' bioracle_layers <- get_bio_oracle_v2(period = "Present",
#'                                      depth = "Surface",
#'                                      variable = "Temperature",
#'                                      save_path = ".")
#'
#' # Download data for the 2050 period, RCP85, and the variable 'Chlorophyll'
#' bioracle_layers <- get_bio_oracle_v2(period = "2050",
#'                                      depth = "Surface",
#'                                      rcp = "RCP85",
#'                                      variable = "Temperature",
#'                                      format = "tif",
#'                                      save_path = ".")
#' }
#'
#' @export
get_bio_oracle_v2 <- function(period = "Present",
                              rcp = NULL,
                              depth = "Benthic.Min",
                              variable = "Temperature",
                              stat = "all",
                              format = "tif",
                              save_path = ".") {

  # Defining valid choices for parameters
  period_choices <- c("Present", "2050", "2100")
  rcp_choices <- c("RCP26", "RCP45", "RCP60", "RCP85")
  depth_choices <- c("Benthic.Min", "Benthic.Max", "Benthic.Mean", "Surface")
  variable_choices <- c("Temperature", "Salinity", "Primary.productivity",
                        "pH", "Iron", "Ice.thickness", "Ice.cover",
                        "Current.Velocity", "Nitrate", "Phosphate", "Silicate",
                        "Dissolved.oxygen", "Chlorophyll",
                        "Phytoplankton", "Calcite", "Par",
                        "Diffuse.attenuation", "Cloud.cover", "Sea")
  stats_choices <- c('Min', 'Max', 'Mean', 'Lt.max', 'Lt.min', 'Range')
  format_choices <- c("tif", "asc")

  # Filtering input arguments to match valid choices
  period <- match.arg(period, period_choices, several.ok = FALSE)
  if (!is.null(rcp) && period != "Present") {
    rcp <- match.arg(rcp, rcp_choices, several.ok = TRUE)
  }
  depth <- match.arg(depth, depth_choices)
  if ("all" %in% variable) {
    variable <- variable_choices
  } else {
    variable <- match.arg(variable, variable_choices, several.ok = TRUE)
  }
  if ("all" %in% stat) {
    stats <- c('Min', 'Max', 'Mean', 'Lt.max', 'Lt.min', 'Range')
  } else {
    stats <- match.arg(stat, stats_choices, several.ok = TRUE)
  }
  bioor_path <- system.file(package = "ntbox","extdata/bio_oracle_v2.rds")
  bio_or_vars <- rio::import(bioor_path)
  # Filtering BioOracle data based on selected parameters
  bio_or_vars_selec <- bio_or_vars |> dplyr::filter(Period == !!period,
                                                    Depth == !!depth,
                                                    Format == !!format,
                                                    Variable %in% !!variable)

  # Additional filtering if a specific period and RCP are selected
  if (period %in% c("2050", "2100") & rcp %in%
      c("RCP26", "RCP45", "RCP60", "RCP85")) {
    bio_or_vars_selec <- bio_or_vars_selec |> dplyr::filter(RCP %in% !!rcp)
  }

  bio_or_vars_selec <- bio_or_vars_selec |> dplyr::filter(Stat %in% !!stats)

  # Check if there is any data for the selected parameters
  if (nrow(bio_or_vars_selec) == 0L)
    stop("No information for selected parameters")

  # Create directories to save the files
  save_path0 <- file.path(save_path, "bio_oracle_v2")
  if (!dir.exists(save_path0)) dir.create(save_path0)
  period_path <- file.path(save_path0, bio_or_vars_selec$Period)
  dirs_paths <- sapply(period_path, function(x)
    if (!dir.exists(x)) dir.create(x))

  # Construct file names for the data layers
  layer_names <- file.path(period_path,
                           paste0(bio_or_vars_selec$Period,
                                  ifelse(is.na(bio_or_vars_selec$GCM), "",
                                         paste0("_", bio_or_vars_selec$GCM)),
                                  "_", bio_or_vars_selec$Depth,
                                  "_", bio_or_vars_selec$Variable, "_",
                                  bio_or_vars_selec$Stat, ".",
                                  bio_or_vars_selec$Format))

  cat("Downloading n =", length(bio_or_vars_selec$Layer_url), "variables\n")

  # Destination file paths for downloaded layers
  destfile_layers <- file.path(names(dirs_paths),
                               basename(bio_or_vars_selec$Layer_url))

  # Downloading layers
  downlayers <- seq_along(bio_or_vars_selec$Layer_url) |>
    furrr::future_map(function(x) {
      download.file(bio_or_vars_selec$Layer_url[x],
                    destfile = destfile_layers[x])
    })

  # Unzipping the downloaded files
  unzipped <- seq_along(layer_names) |> purrr::map(function(x) {
    unzip(zipfile = destfile_layers[x],
          exdir = period_path[x],
          overwrite = TRUE,
          junkpaths = TRUE)
  }) |> unlist()

  # Remove hidden files that start with "."
  ignID <- grep("^[.]", basename(unzipped))
  if (length(ignID) > 0L) unzipped <- unzipped[-ignID]

  # Rename files and remove original zip files
  file.rename(unzipped, layer_names)
  unlink(destfile_layers)

  # Load the data layers into a SpatRaster object
  bioracle_layers <- terra::rast(layer_names)

  # Return the downloaded and processed data layers
  return(bioracle_layers)
}
