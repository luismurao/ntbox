#' get_envirem_clim: Get bioclimatic data from envirem
#' @description Get bioclimatic layers from ENVIREM for present and past scenarios
#' @param period Period. Possible values are: "current" ( 1960 - 1990),"holo" (Mid-Holocene ~ 6000 years ago),"lgm" (Last Glacial Maximum ~ 22000 years ago).
#' @param gcm Global circulation model. Posible values: NULL, "ccsm4", "miroc_esm", and, "mpi_esm_p".
#' @param region Geographic region. Posibles values are: "Africa", "Australia", "Eurasia", "Europe", "global", "NAmerica", "NewWorld", "Pacific", and, "SAmerica".
#' @param resolution Resolution of layers. Posible values are "10arcmin","5arcmin","2.5arcmin", and, "30arcsec".
#' @param fmt File format. Possible values are "bil" and "geotiff".
#' @param sv_dir Path to the directory where the layers will be saved. The default is the working directory of the R session.
#' @param load2r Logical. Load layers into R?
#' @seealso \code{\link[ntbox]{get_chelsa}}, \code{\link[ntbox]{get_envirem_elev}},\code{\link[ntbox]{get_bio_oracle}}
#' @details For more details visit \url{https://envirem.github.io/}
#' @references Title P.O., Bemmels J.B. 2018. ENVIREM: an expanded set of bioclimatic and topographic variables increases flexibility and improves the performance of ecological niche modeling. Ecography. 41:291-307.
#' @export
#' @examples
#' \dontrun{
#' # For a compleate list of posible values of the function
#' # for current period
#' # clim_data_current <- base::readRDS(file.path(system.file("extdata",
#' #                                                   package = "ntbox"),
#' #                                                  "envirem_clim_current.rds"))
#' # For a compleate list of posible values of the function
#' # for pass perids (holo,lgm)
#'
#' clim_eu_lgm_ccsm4_10min <- get_envirem_clim(period="lgm",
#'                                             gcm="ccsm4",
#'                                             region="Europe",
#'                                             resolution= "10arcmin",
#'                                             fmt ="geotiff",
#'                                             sv_dir = "~/Desktop",
#'                                             load2r=TRUE)
#' }

get_envirem_clim <- function(period,gcm=NULL,region,resolution,fmt,sv_dir=getwd(),load2r){

  if(period=="current" || is.null(gcm)){
    clim_data <- base::readRDS(file.path(system.file("extdata",
                                                     package = "ntbox"),
                                         "envirem_clim_current.rds"))
    layer_code <- paste(period,
                        region,
                        resolution,
                        fmt,sep="_")

  }


  url_envirem <- "https://deepblue.lib.umich.edu"
  clim_stack <- NULL
  citation_inf <- NULL

  if(period %in% c("holo","lgm")){
    layer_code <- paste(period,
                        gcm,
                        region,
                        resolution,
                        fmt,sep="_")

    clim_data <- base::readRDS(file.path(system.file("extdata",
                                                     package = "ntbox"),
                                         "envirem_clim_pass.rds"))
  }




  layer_ID <- which(clim_data$split_code %in% layer_code)
  if(length(layer_ID)>0L){
    if(!dir.exists(sv_dir))
      stop("No such a file or directory...")


    fname <- paste0("envirem_",layer_code)
    dirname <- file.path(sv_dir,fname)

    if(!dir.exists(dirname))
      dir.create(dirname)
    fname_path <- file.path(dirname,paste0(fname,".zip"))


    url <- paste0(url_envirem,clim_data$url[layer_ID])
    utils::download.file(url = url,destfile = fname_path,method = "curl" )
    utils::unzip(fname_path,exdir=dirname)
    if(load2r)
      clim_stack <- ntbox::rlayers_ntb(dirname)
    citation_inf <- paste("Title P.O., Bemmels J.B. 2018. ENVIREM: an expanded",
                          "set of bioclimatic and topographic variables",
                          "increases flexibility and improves performance of",
                          "ecological niche modeling. Ecography. 41:291-307.")
  }
  else
    warning(paste("No climate data for:",
                  period,gcm,region,resolution,fmt))
  if(!is.null(citation_inf))
    message(paste("Please cite as:\n",citation_inf))
  return(clim_stack)
}

