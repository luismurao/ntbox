#' get_chelsa: Get bioclimatic layers from CHELSA
#' @description Get bioclimatic layers from CHELSA for present and future scenarios
#' @param period Time period. Posible values are: "current","2041-2060","2061-2080"
#' @param model Circulation model. Posible values are:'ACCESS1-0',
#' 'bcc-csm1-1', 'BNU-ESM', 'CanESM2','CCSM4','CESM1-BGC','CESM1-CAM5',
#' 'CMCC-CM','CMCC-CMS','CNRM-CM5','CSIRO-Mk3-6-0','FGOALS-g2',
#' 'FIO-ESM',GFDL-CM3','GFDL-ESM2G','GFDL-ESM2M','GISS-E2-H',
#' 'GISS-E2-H-CC','GISS-E2-R','GISS-E2-R-CC','HadGEM2-AO',
#' 'HadGEM2-CC','inmcm4','IPSL-CM5A-LR','IPSL-CM5A-MR','MIROC-ESM',
#' 'MIROC-ESM-CHEM','MIROC5','MPI-ESM-LR','MPI-ESM-MR','MRI-CGCM3',
#' 'MRI-ESM1','NorESM1-M'
#' @param rcp Representative Concentration Pathway. Posible values are "rcp26","rcp45","rcp85".
#' @param sv_dir Path to the directory where the layers will be saved. Default is the working directory of R session.
#' @param load2r Logical. Load layers into R?
#' @seealso \code{\link[ntbox]{get_envirem_elev}}, \code{\link[ntbox]{get_envirem_clim}}
#' @details For more details visit \url{http://chelsa-climate.org/}
#' @references Karger, D.N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2017) Climatologies at high resolution for the earth's land surface areas. Scientific Data 4, 170122.
#' @export
#' @examples
#' \dontrun{
#' chelsa_biocurrent <- get_chelsa(period = "current",
#'                                 sv_dir = "~/Desktop")
#' chelsa_bioCCSM4 <- get_chelsa(period = "2041-2060",
#'                               model="CCSM4",
#'                               rcp="rcp85",
#'                               sv_dir = "~/Desktop")
#' }
get_chelsa <- function(period,model=NULL,rcp=NULL,sv_dir=getwd(),load2r=T){
  chelsa_urls <- NULL
  if(period == "current"){
    url <- "https://www.wsl.ch/lud/chelsa/data/bioclim/integer/"
    m_ab <- "chelsa_current"
    chelsa_names <- c(paste0("CHELSA_bio10_0",1:9,".tif"),
                  paste0("CHELSA_bio10_",10:19,".tif"))
    chelsa_urls <- paste0(url , chelsa_names)



  }
  if(period %in% c("2041-2060","2061-2080") && rcp %in% c("rcp26","rcp45","rcp85")){

    chelsa <- base::readRDS(file.path(system.file("extdata",
                                                  package = "ntbox"),
                                      "chelsa_vars.rds"))
    if(period == "2041-2060")
      url <- "https://www.wsl.ch/lud/chelsa/data/cmip5/2041-2060/bio/"
    if(period == "2061-2080")
      url <- "https://www.wsl.ch/lud/chelsa/data/cmip5/2061-2080/bio/"

    m_ab <- paste(model,period,rcp,sep = "_")
    m_abID <- which( chelsa$M_AB %in% m_ab)
    if(length(m_abID)>0L){
      chelsa_names <- chelsa[m_abID,"layer_name"]
      chelsa_urls <- paste0(url, chelsa[m_abID,"layer_name"])
    }

  }
  if(is.null(chelsa_urls))
    warning(paste("No spatial information for",
                  model,period,rcp))
  else{
    dir_name <- base::file.path(sv_dir, m_ab)
    if(!dir.exists(dir_name )) dir.create(dir_name )
    fnames <- base::file.path(dir_name,chelsa_names)
    ch_down <- seq_along(fnames) %>%
      purrr::map(~utils::download.file(chelsa_urls[.x],
                                       fnames[.x],
                                       method = "curl"))
    if(load2r)
      chelsa_urls <- ntbox::rlayers_ntb(dir_name)
    cite_chelsa <- "Karger, D.N., Conrad, O., Bohner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P. & Kessler, M. (2017) Climatologies at high resolution for the earth's land surface areas. Scientific Data 4, 170122."
    warning(paste("Please cite as",cite_chelsa))
  }
  return(chelsa_urls)
}
