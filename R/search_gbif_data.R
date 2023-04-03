#' Function to search and download gbif data
#' @description Searchs gbif data using the function \code{\link[spocc]{occ}} from the spocc package
#' @param genus Species genus
#' @param species Species name
#' @param occlim Occurrence data search limit.
#' @param writeFile Write gibif data into a csv
#' @param leafletplot Logical, if TRUE the records will be plotted on a leaflet map.
#' @param showClusters Logical. Display geographic cluster on the leaflet map.
#' @param ... Pass arguments to the function  \code{\link[spocc]{occ}}  from spocc
#' @return Returns a data.frame with coordinate data from species
#' @details Note that this function is for exploration proposes.
#' If you want to use GBIF data for publications you should generate and cite
#' DOI information according to \href{https://www.gbif.org/citation-guidelines}{this}
#' @export
#' @import leaflet
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Species genus
#' genus <- "ambystoma"
#' # Species name
#' species <- "tigrinum"
#' # GBIF search
#' ambystoma_tigrinum <- ntbox::search_gbif_data(genus,species,
#'                                        occlim=100,
#'                                        writeFile=FALSE)
#' head(ambystoma_tigrinum[,1:5])
#' }
#'
search_gbif_data <- function(genus,species,occlim=10000,
                             writeFile=FALSE,leafletplot=FALSE,
                             showClusters=FALSE,...){

  # Check if species data is on working directory
  file_name <- tolower(paste0(genus,"_",
                              species, occlim,".csv"))

  if(file_name %in% list.files())
    data_gbif <- utils::read.csv(file_name,header=TRUE)

  # Gbif search
  else{

    genus <- stringi::stri_trans_general(genus,
                                         id = "Title")

    df_list <- spocc::occ(query = paste(genus,species),
                          from = 'gbif',limit = occlim,...)

    # GBIF data
    data_gbif <- data.frame(df_list$gbif$data[[1]])
    if(nrow(data_gbif)==0L)
      return(NULL)
    # Remove NA data from longitude
    data_gbif <- data_gbif[!is.na(data_gbif$longitude),]
    # Remove NA data from latitude
    data_gbif <- data_gbif[!is.na(data_gbif$latitude),]

    data_gbif_1 <- lapply(names(data_gbif), function(x) {
      n_rows <- dim(data_gbif)[1]
      v_val <- unlist(data_gbif[,x])
      if(length(v_val) != n_rows)
        v_val <- rep(NA,n_rows)
      return(v_val)
    })
    names(data_gbif_1) <- names(data_gbif)
    data_gbif <- as.data.frame(data_gbif_1)
    #  Send a warning menssage if the species is not in GBIF
    data_gbif[["ntboxID"]] <- 1:nrow(data_gbif)

    if(length(data_gbif)==0L){
      data_gbif <- NULL
    }
    else if(writeFile){
      utils::write.csv(data_gbif,file_name,row.names = FALSE)
    }
    if(is.data.frame(data_gbif) && leafletplot){
      data_gbif$leaflet_info <- paste("<b>Species: </b>",
                                      data_gbif$species,"</a><br/>",
                                      "<b>ntboxID:</b>",data_gbif$ntboxID,
                                      "<br/><b>Record key:</b>",data_gbif$key,
                                      "<br/><b>Identified on: </b>",
                                      data_gbif$dateIdentified,
                                      "<br/><b>Record url: </b><a href='",
                                      data_gbif$references,
                                      "' target='_blank'>click</a>")
      m <- leaflet::leaflet(data_gbif)
      m <- m %>%
        leaflet::addTiles()
      m <- m  %>%
        leaflet::addCircleMarkers(lng=~longitude, lat=~latitude,
                                  popup= ~leaflet_info,
                                  fillOpacity = 0.25,
                                  radius = 7,
                                  clusterOptions = ifelse(showClusters,
                                                          leaflet::markerClusterOptions(),
                                                          NA))

      print(m)
    }
  }
  return(data_gbif)
}
