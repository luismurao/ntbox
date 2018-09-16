#' Function to convert leaflet.extras output to SpatialPolygonsDataFrame9
#' uses the reactive values from shiny's draw_all_features (leaflet.extras.


ldraw2sp <- function(leaflet_draw) {

  if(leaflet_draw$type==  "FeatureCollection" && length(leaflet_draw$features)>0L){

    xy2sp <- lapply(1:length(leaflet_draw$features),function(y){
      s12 <- leaflet_draw$features[[y]]
      if(s12$geometry$type=="Polygon"){
        df1 <- lapply(seq_along(s12$geometry$coordinates[[1]]),function(x){
          xy2sp_0 <- data.frame(x=s12$geometry$coordinates[[1]][x][[1]][[1]],
                                y=s12$geometry$coordinates[[1]][x][[1]][[2]])
          return(xy2sp_0)

        })
        df1 <- do.call("rbind.data.frame",df1)

      }
      else
        df1 <- NULL


      return(df1)
    })
    xy2sp[sapply(xy2sp, is.null)] <- NULL
    if(length(xy2sp)>0L){
      pols <- lapply(xy2sp, Polygon)

      pols_ids <- lapply(seq_along(pols), function(i)
        Polygons(list(pols[[i]]), ID =i ))

      sp_pols <- SpatialPolygons(pols_ids, proj4string = CRS("+proj=longlat +datum=WGS84") )

      sp_pols_df <- SpatialPolygonsDataFrame(sp_pols,
                                             data.frame(id =1:length(pols),
                                                        row.names = 1:length(pols)))

      return(sp_pols_df)
    }
    else
      warning("No polygon object")
  }
  warning("No polygon object")
}
