#' inEllipsoid: Determine if a point is inside or outside an ellipsoid
#'
#' @description Determine if a point is inside or outside an ellipsoid.
#' @param centroid A numeric vector of centroids for each environmental variable
#' @param eShape Shape matrix of the ellipsoid (can be a covariance matrix or a minimum volume ellipsoid).
#' @param env_data A data frame with the environmental training data.
#' @param level Proportion of points to be included in the ellipsoids. This paramter is equivalent to the error (E) proposed by Peterson et al. (2008).
#' @return A data.frame with 2 columns. The first "in_Ellipsoid" binary response with values 1 (inside the ellipsoid) and zeros (outside the ellipsoid); the second "mh_dist" Mahalanobis distance to centroid.
#' @export
#' @examples
#' \dontrun{
#' # Bioclimatic layers path
#' wcpath <- list.files(system.file("extdata/bios",
#'                                 package = "ntbox"),
#'                     pattern = ".tif$",full.names = TRUE)
#' # Bioclimatic layers
#' wc <- raster::stack(wcpath)
#' # Occurrence data for the giant hummingbird (Patagona gigas)
#' pg <- utils::read.csv(system.file("extdata/p_gigas.csv",
#'                                   package = "ntbox"))
# Environmental data
#' pg_env <- raster::extract(wc,pg[,c("longitude",
#'                                       "latitude")],
#'                              df=TRUE)
#' pg_env <- pg_env[,-1]
#'
#' pg_ellip <- cov_center(pg_env,mve=TRUE,
#'                        level=0.95,
#'                        vars = c("bio05",
#'                                 "bio06",
#'                                 "bio12"))
#' # Environmental random data
#' env_rdata <- raster::sampleRandom(wc,1000)
#' inErdata <- inEllipsoid(env_data = env_rdata[,c("bio05",
#'                                                 "bio06",
#'                                                 "bio12")],
#'                         centroid = pg_ellip$centroid,
#'                         eShape=pg_ellip$covariance,
#'                         level = 0.99)
#'
#' }
inEllipsoid <- function(centroid,eShape,env_data,level){

  mh_dist <- stats::mahalanobis(env_data,
                                center = centroid,
                                cov =eShape)
  in_Ellipsoid <- mh_dist <= stats::qchisq(level,
                                           length(centroid))
  in_Ellipsoid <- in_Ellipsoid*1
  in_Ellipsoid_mh <- data.frame(in_Ellipsoid,mh_dist )

  return(in_Ellipsoid_mh)
}
