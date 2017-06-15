#' Calculate number of addresses within a given address of an event
#'
#' Find all of the alumni whose address lies within a certain distance of an event.Here i.e 20miles
#' @param x dataframe with columns of Geographic coordinates for zipcode in longitude and latitude respectively (Alumni zipcode)
#' @param y dataframe with columns of Geographic coordinates for zipcode in longitude and latitude respectively (Event zipcode)
#' @return A numeric value
#' @export

Alumnitotal <- function(x, y){
  A1 <- ifelse(distm(x[,2:3], y,fun=distHaversine)/1609<=20,"TRUE","FALSE")
  return(sum(A1=="TRUE"))
}
