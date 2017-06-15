#' Calculate number of address within a given address
#'
#' Find all of the alumni who live within a certain distance of an event
#' @param x Geographic coordinates
#' @return A numeric value
#' @export

distance <- function(x, y){
  d1 <- ifelse(distm(x,y,fun=distHaversine)<=20,"TRUE","FALSE")
  return(sum(d1=="TRUE"))
}
