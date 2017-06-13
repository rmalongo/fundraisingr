#' Calculate current fiscal year
#'
#' Calculate current fiscal year based on 7/1 - 6/30 fiscal calendar
#' @param x No input
#' @return The current fiscal year
#' @export
getFY <- function(){

  fiscal.year <-
    ifelse(month(Sys.Date()) %in% c(1:6),
           year(Sys.Date()),
           year(Sys.Date()) + 1)

  return(fiscal.year)
}
