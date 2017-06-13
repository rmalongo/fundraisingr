
#' Calculate any fiscal year
#'
#' calculate fiscal year of a date based on 7/1 - 6/30 fiscal calendar
#' @param x A date object
#' @return The fiscal year of the date object
#' @export

calculateFY <- function(date = Sys.Date(), date.format = "%m/%d/%Y", ytd = FALSE){

  date <- as.Date(date, date.format)

  fy.date <-
    ifelse(month(date) %in% c(1:6),
           year(date),
           year(date) + 1)

  if(ytd == TRUE){

    fy <- getFY()

    end.this.fy  <- as.Date(paste0("6/30/", fy), format = "%m/%d/%Y")

    days.left.this.fy <- end.this.fy - Sys.Date()

    end.date.fy  <- as.Date(paste0("6/30/", fy.date), format = "%m/%d/%Y")

    days.left.date.fy <- end.date.fy - date

    if(days.left.date.fy >= days.left.this.fy){

      return(fy.date)

    }else{

      return(NA)

    }

  }else{

    return(fy.date)

  }

}

