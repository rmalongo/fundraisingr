#' Calculate number of addresses within a given address of an event
#'
#' Find all of the alumni whose address lies within a certain distance of an event.Here i.e 20miles
#' @param x dataframe with columns of Geographic coordinates for zipcode in longitude and latitude respectively (Alumni zipcode)
#' @param y dataframe with columns of Geographic coordinates for zipcode in longitude and latitude respectively (Event zipcode)
#' @return A numeric value
#' @export

Alumnitotal <- function(x,y){

  # define inputs

  tmp_zip <- x
  tmp_center_zip <- y
  radius <- 50

  # grab zipcodes from zipcode package
  data("zipcode")

  # clean zipcode

  tmp_zip <- clean.zipcodes(tmp_zip)

  # input 1 - address
  # subset zipcode data based on address input
  tmp <- zipcode[zipcode$zip %in% tmp_zip,]
  tmp_cord <- data.frame(longitude =tmp$longitude,
                         latitude =  tmp$latitude)


  # input 2 - center point
  # subset zipcode data based on zipcode centroid input
  tmp_center <- zipcode[zipcode$zip == tmp_center_zip,]
  tmp_center <- data.frame(longitude =  tmp_center$longitude,
                           latitude =  tmp_center$latitude)

  tmp_zip
  # calculate distance between input and specified zipcode
  distance <- ifelse(distm(tmp_cord, tmp_center, fun=distHaversine)/1609<=radius,"TRUE","NA")


  if(distance==TRUE){

    return(sum==TRUE)

  }else {

    return(NA)
  }

}

