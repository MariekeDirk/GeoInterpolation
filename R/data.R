#' Temperature data from stations
#' 
#' This data-set contains KNMI temperature data from 34 stations, from 1995 until 2014.
#' 
#' @format A data frame with 229183 rows and 8 variables:
#' \describe{
#'  \item{Stn}{station number}
#'  \item{Locatie}{Location name of the station}
#'  \item{RDN_X}{x-coordinates of the station in the Dutch RD-coordinate system}
#'  \item{RDN_Y}{y-coordinates of the station in the Dutch RD-coordinate system}
#'  \item{Datum}{Day of the measurement in YYYYMMDD format}
#'  \item{Tg}{Mean temperature in degrees celcius}
#'  \item{Tn}{Minimum temperature in degrees celcius}
#'  \item{Tx}{Maximum temperature in degrees celcius}
#' }
"temperature_stationdata"

#' Daily Minimum and Maximum temperatures dataset
#' 
#' This data-set contains KNMI temperature data from 34 stations, from 1990 until 2017.
#' 
#' @format A data frame with 325411 rows and 6 variables:
#' \describe{
#'  \item{Datum}{Datum in the format YYYY-MM-DD}
#'  \item{DS_CODE}{unique code name of the observation station 3 numbers ending with _H}
#'  \item{STN}{code name of the observation station 3 numbers}
#'  \item{TN}{Minimum temperature of the day out of the 6 hour minimum temperatures}
#'  \item{TX}{Maximum temperature of the day out of the 6 hour maximum temperatures}
#'  \item{DTR}{Diurnal Temperature Range (DTR) calculated as TX-TN}
#'}
"temperature_min_max"


#' Radiation data from stations
#' 
#' This data-set contains KNMI incomming radiation data from 32 stations and satellite observations from MSG at these locations, from 2004 until 2016.
#' The satellite product SICCS was used and compared using the 'over' function. 
#' 
#' @format A data frame with 145063 rows and 7 variables:
#' \describe{
#'  \item{Datum}{Day of the measurement in YYYY-MM-DD format}
#'  \item{DS_CODE}{unique code name of the observation station 3 numbers ending with _H}
#'  \item{RDN_X}{x-coordinates of the station in the Dutch RD-coordinate system}
#'  \item{RDN_Y}{y-coordinates of the station in the Dutch RD-coordinate system}
#'  \item{Q}{Incomming Radiation from the ground-based observations}
#'  \item{SAT}{Incomming Radiation from the satellite observations}
#'  \item{DIFF}{Difference between the ground-based observations and satellite observations}
#' }
"solar_radiation"

#' Climatological record mean temperature data from AWS stations
#' 
#' This data-set contains KNMI temperature data from AWS stations, from 1901 until 2017. The time series is homogenious. 
#' 
#' @format A data frame with 635814 rows and 3 variables:
#' \describe{
#'  \item{STN}{station number}
#'  \item{Datum}{Day of the measurement in YYYY-MM-DD format}
#'  \item{Tg}{Mean temperature in degrees celcius}
#' }
"temperature_climate"

#' Coordinates of the AWS station in RD
#' 
#' Description of the sites, inlcuding coordinates in lat lon and Rijksdriehoekstelsel for all AWS stations
#' 
#' @format A data frame with 50 rows and 8 variables:
#' \describe{
#'  \item{STN}{station number}
#'  \item{DS_ALT}{altitude [m]}
#'  \item{DS_NAME}{name site}
#'  \item{DS_LAT}{latitude WGS84}
#'  \item{DS_LON}{longitude WGS84}
#'  \item{optional}{TRUE or FALSE}
#'  \item{RDN_X}{rijksdriehoeks x-coordinates}
#'  \item{RDN_Y}{rijksdriehoeks y-coordinates}
#' }
"coords_aws"

#' Distance to the sea grid
#' 
#' In order to estimate the distance to the sea within the Netherlands a virtual shoreline is drawn along the Dutch coast.
#' The "shoreline" is North of the Waddenzee.
#' 
#' @slot distsea distance to the virtual shoreline in meters 
#' 
"distsea.grid"

#'
#'Metadata from Wunderground stations
#'
#'@format A data frame with 10 rows and 8 variables:
#' \describe{
#'  \item{Station.ID}{station name}
#'  \item{Lat}{Latitude of the station in WGS84}
#'  \item{Lon}{Longitude of the station in WGS84}
#'  \item{System}{Hardware used for the meteorological measurements}
#'  \item{Elevation}{Elevation of the station}
#'  \item{start}{Date of the first measurement of the station}
#'  \item{stop}{Date of the last measurement}
#'  \item{number_obs}{Number of observations between the start and stop dates}
#' }
#' 
#' 
"wunderground_meta"

