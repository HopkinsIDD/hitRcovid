
#' Country and admin1 code look-up table
#' 
#' A dataset with the country codes and admin1 codes with their corresponding names.
#' It can be used to identify codes to use when filtering the HIT-COVID database.
#' 
#' @format A data frame with 3611 rows and 4 variables:
#' \describe{
#' \item{country}{ISO 3166-1 alpha-3 country code}
#' \item{admin1}{first administrative unit code (following GADM5 unless otherwise noted)}
#' \item{country_name}{country name}
#' \item{admin1_name}{level 1 administrative unit name}
#' \item{continent}{continent name}
#' }
#' 
#' @references
#' \url{https://gadm.org/}
#' 
"geo_lookup"