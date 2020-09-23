
#' Pulls current HIT-COVID database from GitHub and subsets if desired
#' 
#' This function pulls the HIT-COVID database from the GitHub repo: 
#' \url{https://github.com/HopkinsIDD/hit-covid}.
#' If no arguments are supplied the entire database is returned. There is also the option to specify
#' filtering by country, admin 1 unit, locality, intervention group, and/or specific intervention.
#' 
#' All filtering arguments are optional. If none are provided, the entire database will be returned.
#' Any or all of the arguments can be specified allowing filtering by location, intervention type
#' or both. The locality field is used infrequently in this database and this filtering argument
#' should only be used if it is known that the database has a certain locality (lower than the 
#' admin1 level).
#' 
#' As part of the larger effort, there was a special project to collect some USA county-level data.
#' If interested in that project, set \code{usa_county} to TRUE. If you want to exclude the USA
#' county-level data, set \code{usa_county} to FALSE.
#' 
#' @param country vector of ISO 3166-1 alpha-3 country codes to filter the data to 
#' (see \code{\link{geo_lookup.csv}} for concordance of country codes to names)
#' @param admin1 vector of the first administrative unit codes to filter the data to
#' (see \code{\link{geo_lookup.csv}} for concordance of admin 1 codes to names)
#' @param locality vector of the names of localities to include (this is a free text field)
#' @param usa_county logical indicating if the data should be filtered to USA county-level data.
#' If set to TRUE, only data from \code{country = "USA"} will be provided. If FALSE, all USA
#' county-level will be removed and country filtering will be determined by the \code{country} 
#' argument.
#' @param intervention vector of intervention group codes to filter the data to
#' (see \code{\link{intervention_lookup.csv}} column "intervention" for options)
#' @param intervention_specific vector of specific interventions to filter the data to
#' (see \code{\link{intervention_lookup.csv}} column "intervention_specific" for options)
#' @param remove_columns a logical indicating if columns with only missing values should be removed 
#' (default is TRUE)
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README
#' (\url{https://github.com/HopkinsIDD/hit-covid}) but excluding any columns that have only missing
#' values if \code{remove_columns = TRUE}


pull_data <- function(country = NULL, admin1 = NULL, locality = NULL, usa_county = NULL,
                      intervention = NULL, intervention_specific = NULL, remove_columns = TRUE){
  
  #Pulling data from GitHub
  urlfile = 'https://raw.githubusercontent.com/HopkinsIDD/hit-covid/master/data/hit-covid-longdata.csv'
  data<-read.csv(url(urlfile))
  
  #Removing rows denoting no update and columns not really of interest
  keep_col <- !names(data) %in% c("record_id", "update", "national_entry")
  data2 <- data[data$update == "Update", keep_col]
  
  
  #Look for countries specified, return warning with any not found
  #Look for admin units specified, return warning with any not found
  #Look for localities specified, return warning with any not found
  #Look for intervention specified, return warning with any not found
  #Look for intervention_specific specified, return warning with any not found
  
  #Restrict by parameters specified
  
  #Remove columns that are completely empty (print note with columns that were removed)
  
  
  return(data2)
}
data2 <- pull_data()


