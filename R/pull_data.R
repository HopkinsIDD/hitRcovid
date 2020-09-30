
#' Pulls HIT-COVID database from GitHub
#' 
#' This function pulls the HIT-COVID database from the GitHub repo: 
#' \url{https://github.com/HopkinsIDD/hit-covid}.
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README 
#' (\url{https://github.com/HopkinsIDD/hit-covid}) 
#' 
#' @seealso \link{filter_data}
#' 
#' @export


pull_data <- function(){
  
  #Pulling data from GitHub
  urlfile = 'https://raw.githubusercontent.com/HopkinsIDD/hit-covid/master/data/hit-covid-longdata.csv'
  data <- utils::read.csv(url(urlfile))
  
  #Removing rows denoting no update and columns not really of interest
  keep_col <- !names(data) %in% c("record_id", "update", "national_entry")
  data2 <- data[data$update == "Update", keep_col]
  
  return(data2)
}



#' Subsets HIT-COVID database
#' 
#' This function subsets the HIT-COVID database after it has been loaded with \link{pull_data}.
#' If no filtering arguments are supplied the entire database is returned.
#' There is the option to filter by continent, country, admin 1 unit, locality or intervention group.
#' 
#' All filtering arguments are optional. If none are provided, the entire database will be returned.
#' Any or all of the arguments can be specified allowing filtering by location, intervention type
#' or both. The locality field is used infrequently in this database and this filtering argument
#' should only be used if it is known that the database has a certain locality (lower than the 
#' admin1 level). The dataset is filtered in the following order:
#' continent, country, admin1, locality, intervention group
#' 
#' As part of the larger effort, there was a special project to collect some USA county-level data.
#' If interested in that project, set \code{usa_county} to TRUE. If you want to exclude the USA
#' county-level data, set \code{usa_county} to FALSE, if you want to include the USA county-level data
#' along with other records, keep usa_county as NULL
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{pull_data}
#' @param continent vector of continent names to filter the data to; should be one of
#' \code{c("Asia", "Europe", "Africa", "Oceania", "North America", "South America")}
#' @param country vector of ISO 3166-1 alpha-3 country codes to filter the data to 
#' (see \link{geo_lookup} for concordance of country codes to names)
#' @param admin1 vector of the first administrative unit codes to filter the data to
#' (see \link{geo_lookup} for concordance of admin 1 codes to names)
#' @param locality vector of the names of localities to include (this is a free text field)
#' @param intervention_group vector of intervention group to filter the data to 
#' (see \link{intervention_lookup} column "intervention_group" or "intervention_specific" for options)
#' @param include_usa_county logical indicating if the data should be filtered to USA county-level data.
#' If set to TRUE, only county-level data from \code{country = "USA"} will be provided.
#' If FALSE, all USA county-level will be removed and country filtering will be determined by the
#' \code{country} argument. If left as NULL, USA county-level data will be included and the country
#' filtering will be determined by the \code{country} argument.
#' @param remove_columns a logical indicating if columns with only missing values should be removed 
#' (default is TRUE)
#' 
#' @seealso \link{pull_data}
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README
#' (\url{https://github.com/HopkinsIDD/hit-covid}) but excluding any columns that have only missing
#' values if \code{remove_columns = TRUE}
#' 
#' @export


filter_data <- function(hit_data, continent = NULL, country = NULL, admin1 = NULL, locality = NULL,
                        intervention_group = NULL, include_usa_county = NULL, remove_columns = TRUE){
  
  ## Error handling -------------------------------------------------------------------------------
  
  #Determine if continent input is valid
  continentTest <- !continent %in% c("Asia", "Europe", "Africa", "Oceania", "North America", "South America")
  if(sum(continentTest) > 0){
    warning("At least one continent provided is not valid")
  }
  
  #Look for countries specified, return warning with any not found
  wrong_country <- country[!country %in% geo_lookup$country]
  if(length(wrong_country) >= 1){
    warning("The following country codes are not valid: ",
            paste0(paste0(wrong_country, collapse = ", ")))
  }
  
  #Look for admin units specified, return warning with any not found
  wrong_admin1 <- admin1[!admin1 %in% geo_lookup$admin1]
  if(length(wrong_admin1) >= 1){
    warning("The following admin1 codes are not valid: ",
            paste0(paste0(wrong_admin1, collapse = ", ")))
  }
  
  #Look for intervention specified, return warning with any not found
  wrong_int <- intervention_group[!intervention_group %in% intervention_lookup$intervention_group]
  if(length(wrong_int >= 1)){
    warning("The following intervention codes are not valid: ",
            paste0(paste0(wrong_int, collapse = ", ")))
  }
  
  
  ## Filtering by parameters specified ------------------------------------------------------------
  
  #Continent
  if(!is.null(continent)){
    countries <- unique(geo_lookup[geo_lookup$continent %in% continent, "country"])
    data <- hit_data[hit_data$country %in% countries ,]
  }else{
    data <- hit_data
  }
  
  #Country
  if(!is.null(country)){
    miss_country <- country[!country %in% data$country & !country %in% wrong_country]
    if(length(miss_country) >= 1){
      warning("The following countries are not represented in the database: ",
              paste0(paste0(miss_country, collapse = ", "))) 
    }
    data2 <- data[data$country %in% country ,]
  }else{
    data2 <- data
  }
  
  #Admin1
  if(!is.null(admin1)){
    miss_admin1 <- admin1[!admin1 %in% data2$admin1 & !admin1 %in% wrong_admin1]
    if(length(miss_admin1) >= 1){
      warning("The following admin1 units are not represented in the database with the provided countries: ",
              paste0(paste0(miss_admin1, collapse = ", "))) 
    }
    data3 <- data2[data2$admin1 %in% admin1,]
  }else{
    data3 <- data2
  }
  
  #Locality
  if(!is.null(locality)){
    miss_local <- locality[!locality %in% data3$locality]
    if(length(miss_local) >= 1){
      warning("The following admin1 units are not represented in the database with the provided countries and admin1 units: ",
              paste0(paste0(miss_local, collapse = ", "))) 
    }
    data4 <- data3[data3$locality %in% locality,]
  }else{
    data4 <- data3
  }
  
  #Intervention group
  if(!is.null(intervention_group)){
    miss_int <- intervention_group[!intervention_group %in% data4$intervention_group & 
                                     !intervention_group %in% wrong_int]
    if(length(miss_int) >= 1){
      warning("The following interventions are not represented in the database with the provided countries, admin1 units, and localities: ",
              paste0(paste0(miss_int, collapse = ", "))) 
    }
    data5 <- data4[data4$intervention_group %in% intervention_group,]
  }else{
    data5 <- data4
  }
  
  #Addressing include_usa_county argument
  if(is.null(include_usa_county)){
    data6 <- data5
  }else if(include_usa_county == TRUE){
    data6 <- data5[!is.na(data5$usa_county), ]
  }else if(include_usa_county == FALSE){
    data6 <- data5[is.na(data5$usa_county), ]
  }else{
    data6 <- data5
  }
  
  
  ## Final formatting -----------------------------------------------------------------------------
  
  if(nrow(data6) == 0){
    warning("The set of filters provided did not match any records in the database.")
  }
  
  #Remove columns that are completely empty (print note with columns that were removed)
  if(remove_columns == TRUE){
    # Removing empty columns except status and date_of_update
    data7 <- data6[, dplyr::select_if(data6, ~ !all(is.na(.))) %>% names()]
  }else{
    data7 <- data6
  }
  
  return(data7)
}



