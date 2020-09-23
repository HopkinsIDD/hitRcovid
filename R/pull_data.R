
#' Pulls HIT-COVID database from GitHub
#' 
#' This function pulls the HIT-COVID database from the GitHub repo: 
#' \url{https://github.com/HopkinsIDD/hit-covid}.
#' If no arguments are supplied the entire database is returned. There is also the option to
#' filter by country, admin 1 unit, locality, intervention group, and/or specific intervention.
#' 
#' All filtering arguments are optional. If none are provided, the entire database will be returned.
#' Any or all of the arguments can be specified allowing filtering by location, intervention type
#' or both. The locality field is used infrequently in this database and this filtering argument
#' should only be used if it is known that the database has a certain locality (lower than the 
#' admin1 level). The dataset is filtered in the following order:
#' country, admin1, locality, intervention
#' 
#' As part of the larger effort, there was a special project to collect some USA county-level data.
#' If interested in that project, set \code{usa_county} to TRUE. If you want to exclude the USA
#' county-level data, set \code{usa_county} to FALSE.
#' 
#' @param country vector of ISO 3166-1 alpha-3 country codes to filter the data to 
#' (see \link{geo_lookup} for concordance of country codes to names)
#' @param admin1 vector of the first administrative unit codes to filter the data to
#' (see \link{geo_lookup} for concordance of admin 1 codes to names)
#' @param locality vector of the names of localities to include (this is a free text field)
#' @param usa_county logical indicating if the data should be filtered to USA county-level data.
#' If set to TRUE, only data from \code{country = "USA"} will be provided. If FALSE, all USA
#' county-level will be removed and country filtering will be determined by the \code{country} 
#' argument.
#' @param intervention_group vector of intervention group to filter the data to 
#' (see \link{intervention_lookup} column "intervention_group" or "intervention_specific" for options)
#' @param remove_columns a logical indicating if columns with only missing values should be removed 
#' (default is TRUE)
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README
#' (\url{https://github.com/HopkinsIDD/hit-covid}) but excluding any columns that have only missing
#' values if \code{remove_columns = TRUE}


pull_data <- function(country = NULL, admin1 = NULL, locality = NULL, usa_county = NULL,
                      intervention_group = NULL, remove_columns = TRUE){
  
  #Pulling data from GitHub
  urlfile = 'https://raw.githubusercontent.com/HopkinsIDD/hit-covid/master/data/hit-covid-longdata.csv'
  data <- utils::read.csv(url(urlfile))
  
  #Removing rows denoting no update and columns not really of interest
  keep_col <- !names(data) %in% c("record_id", "update", "national_entry")
  data2 <- data[data$update == "Update", keep_col]
  
  
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

  
  #Restrict by parameters specified
  if(!is.null(country)){
    miss_country <- country[!country %in% data2$country & !country %in% wrong_country]
    if(length(miss_country) >= 1){
      warning("The following countries are not represented in the database: ",
              paste0(paste0(miss_country, collapse = ", "))) 
    }
    data3 <- data2[data2$country %in% country ,]
  }else{
    data3 <- data2
  }
  if(!is.null(admin1)){
    miss_admin1 <- admin1[!admin1 %in% data3$admin1 & !admin1 %in% wrong_admin1]
    if(length(miss_admin1) >= 1){
      warning("The following admin1 units are not represented in the database with the provided countries: ",
              paste0(paste0(miss_admin1, collapse = ", "))) 
    }
    data4 <- data3[data3$admin1 %in% admin1,]
  }else{
    data4 <- data3
  }
  if(!is.null(locality)){
    miss_local <- locality[!locality %in% data4$locality]
    if(length(miss_local) >= 1){
      warning("The following admin1 units are not represented in the database with the provided countries and admin1 units: ",
              paste0(paste0(miss_local, collapse = ", "))) 
    }
    data5 <- data4[data4$locality %in% locality,]
  }else{
    data5 <- data4
  }
  if(!is.null(intervention_group)){
    miss_int <- intervention_group[!intervention_group %in% data5$intervention_group & 
                                     !intervention_group %in% wrong_int]
    if(length(miss_int) >= 1){
      warning("The following interventions are not represented in the database with the provided countries, admin1 units, and localities: ",
              paste0(paste0(miss_int, collapse = ", "))) 
    }
    data6 <- data5[data5$intervention_group %in% intervention_group,]
  }else{
    data6 <- data5
  }
  
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

data_sub <- pull_data()


