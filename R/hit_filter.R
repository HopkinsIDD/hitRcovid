
#' Pulls HIT-COVID database from GitHub
#' 
#' This function pulls the HIT-COVID database from the GitHub repo: 
#' \url{https://github.com/HopkinsIDD/hit-covid}.
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README 
#' (\url{https://github.com/HopkinsIDD/hit-covid}) 
#' 
#' @seealso \link{hit_filter}
#' 
#' @export


hit_pull <- function(){
  
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
#' This function subsets the HIT-COVID database after it has been loaded with \link{hit_pull}.
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
#' If filtering to certain admin1 units, national data will be included by default as often these
#' policies carry down to the admin units. If only the admin1 data is desired, set 
#' \code{include_national} to FALSE. Conversely when filtering to certain countries or 
#' continents, all admin1 information for those countries will be included by default. If only 
#' national data is desired, set \code{include_admin1} to FALSE. Because the locality field is 
#' rarely used, the locality (lower than admin1) data is excluded by default unless a locality is
#' specified for filtering or if \code{include_locality} is set to TRUE.
#' 
#' As part of the larger effort, there was a special project to collect some USA county-level data.
#' If interested in that project, set \code{usa_county_data} to TRUE. If you want to exclude the USA
#' county-level data, set \code{usa_county_data} to FALSE, if you want to include the USA county-level data
#' along with other records, keep \code{usa_county_data} as NULL
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param continent vector of continent names to filter the data to; should be one of
#' \code{c("Asia", "Europe", "Africa", "Oceania", "North America", "South America")}
#' @param country vector of ISO 3166-1 alpha-3 country codes to filter the data to 
#' (see \link{geo_lookup} for concordance of country codes to names)
#' @param admin1 vector of the first administrative unit codes to filter the data to
#' (see \link{geo_lookup} for concordance of admin 1 codes to names). 
#' @param locality vector of the names of localities to include (this is a free text field)
#' @param include_national logical indicating if national-level data should be included (default is TRUE)
#' @param include_admin1 logical indicating if admin1-level data should be included (default is TRUE)
#' @param include_locality logical indicating if locality data should be included (default is FALSE)
#' @param intervention_group vector of intervention group to filter the data to 
#' (see \link{intervention_lookup} column "intervention_group" for options)
#' @param usa_county_data logical indicating if the data should be filtered to USA county-level data.
#' If set to TRUE, only county-level data from \code{country = "USA"} will be provided.
#' If FALSE, all USA county-level will be removed from the filtered dataset.
#' If left as NULL (the default), USA county-level data will be included along with other records in
#' the filtered dataset.
#' @param remove_columns a logical indicating if columns with only missing values should be removed 
#' (default is TRUE)
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README
#' (\url{https://github.com/HopkinsIDD/hit-covid}) but excluding any columns that have only missing
#' values if \code{remove_columns = TRUE}
#' 
#' @examples
#' 
#' # Pulling HIT-COVID database
#' hit_data <- hit_pull()
#' 
#' #Filtering to Africa
#' africa <- hit_filter(hit_data, continent = "Africa")
#' 
#' #Filtering to border closures in china
#' china <- hit_filter(hit_data, country = "CHN", intervention_group = "closed_border")
#' 
#' #Filtering to New Jersey state and national data
#' nj <- hit_filter(hit_data, admin1 = "USA.31_1")
#' 
#' #Filtering to just New Jersey state-level data
#' nj <- hit_filter(hit_data, admin1 = "USA.31_1", include_national = FALSE)
#' 
#' #Filtering to all national data
#' national <- hit_filter(hit_data, include_admin1 = FALSE)
#' 
#' #Filtering to just USA county data
#' usa_county <- hit_filter(hit_data, usa_county_data = TRUE)
#' 
#' #Removing USA county data
#' no_county <- hit_filter(hit_data, usa_county_data = FALSE)
#' 
#' @seealso \link{hit_pull}
#' 
#' @export


hit_filter <- function(hit_data, continent = NULL, country = NULL, admin1 = NULL, locality = NULL,
                       include_national = TRUE, include_admin1 = TRUE, include_locality = FALSE,
                        intervention_group = NULL, usa_county_data = NULL, remove_columns = TRUE){
  
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
  
  #Removing missing status_simp (invalid "unknown" status from early survey versions)
  hit_data <- hit_data[!is.na(hit_data$status_simp), ]
  
  #Removing restaurant reduced (duplicate information)
  hit_data <- hit_data[hit_data$intervention_name != "Limiting number of patrons in restaurants",]
  
  #Resolving the inclusion arguments
  if(include_national == FALSE){
    hit_data <- hit_data[!is.na(hit_data$admin1), ]
  }
  if(include_admin1 == FALSE & is.null(admin1)){
    hit_data <- hit_data[is.na(hit_data$admin1), ]
  }
  if(include_locality == FALSE & is.null(locality)){
    hit_data <- hit_data[is.na(hit_data$locality), ]
  }
  
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
      warning("The following countries are not represented in the database with the provided continent: ",
              paste0(paste0(miss_country, collapse = ", "))) 
    }
    data2 <- data[data$country %in% country ,]
  }else{
    data2 <- data
  }
  
  #Admin1
  #Finding countries represented by the admin units
  admin_countries <- gsub("[0-9]+|\\.|\\_", "", admin1)
  if(!is.null(admin1)){
    miss_admin1 <- admin1[!admin1 %in% data2$admin1 & !admin1 %in% wrong_admin1]
    if(length(miss_admin1) >= 1){
      warning("The following admin1 units are not represented in the database with the provided countries: ",
              paste0(paste0(miss_admin1, collapse = ", "))) 
    }
    data3 <- data2[data2$admin1 %in% admin1 | 
                     (data2$country %in% admin_countries & is.na(data2$admin1)), ]
  }else{
    data3 <- data2
  }
  
  #Locality
  if(!is.null(locality)){
    miss_local <- locality[!locality %in% data3$locality]
    if(length(miss_local) >= 1){
      warning("The following localities are not represented in the database with the provided countries and admin1 units: ",
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
  
  #Addressing usa_county_data argument
  if(is.null(usa_county_data)){
    data6 <- data5
  }else if(usa_county_data == TRUE){
    data6 <- data5[!is.na(data5$usa_county), ]
  }else if(usa_county_data == FALSE){
    data6 <- data5[is.na(data5$usa_county), ]
  }
  
  
  ## Final formatting -----------------------------------------------------------------------------
  
  if(nrow(data6) == 0){
    warning("The set of filters provided did not match any records in the database.")
  }
  
  #Remove columns that are completely empty
  if(remove_columns == TRUE){
    # Removing empty columns
    emptycols <- colSums(is.na(data6)) == nrow(data6)
    data7 <- data6[!emptycols]
  }else{
    data7 <- data6
  }
  
  return(data7)
}



