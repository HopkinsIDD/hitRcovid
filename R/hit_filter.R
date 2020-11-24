
#' Pulls HIT-COVID database from GitHub
#' 
#' This function pulls the HIT-COVID database from the GitHub repo: 
#' \url{https://github.com/HopkinsIDD/hit-covid}. If desired, it also uses the 
#' \code{covidregionaldata} package to add the date of the first case of COVID-19 and the date of 
#' the first death from COVID-19. The source is either the European Centre for Disease Control 
#' (ECDC) or the World Health Organization (WHO).
#' 
#' @param add_first_case a logical indicating if information about the first case and the first
#' death for each country should be added to the dataset
#' @param source the source of the case data that is used to determine the date of first case and
#' first death if \code{add_first_case} is TRUE: one of "ECDC" or "WHO" (default is "WHO")
#' 
#' @return 
#' A dataframe with columns as described in the GitHub README 
#' (\url{https://github.com/HopkinsIDD/hit-covid}) 
#' 
#' @examples 
#' 
#' \donttest{
#' #Pulling the HIT-COVID database and adding date of first case and death from WHO
#' hit_data <- hit_pull(add_first_case = TRUE, source = "WHO")
#' 
#' #Pulling the HIT-COVID databasea and not adding the date of first case and first death
#' hit_data <- hit_pull(add_first_case = FALSE)
#' }
#' 
#' @seealso \link{hit_filter}, \link{get_first_case}, \link[covidregionaldata]{get_national_data}
#'
#' @references 
#' Sam Abbott, Katharine Sherratt, Jonnie Bevan, Hamish Gibbs, Joel Hellewell, James Munday,
#' Paul Campbell and Sebastian Funk (2020). covidregionaldata: Subnational Data for the
#' Covid-19 Outbreak. R package version 0.6.0.
#' https://CRAN.R-project.org/package=covidregionaldata
#' 
#' @export


hit_pull <- function(add_first_case = TRUE, source = c("WHO", "ECDC")){
  
  #Pulling data from GitHub
  urlfile = 'https://raw.githubusercontent.com/HopkinsIDD/hit-covid/master/data/hit-covid-longdata.csv'
  data <- utils::read.csv(url(urlfile))
  
  #Removing rows denoting no update and columns not really of interest
  keep_col <- !names(data) %in% c("record_id", "update", "national_entry")
  data2 <- data[data$update == "Update", keep_col]
  
  #Adding first case and first death
  if(add_first_case == TRUE){
    firsts <- get_first_case(source = source)
    data3 <- merge(data2, firsts, all.x = TRUE)
  }else{
    data3 <- data2
  }
  
  return(data3)
}



#' Subsets HIT-COVID database
#' 
#' This function subsets the HIT-COVID database after it has been loaded with \link{hit_pull}.
#' If no filtering arguments are supplied the entire database is returned.
#' There is the option to filter by continent, country, admin 1 unit, locality or intervention group.
#' 
#' All filtering arguments are optional. If none are provided, the entire database of national and
#' admin 1 unit data will be returned.Any or all of the arguments can be specified allowing 
#' filtering by location, intervention type or both. The locality field is used infrequently in 
#' this database and this filtering argument should only be used if it is known that the database 
#' has a certain locality (lower than the admin1 level). The dataset is filtered in the following order:
#' continent, country, admin1, locality, intervention group
#' 
#' If filtering to certain admin1 units, national data will be included by default as often these
#' policies carry down to the admin units. If only the admin1 data is desired, set 
#' \code{include_national} to FALSE. Conversely when filtering to certain countries or 
#' continents, all admin1 information for those countries will be included by default. If only 
#' national data is desired, set \code{include_admin1} to FALSE. Because the locality field is 
#' rarely used, the locality (lower than admin1) data is excluded by default unless a locality is
#' specified for filtering or if \code{include_locality} is set to TRUE. If filtering by continent,
#' countries in "Eurasia" will be included when filtering to either "Europe" or "Asia".
#' 
#' As part of the larger effort, there was a special project to collect some USA county-level data.
#' If interested in only the data from that project, set \code{usa_county_data} to "restrict_to".
#' If you want to exclude the USA county-level data, set \code{usa_county_data} to "exclude",
#' if you want to include the USA county-level data along with other records, 
#' set \code{usa_county_data} to "include". Note that continent, country, and admin1 filtering 
#' take precedence and what USA county data is included will depend on these filtering arguments.
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param continent vector of continent names to filter the data to; should be one of
#' "Antarctica", "Asia", "Europe", "Africa", "Oceania", "North America", "South America"
#' @param country vector of country ISO 3166-1 alpha-3 codes to filter the data to 
#' (see \link{geo_lookup} for concordance of country codes to names)
#' @param admin1 vector of the first administrative unit GID codes to filter the data to
#' (see \link{geo_lookup} for concordance of admin 1 codes to names). 
#' @param locality vector of the names of localities to include (this is a free text field)
#' @param include_national logical indicating if national-level data should be included (default is TRUE)
#' @param include_admin1 logical indicating if admin1-level data should be included (default is TRUE)
#' @param include_locality logical indicating if locality data should be included (default is FALSE)
#' @param intervention_group vector of intervention group to filter the data to 
#' (see \link{intervention_lookup} column "intervention_group" or run \link{get_interventions} for options)
#' @param usa_county_data character string indicating how to deal with USA county-level data: one
#' of "include", "exclude" or "restrict_to" (default is "exclude").
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
#' #Pulling HIT-COVID database
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
#' usa_county <- hit_filter(hit_data, usa_county_data = "restrict_to")
#' 
#' #Adding USA county data (default is to exclude)
#' no_county <- hit_filter(hit_data, usa_county_data = "include")
#' 
#' @seealso \link{hit_pull} \link{get_interventions}
#' 
#' @export


hit_filter <- function(hit_data,
                       continent = NULL,
                       country = NULL,
                       admin1 = NULL,
                       locality = NULL,
                       intervention_group = NULL,
                       include_national = TRUE,
                       include_admin1 = TRUE,
                       include_locality = FALSE,
                       usa_county_data = c("include", "exclude", "restrict_to"),
                       remove_columns = TRUE){
  
  ## Error handling -------------------------------------------------------------------------------
  
  #Determine if continent input is valid
  if(!is.null(continent)){
    continentTest <- !continent %in% c("Antarctica", "Asia", "Europe", "Africa", "Oceania",
                                       "North America", "South America")
    if(sum(continentTest) > 0){
      warning("At least one continent provided is not valid")
    }
  }
  
  #Look for countries specified, return warning with any not found
  if(!is.null(country)){
    wrong_country <- country[!country %in% hitRcovid::geo_lookup$country]
    if(length(wrong_country) >= 1){
      warning("The following country codes are not valid: ",
              paste0(wrong_country, collapse = ", "))
    }
  }
  
  #Look for admin units specified, return warning with any not found
  if(!is.null(admin1)){
    wrong_admin1 <- admin1[!admin1 %in% hitRcovid::geo_lookup$admin1]
    if(length(wrong_admin1) >= 1){
      warning("The following admin1 codes are not valid: ",
              paste0(wrong_admin1, collapse = ", "))
    }
  }
  
  #Look for intervention specified, return warning with any not found
  if(!is.null(intervention_group)){
    wrong_int <- intervention_group[!intervention_group %in% hitRcovid::intervention_lookup$intervention_group]
    if(length(wrong_int >= 1)){
      warning("The following intervention codes are not valid: ",
              paste0(paste0(wrong_int, collapse = ", ")))
    }
  }
  
  #Setting default usa_county_data to exclude and throwing an error if the input is not valid
  if(all(usa_county_data == c("include", "exclude", "restrict_to"))){
    usa_county_data <- "exclude"
  }else if(length(usa_county_data) > 1){
    stop("usa_county_data should be one of 'include', 'exclude', 'restrict_to'")
  }else if(!usa_county_data %in% c("include", "exclude", "restrict_to")){
    stop("usa_county_data should be one of 'include', 'exclude', 'restrict_to'")
  }
  
  ## Filtering by parameters specified ------------------------------------------------------------
  
  #Removing missing status_simp (invalid "unknown" status from early survey versions)
  #Also removing any other record with missing time (only 1 as of 10/21/2020)
  hit_data <- hit_data[!is.na(hit_data$status_simp) & 
                         hit_data$status_simp != "Unknown" &
                         !is.na(hit_data$date_of_update), ]
  
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
    countries <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$continent %in% continent, "country"])
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
  if(usa_county_data == "include"){
    data6 <- data5
  }else if(usa_county_data == "restrict_to"){
    data6 <- data5[!is.na(data5$usa_county), ]
  }else if(usa_county_data == "exclude"){
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



#' Lists the intervention_group codes
#' 
#' Prints the valid values of the \code{intervention_group} argument used in \link{hit_filter} 
#' and therefore also in \link{intervention_timeline} and \link{intervention_map}.
#'
#' @seealso \link{hit_filter}, \link{intervention_timeline}, \link{intervention_map}
#' 
#' @export

get_interventions <- function(){
  print(unique(hitRcovid::intervention_lookup$intervention_group))
}


#' Lists the admin1 codes for a country
#' 
#' Prints the admin1 levels for a given country that have data in the HIT-COVID database. The database
#' should first be pulled using \link{hit_pull} and fed into this function.
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param country country ISO 3166-1 alpha-3 code
#' 
#' @export

get_admin1 <- function(hit_data, country){
  print(unique(hit_data[hit_data$country == country & !is.na(hit_data$admin1), "admin1"]))
}

