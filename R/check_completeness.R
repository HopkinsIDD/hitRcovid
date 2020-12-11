
#' Prints a completeness report for a country or admin1 unit
#' 
#' The admin1 units that are included in the HIT-COVID database can be found by using \link{get_admin1}
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param country country ISO 3166-1 alpha-3 code
#' @param admin1 vector of the first administrative unit GID codes to filter the data to
#' (see \link{geo_lookup} for concordance of admin 1 codes to names).
#' 
#' @importFrom rlang .data
#' 
#' @export

check_completeness <- function(hit_data, country = NULL, admin1 = NULL){
  
  #### Error handling -----------------------------------------------------------------------------
  
  #Checking the country code
  if(!is.null(country)){
    #Determining if the country code is valid
    if(!country %in% hitRcovid::geo_lookup$country){
      stop("The country code provided is not valid.")
    }
    #Determining if the country is in the HIT-COVID database
    if(!country %in% hit_data$country){
      stop("The country code provided is not represented in the HIT-COVID database.")
    }
  }
  
  #Checking the admin1 code
  if(!is.null(admin1)){
    #Determining if the admin1 code is valid
    if(!admin1 %in% hitRcovid::geo_lookup$admin1){
      stop("The admin1 code provided is not valid.")
    }
    if(!admin1 %in% hit_data$admin1){
      stop("The admin1 code provided is not represented in the HIT-COVID database.")
    }
  }
  
  #Making sure either country or admin1 is specified
  if(is.null(country) & is.null(admin1)){
    stop("Please provide either a country or admin1 code.")
  }
  
  
  #### Finding report info ------------------------------------------------------------------------
  
  #Filtering HIT-COVID database to country of interest
  locn_data <- hit_filter(hit_data, country = country, admin1 = admin1, remove_columns = FALSE)
  
  #Filter to national data and admin1 data only
  country_national <- locn_data[is.na(locn_data$admin1), ]
  country_admin1 <- locn_data[!is.na(locn_data$admin1), ]
  
  #Reading in the completeness data and filtering to selected country
  urlfile = 'https://raw.githubusercontent.com/HopkinsIDD/hit-covid/master/data/hit-covid-completeness.csv'
  completeness <- utils::read.csv(url(urlfile))
  
  
  if(!is.null(admin1)){
    
    #Finding the country or admin1 name and code
    name <- hitRcovid::geo_lookup[hitRcovid::geo_lookup$admin1 == admin1 & 
                                    !is.na(hitRcovid::geo_lookup$admin1), "admin1_name"][1]
    code <- admin1
    
    #Finding entry quality
    quality_tab <- table(country_admin1$entry_quality)
    quality_tab_p <- prop.table(table(country_admin1$entry_quality))
    
    #Finding date of last update per intervention
    last_update <- dplyr::arrange(country_admin1, .data$intervention_group, desc(.data$entry_time))
    
    #Filtering the completeness dataset
    locn_comp <- completeness[completeness$admin1 == admin1 & !is.na(completeness$admin1), ]
    
  }else{
    
    #Finding the country or admin1 name and code
    name <- hitRcovid::geo_lookup[hitRcovid::geo_lookup$country == country, "country_name"][1]
    code <- country
    
    #Percentage of updates that are national (verses admin1)
    p_national <- round(100 * sum(is.na(locn_data$admin1)) / nrow(locn_data), 1)
    
    #Finding entry quality
    quality_tab <- table(locn_data$entry_quality)
    quality_tab_p <- prop.table(table(locn_data$entry_quality))
    
    #Finding date of last update per intervention
    last_update <- dplyr::arrange(country_national, .data$intervention_group, desc(.data$entry_time))
    
    #Filtering the completeness dataset
    locn_comp <- completeness[completeness$country == country & is.na(completeness$admin1), ]
  }
  
  #Extracting entry quality
  if(length(quality_tab) == 1){
    quality_n <- as.numeric(quality_tab)
    quality_p <- as.numeric(quality_tab_p)
  }else{
    quality_n <- as.numeric(quality_tab[2])
    quality_p <- as.numeric(quality_tab_p[2])
  }
  
  
  ## Table of intervention status
  
  #Finding the date of last update per intervention
  last_update <- dplyr::group_by(last_update, .data$intervention_group) 
  last_update <- dplyr::slice(last_update, 1)
  last_update$date_of_last_update <- as.Date(last_update$entry_time)
  last_update <- last_update[, c("intervention_group", "date_of_last_update")]
  
  #Finding last time each intervention was marked as complete
  complete <- locn_comp[locn_comp$completeness == "Complete" & !is.na(locn_comp$completeness), ]
  complete <- dplyr::arrange(complete, .data$intervention_group, desc(.data$date))
  complete <- dplyr::group_by(complete, .data$intervention_group) 
  complete <- dplyr::slice(complete, 1)
  
  #Finding interventions that were never marked as complete
  incomplete <- locn_comp[!locn_comp$intervention_group %in% complete$intervention_group, ]
  incomplete <- dplyr::arrange(incomplete, .data$intervention_group, desc(.data$date))
  incomplete <- dplyr::group_by(incomplete, .data$intervention_group) 
  incomplete <- dplyr::slice(incomplete, 1)
  
  locn_comp2 <- bind_rows(complete, incomplete)
  locn_comp2 <- locn_comp2[, c("intervention_group", "completeness", "date")]
  
  #Combining last updates for status table
  intervention_names <- unique(hitRcovid::intervention_lookup[, c("intervention_group",
                                                                  "intervention_group_name")])
  status_table <- merge(last_update, locn_comp2, by = "intervention_group", all = TRUE)
  status_table <- merge(intervention_names, status_table, by = "intervention_group")
  status_table <- status_table[, c("intervention_group_name", "date_of_last_update", "completeness", "date")]
  names(status_table) <- c("Intervention", "Last Update",
                           "Intervention Completeness", "Completeness Last Logged")
  status_table <- status_table[order(status_table$Intervention), ]
  
  
  #### Report --------------------------------------------------------------------------------------
  
  cat(paste0("\nCompleteness Report for ", name, " (", code, ").\n\n"))
  
  cat(paste0("There have been ", nrow(country_national), " national level policy changes logged.\n"))
  cat(paste0("There have been ", nrow(locn_data) - nrow(country_national),
             " admin1 level policy changes logged.\n"))
  
  if(is.null(admin1)){
    cat(paste0("Therefore, ", p_national, "% of the updates are at the national level.\n\n")) 
  }
  
  cat(paste0("Of all of the entries for this country, ", quality_n, " (",
             round(100 * quality_p, 1), "%) were marked as verified.\n"))
  
  cat(paste0("At the national level, the last day that a intervention update or 'no updates' was logged was: ",
             as.Date(max(country_national$entry_time)), ".\n"))
  
  cat(paste0("At the admin1 level, the last day that a intervention update or 'no updates' was logged was: ",
             as.Date(max(locn_data$entry_time)), ".\n\n"))
  
  cat("Table of Intervention Completeness")
  print(knitr::kable(status_table))
  
  if(is.null(admin1) & p_national < 20){
    message("Fewer than 20% of the updates for this country are at the national level so this table\n may be misleading. You can run this function at the admin1 level for more detailed information.")
  }
}



