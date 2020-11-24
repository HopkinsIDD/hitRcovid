
#' Plots case counts with intervention status timelines
#' 
#' This function plots the daily case counts (epi-curve) for a given country or admin1 unit with
#' as seven day rolling average. The status of select intervention information (border closures, 
#' household confinement, universal mask mandates, restaurant closures, primary school closures, 
#' and retail store closures) are also plotted. The admin1 plotting is only available for 11 countries.
#' 
#' The HIT-COVID database must first be loaded with \link{hit_pull} and fed into this function. 
#' Only one country or admin1 unit can be plotted at a time and the resulting plot will include the
#' case counts for this location with select intervention data. The user can specify the date range 
#' for the plot using \code{first_date} and \code{last_date}. Another way to restrict the plot is to change 
#' \code{case_threshold} then the earliest date of the plot will be when the total number of cases
#' in the country exceeded \code{case_threshold}. If both \code{first_date} and \code{case_threshold} 
#' are provided, \code{first_date} will be used as the start of the plot.
#' 
#' The source for the country level data is either the European Centre for Disease
#' Control (ECDC) or the World Health Organization (WHO) with the WHO as the default. The source for
#' the admin1 data varies depending on the package and can be seen in the 
#' \href{https://github.com/epiforecasts/covidregionaldata/tree/master/R}{covidregional data source code}.
#' The admin1 case counts data is pulled using \link[covidregionaldata]{get_regional_data}. As of now
#' this is only available for 11 countries: Afghanistan, Belgium, Brazil, Canada, Columbia, Germany,
#' India, Italy, Russia, UK, and USA. The admin1 units that are included in the HIT-COVID database
#' can be found by using \link{get_admin1}.
#' 
#' IMPORTANT note about intervention timelines printed under the epi curves. These final bars of each
#' timeline represent the last logged status of an intervention. For some locations, the intervention data
#' may not have been updated which means that older policies would appear to carry to the present when
#' they are not still active. Care should be taken when interpretting these plots without knowledge of
#' the completeness of the intervention data of the location of interest.
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param country country ISO 3166-1 alpha-3 code to plot
#' (see \link{geo_lookup} for concordance of country codes to names)
#' @param admin1 first administrative unit GID codes to plot
#' (see \link{geo_lookup} for concordance of admin 1 codes to names).
#' @param source the source of the daily case count data, one of "ECDC" or "WHO" (default is "WHO").
#' @param case_threshold threshold the total number of cases needs to exceed to start the plot 
#' (default is 0).
#' @param first_date character string indicating the earliest date to plot (default is first day where
#' the total number of cases is greater than the \code{case_threshold}).
#' @param last_date character string indicating the latest date to plot (default is today)
#' @param date_format character string indicating the format of the date inputs (default is "%m/%d/%Y").
#' @param include_title logical indicating if the plot should have a title (default is TRUE)
#' 
#' @examples
#' 
#' # Puling the HIT-COVID database
#' hit_data <- hit_pull(add_first_case = FALSE)
#' 
#' # Plotting the case counts for India
#' intervention_epi(hit_data, country = "IND")
#' 
#' # Starting when there were more than 100 cases
#' intervention_epi(hit_data, country = "IND", case_threshold = 100)
#' 
#' # Plotting the case counts for the New Zealand from February to May 
#' intervention_epi(hit_data, country = "NZL", first_date = "3/1/2020", last_date = "5/31/2020")
#' 
#' # Plotting the case counts for New Jersey, USA
#' intervention_epi(hit_data, admin1 = "USA.31_1")
#'  
#' @seealso \link{hit_filter}
#' 
#' @references 
#' Sam Abbott, Katharine Sherratt, Jonnie Bevan, Hamish Gibbs, Joel Hellewell, James Munday,
#' Paul Campbell and Sebastian Funk (2020). covidregionaldata: Subnational Data for the
#' Covid-19 Outbreak. R package version 0.6.0.
#' https://CRAN.R-project.org/package=covidregionaldata
#' 
#' @importFrom rlang .data
#' 
#' @export


## TODO: Add admin1 data where available

intervention_epi <- function(hit_data,
                      country = NULL,
                      admin1 = NULL,
                      source = c("WHO", "ECDC"),
                      case_threshold = 0,
                      first_date = NULL,
                      last_date = NULL,
                      date_format = "%m/%d/%Y",
                      include_title = TRUE){
  
  #### Error handling -----------------------------------------------------------------------------
  
  source <- toupper(source)
  
  #Setting default source to WHO and throwing an error if the source is not valid
  if(all(source == c("WHO", "ECDC"))){
    source <- "WHO"
  }else if(length(source) > 1){
    stop("source should be one of 'ECDC' or 'WHO'")
  }else if(!source %in% c("WHO", "ECDC")){
    stop("source should be one of 'ECDC' or 'WHO'")
  }
  
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
  
  #Determining if there is admin1-level data for the admin1 code
  admin1_avail <- hitRcovid::geo_lookup[!is.na(hitRcovid::geo_lookup$admin1_ISO), ]
  
  #Checking the admin1 code
  if(!is.null(admin1)){
    #Determining if the admin1 code is valid
    if(!admin1 %in% hitRcovid::geo_lookup$admin1){
      stop("The admin1 code provided is not valid.")
    }
    #Determining if the admin1 code is in the list with case counts data
    if(!admin1 %in% admin1_avail$admin1){
      stop("There are no case count data available for the admin1 code provided.")
    }
    #Determining if the admin1 code is in the HIT-COVID database
    if(!admin1 %in% hit_data$admin1){
      stop("The admin1 code provided is not represented in the HIT-COVID database.")
    }
  }
  
  #Making sure either country or admin1 is specified
  if(is.null(country) & is.null(admin1)){
    stop("Please provide either a country or admin1 code.")
  }
  
  # Determine if the dates entered are valid, if not, return warning
  if(!is.null(first_date)){
    first_date <- as.Date(first_date, date_format)  
    if(is.na(first_date)) {
      stop('first_date is not valid, please enter a valid date in right format.')
    } else if(first_date < as.Date("1/1/2020", date_format) |
              first_date > Sys.Date()) {
      stop("first_date is not valid, please enter valid date between 1/1/2020 and present.")
    } 
  }
  if(!is.null(last_date)){
    last_date <- as.Date(last_date, date_format)  
    if(is.na(last_date)) {
      stop('last_date is not valid, please enter a valid date in right format.')
    } else if(last_date < as.Date("1/1/2020", date_format) |
              last_date > Sys.Date()) {
      stop("last_date is not valid, please enter valid date between 1/1/2020 and present.")
    } 
  }


  
  #### Setting up plotting data -------------------------------------------------------------------
  
  #Creating intervention labels with line breaks for better printing
  int_labels <- cbind.data.frame("intervention_group" = c("household_confined",
                                                          "closed_border",
                                                          "mask",
                                                          "school_closed",
                                                          "store_closed",
                                                          "restaurant_closed"),
                                 "intervention_label" = c("Household confinement",
                                                          "Border closures",
                                                          "Universal mask mandates",
                                                          "Primary school closures",
                                                          "Retail store closures",
                                                          "Restaurant closures"))
  
  ## Filtering to provided country or admin1
  
  #If admin1 is not provided, filter to country
  if(is.null(admin1)){
    #Finding iso_code (2 letter) that goes with the country specified
    iso_code <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$country == country & 
                                                   !is.na(hitRcovid::geo_lookup$country), "alpha_2"])
    
    #Getting all case counts
    case_counts <- covidregionaldata::get_national_data(source = source)
    
    #Filtering to the country provided
    case_counts <- case_counts[!is.na(case_counts$iso_code) & case_counts$iso_code == iso_code, ]
  }
  
  #If admin1 is provided, filter to admin1
  if(!is.null(admin1)){
    
    #Finding the country code that goes with the admin1 unit specified
    country <- substr(admin1, 1, 3)
    
    #Finding name that goes with the country specified
    name <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$country == country & 
                                               !is.na(hitRcovid::geo_lookup$country), "country_name"])
    if(name == "United States"){name <- "USA"}
    if(name == "United Kingdom"){name <- "UK"}
    
    #Finding the iso code or ons region code (UK only) to match the admin1 code
    if(name == "UK"){
      new_code <- admin1_avail[admin1_avail$admin1 == admin1,
                               "ons_region_code_uk"]
    }else{
      new_code <- admin1_avail[admin1_avail$admin1 == admin1,
                               "admin1_ISO"]
    }
    
    #Pulling case counts for the whole country
    case_counts <- covidregionaldata::get_regional_data(country = name)
    
    #Filtering to admin1 unit specified
    if(name == "UK"){
      case_counts <- case_counts[case_counts$ons_region_code == new_code, ]
    }else{
      case_counts <- case_counts[case_counts$iso_3166_2 == new_code, ]
    }
  }
  
  
  
  #If first_date provided, filtering to days after that date
  #If first_date is not provided, filtering to days when there are more than case_threshold cases
  if(!is.null(first_date)){
    case_counts <- case_counts[case_counts$date >= first_date, ]
  }else{
    case_counts <- case_counts[case_counts$cases_total > case_threshold & 
                                 !is.na(case_counts$cases_total), ]
  }
  
  #Finding the min and max date (if first_date and last_date were not user-specified)
  #If provided first_date is before the 
  if(is.null(first_date)){first_date <- min(case_counts$date)}
  if(is.null(last_date)){last_date <- Sys.Date()}
  if(last_date <= first_date){
    stop("first_date must be earlier than last_date")
  }
  
  #Filtering to days before last_date
  case_counts <- case_counts[case_counts$date <= last_date, ]
  
  #Adding 7-day rolling average
  case_counts$cases_smooth <- zoo::rollmean(case_counts$cases_new, 7, fill = NA)

  #Removing missing values
  case_counts <- case_counts[!is.na(case_counts$cases_new), ]

  #Filtering the HIT-COVID database and selecting interventions
  ## TODO: Allow user to select interventions
  if(is.null(admin1)){
    suppressWarnings(
      int_data <- hit_filter(hit_data, country = country, include_admin1 = FALSE,
                           intervention_group = c("household_confined", "closed_border", "mask",
                                                  "store_closed", "restaurant_closed", "school_closed"))
    )
  }
  if(!is.null(admin1)){
    suppressWarnings(
      int_data <- hit_filter(hit_data, admin1 = admin1, include_national = FALSE,
                           intervention_group = c("household_confined", "closed_border", "mask",
                                                  "store_closed", "restaurant_closed", "school_closed"))
    )
  }

  
  #Filtering to just primary school closures for schools
  int_data <- int_data[!int_data$intervention_name %in% c("Nursery school closures",
                                                          "Secondary school closures",
                                                          "Post-secondary school closures"),
                       c("intervention_group", "status", "status_simp", "date_of_update")]
  
  #Ordering by intervention and date
  int_data <- int_data[order(int_data$intervention_group, int_data$date_of_update),]
  
  #Adding status of previous intervention of the same type
  int_data <- dplyr::group_by(int_data, .data$intervention_group)
  int_data <- dplyr::mutate(int_data, lag_status = dplyr::lag(.data$status_simp))
  
  #Combining with intervention labels
  int_data <- merge(int_data, int_labels, by = "intervention_group", all.x = TRUE)
  
  #Finding the groups of consecutive updates of the same intervention with the same status
  split_data <- split(int_data, int_data$intervention_group)
  int_data2 <- purrr::map_dfr(split_data, sameStatus)
  
  #Finding the start and end date for each group of updates with the same status
  int_data2 <- dplyr::group_by(int_data2, .data$intervention_group)
  int_data2 <- dplyr::mutate(int_data2, end = dplyr::lead(.data$start))
  int_data2$end <- ifelse(is.na(int_data2$end), as.character(last_date), int_data2$end)
  
  #Collapsing to one row for each group of updates with the same status
  int_data3 <- dplyr::group_by(int_data2, .data$intervention_group, .data$interval)
  int_data3 <- dplyr::slice_max(int_data3, .data$end)
  int_data3$start <- as.Date(int_data3$start)
  int_data3$end <- as.Date(int_data3$end)
  
  # If 'start' is earlier than first_date and 'end' is later than first_date
  # then set the 'start' to be first_date or this bar will not be plotted
  int_data3[int_data3$start <= first_date & int_data3$end >= first_date,]$start <- first_date
  # If 'start' is within the date range and 'end' is later than last_date
  # then set the 'end' to be the last_date or this bar will not be plotted
  int_data3[int_data3$start >= first_date & int_data3$start <= last_date &
              int_data3$end > last_date,]$end <- last_date
  
  #Removing interventions which start after the time window
  int_data3 <- int_data3[!(int_data3$start >= last_date), ]
  
  #Removing rows where 'start' == 'end', otherwise there will be a dot on the plot
  int_data3 <- int_data3[int_data3$start < int_data3$end, ]
  
  #Setting plotting parameters
  int_data3$color <- ifelse(int_data3$status_simp == "Strongly Implemented", "red",
                            ifelse(int_data3$status_simp == "Partially Implemented", "darkorange",
                                   "grey"))
    
  
  
  #### Making plots -------------------------------------------------------------------------------
  
  #Plot of case counts
  p1 <- ggplot2::ggplot(data = case_counts) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$date, y = .data$cases_new),
                      stat = "identity", na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(x = .data$date, y = .data$cases_smooth),
                       color = "darkblue", size = 1.3, na.rm = TRUE) +
    ggplot2::labs(y = "Number of new cases") +
    ggplot2::xlim(c(first_date - 1, last_date + 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                   axis.title.x = ggplot2::element_blank())
  
  if(include_title == TRUE){
    if(is.null(admin1)){
      title_name <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$country == country &
                                                   !is.na(hitRcovid::geo_lookup$country),
                                                 "country_name"])
    }else{
      title_name <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$admin1 == admin1 &
                                                   !is.na(hitRcovid::geo_lookup$admin1),
                                                 "admin1_name"])
    }
    p1 <- p1 + ggplot2::ggtitle(paste0("Case counts and intervention timeline for ",
                                       title_name))
  }
  
  #Plot of interventions
  p2 <- vistime::gg_vistime(int_data3, col.event = "status_simp",
                            col.group = "intervention_label",
                            show_labels = FALSE, linewidth = 6) +
    ggplot2::scale_color_identity(name = "",
                                  breaks = c("red", "darkorange", "grey"),
                                  labels = c("Strongly Implemented",
                                             "Partially Implemented",
                                             "Implementation Suspended"),
                                  guide = "legend") +
    ggplot2::theme(legend.position = "bottom",
                   legend.margin=ggplot2::margin(0,0,0,0),
                   legend.box.margin=ggplot2::margin(-10,-10,-5,-10)) +
    ggplot2::xlim(as.POSIXct(c(first_date - 1, last_date + 1)))
  
  #Combining and using egg package to align axes
  p <- egg::ggarrange(p1, p2, heights = c(0.7, 0.3), draw = FALSE)
  
  return(p)
}


#Function to create groups of consecutive updates with the same status
sameStatus <- function(intdf){
  
  #Setting first row values
  intdf[1, "interval"] <- 1
  intdf[1, "start"] <- intdf[1, "date_of_update"]
  
  #Initializing values for loop
  currentInterval <- intdf[1,]
  interval <- 1
  start <- intdf[1, "date_of_update"]
  
  if(nrow(intdf) == 1){
    intdf[1, "start"] <- intdf[1, "date_of_update"]
    intdf[1, "end"] <- intdf[1, "date_of_update"]
  }
  
  if (nrow(intdf) > 1) {
    for (i in 2:nrow(intdf)){  
      if (intdf[i, "status_simp"] != intdf[i, "lag_status"]){ # if status different from previous status (lag)
        interval <- interval + 1 # interval represent the interval between two policy updates
        start <- intdf[i, "date_of_update"] # define the new start of the new interval
        currentVisit <- intdf[i, ]
      }
      intdf[i, "interval"] <- interval
      intdf[i, "start"] <- start
    }
  }  
  return(intdf)
}

