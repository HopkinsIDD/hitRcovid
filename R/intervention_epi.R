
## UNDER DEVELOPMENT ##

#hit_data <- hit_pull(add_first_case = FALSE)


## TODO: Add admin1 data where available

intervention_epi <- function(hit_data,
                      country,
                      source = c("WHO", "ECDC"),
                      case_threshold = 0,
                      first_date = NULL,
                      last_date = NULL,
                      date_format = "%m/%d/%Y"){
  
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
  
  # Determine if the dates entered are valid, if not, return warning
  if(!is.null(first_date)){
    first_date <- as.Date(time_point, date.format)  
    if(is.na(first_date)) {
      stop('Date entered here is not valid, please enter a valid date in right format.')
    } else if(first_date < as.Date("1/1/2020", date.format) |
              first_date > Sys.Date()) {
      stop("Date entered here is not valid, please enter valid date between 1/1/2020 and present.")
    } 
  }
  if(!is.null(last_date)){
    last_date <- as.Date(time_point, date.format)  
    if(is.na(last_date)) {
      stop('Date entered here is not valid, please enter a valid date in right format.')
    } else if(last_date < as.Date("1/1/2020", date.format) |
              last_date > Sys.Date()) {
      stop("Date entered here is not valid, please enter valid date between 1/1/2020 and present.")
    } 
  }


  
  #### Setting up plotting data -------------------------------------------------------------------
  
  #Getting all case counts
  case_counts <- covidregionaldata::get_national_data(source = source)
  
  #Finding iso_code (2 letter) that goes with the country specified
  country_code <- unique(geo_lookup[geo_lookup$country == country & !is.na(geo_lookup$country),
                                    "alpha_2"])
  
  #Filtering to country, restricting to more cases than specified threshold and adding 7-day average
  case_counts <- case_counts %>%
    filter(cases_total > case_threshold, iso_code == country_code) %>%
    mutate(cases_smooth = zoo::rollmean(cases_new, 7, fill = NA))
  
  #Finding the min and max date (if first_date and last_date were not user-specified)
  if(is.null(first_date)){first_date <- min(case_counts$date)}
  if(is.null(last_date)){last_date <- max(case_counts$date)}
  if(last_date <= first_date){
    stop("first_date must be earlier than last_date")
  }

  
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

  #Filtering the HIT-COVID database and selecting interventions
  ## TODO: Allow user to select interventions
  int_data <- hit_filter(hit_data, country = country, include_admin1 = FALSE,
                         intervention_group = c("household_confined", "closed_border", "mask",
                                                "store_closed", "restaurant_closed", "school_closed"))
  
  #Subsetting to just primary school closures for schools
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
  # then set the 'start' to be first_date, int_data3 this bar will not be plotted
  int_data3[int_data3$start <= first_date & int_data3$end >= first_date,]$start <- first_date
  #If 'start' == 'end', then delete this row, otherwise there will be a dot on the plot
  int_data3 <- int_data3[int_data3$start < int_data3$end, ]
  
  #Setting plotting parameters
  int_data3$color <- ifelse(int_data3$status_simp == "Strongly Implemented", "red",
                            ifelse(int_data3$status_simp == "Partially Implemented", "darkorange",
                                   "grey"))
    
  
  
  #### Making plots -------------------------------------------------------------------------------
  
  #Plot of case counts
  p1 <- ggplot2::ggplot(data = case_counts) +
    ggplot2::geom_bar(ggplot2::aes(x = date, y = cases_new), stat = "identity") +
    ggplot2::geom_line(ggplot2::aes(x = date, y = cases_smooth), color = "darkblue", size = 1.3, na.rm = TRUE) +
    ggplot2::labs(y = "Number of new cases") +
    ggplot2::xlim(c(first_date - 1, last_date + 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                   axis.title.x = ggplot2::element_blank())
  
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
  p <- egg::ggarrange(p1, p2, heights = c(0.7, 0.3))
  
  return(p)
}


#Function to create groups of consecutive updates with the same status
#TODO: let you use status_simp or a collapsed version (implemented/not)
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

