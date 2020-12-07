
#' Mapping of interventions 
#' 
#' This function maps the interventions at both national and administrative level at a specific time.
#' The HIT-COVID dataset is loaded with \link{hit_pull}, and data of one specific intervention
#' is pulled using \link{hit_filter}. 
#' 
#' The function aims to draw a world map depicting the latest interventions before this \code{time_point}.
#' If no value is entered for \code{time_point}, it will return a map of most recent interventions.
#' Both national level and administrative level data are mapped on the same plot. 
#' National level data are represented by the color of country territories, and administrative level data are 
#' represented by dots, both types of data are coded in red (Strongly implemented),
#' yellow (partially implemented) or green (implementation suspended). The strongest implementation 
#' intensity is used for mapping if for one territory there are multiple entries in the same intervention group on the same day. 
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param time_point character string indicating the desired mapping date (default is Sys.Date())
#' @param intervention_group vector of intervention group to filter the data to 
#' (see \link{intervention_lookup} column "intervention_group" or run \link{list_interventions} for options)
#' @param date.format character string indicating the desired format of date. (default is "%m/%d/%Y")
#' 
#' @return 
#' A world map depicting interventions of countries and administrative divisions by a specific date.
#' 
#' @examples
#' 
#' # Puling the HIT-COVID database
#' hit_data <- hit_pull(add_first_case = FALSE)
#'  
#' # Mapping of most recent interventions in "quarantine and isolation" domain
#' intervention_map(hit_data, intervention_group = "quar_iso")
#' 
#' # Mapping of latest "quarantine and isolation" interventions before 5/24/2020
#' intervention_map(hit_data, intervention_group = "quar_iso", time_point = "5/24/2020")
#' 
#' @seealso \link{hit_filter}
#' 
#' @references 
#' https://gadm.org/download_world.html
#' 
#' @importFrom rlang .data
#' 
#' @export

intervention_map <- function(hit_data,
                    time_point = Sys.Date(), 
                    intervention_group = NULL, 
                    date.format = "%m/%d/%Y") {
  
  # Error handling-----------------------------------------------------------------
  
  
  # Determine if the intervention_group entered is valid, if not, return warning
  if(!intervention_group %in% hitRcovid::intervention_lookup$intervention_group){
    stop("The intervention code entered here is not valid.")
  }
  
  # Determine if the time_point entered is valid, if not, return warning
  time_point <- as.Date(time_point, date.format)  
  if(is.na(time_point)) {
    stop('Time point entered here is not valid, please enter a valid date in right format.')
  } else if(time_point < as.Date("1/1/2020", date.format) |
            time_point > Sys.Date()) {
    stop("Time point entered here is not valid, please enter valid date between 1/1/2020 and present.")
  } 
  
  
  # update country names
  hit_data$country_name = as.character(hit_data$country_name)
  hit_data[which(hit_data$country_name=='Hong Kong'),]$country_name ='China'
  hit_data[which(hit_data$country_name=='Korea Republic of'),]$country_name='South Korea'
  hit_data[which(hit_data$country_name=='Bolivia Plurinational State of'),]$country_name='Bolivia'
  hit_data[which(hit_data$country_name=='Congo Democratic Republic of the'),]$country_name='Democratic Republic of the Congo'
  hit_data[which(hit_data$country_name=='Cote dIvoire'),]$country_name='Ivory Coast'
  hit_data[which(hit_data$country_name=='Czechia'),]$country_name='Czech Republic'
  hit_data[which(hit_data$country_name=='Iran Islamic Republic of'),]$country_name='Iran'
  hit_data[which(hit_data$country_name=='Lao Peoples Democratic Republic'),]$country_name='Laos'
  hit_data[which(hit_data$country_name=='Russian Federation'),]$country_name='Russia'
  hit_data[which(hit_data$country_name=='Saint Vincent and the Grenadines'),]$country_name='Saint Vincent'
  hit_data[which(hit_data$country_name=='South Georgia and the South Sandwich Islands'),]$country_name='South Georgia'
  hit_data[which(hit_data$country_name=='Tanzania United Republic of'),]$country_name='Tanzania'
  hit_data[which(hit_data$country_name=='United Kingdom of Great Britain and Northern Ireland'),]$country_name='UK'
  hit_data[which(hit_data$country_name=='United States of America'),]$country_name='USA'
  hit_data[which(hit_data$country_name=='Venezuela Bolivarian Republic of'),]$country_name='Venezuela'
  hit_data[which(hit_data$country_name=='Virgin Islands US'),]$country_name='Virgin Islands'
  hit_data[which(hit_data$country_name=='GuineaBissau'),]$country_name='Guinea-Bissau'
  hit_data[which(hit_data$country_name=='Eswatini'),]$country_name='Swaziland'
  hit_data[which(hit_data$country_name=='Congo'),]$country_name='Republic of Congo'
  
  # subset hit_data by intervention_group
  hit_data <- hit_filter(hit_data, intervention_group = intervention_group, usa_county_data = "exclude")
  
  # Prepare the dates
  hit_data$date_of_update <- as.Date(as.character(hit_data$date_of_update, format = "%y-%m-%d"))
  
  # order the status_simp levels
  hit_data$status_simp <- ordered(hit_data$status_simp, 
                                  levels = c("Implementation Suspended",
                                             "Partially Implemented",
                                             "Strongly Implemented"))
  
  # Get latest & strongest implementation status at country or admin1 level
  recent_data <- dplyr::filter(hit_data, .data$date_of_update <= time_point)
  recent_data <- dplyr::group_by(recent_data, .data$country, .data$country_name, .data$admin1)
  recent_data <- dplyr::filter(recent_data, .data$date_of_update == max(.data$date_of_update))
  recent_data <- dplyr::group_by(recent_data, .data$country, .data$country_name, .data$admin1, .data$date_of_update)
  recent_data <- dplyr::summarise(recent_data, recent_status = max(.data$status_simp))
  
  # country level data prep -----------------------------------------------------
  # load the world map (remove Antarctica) and merge with country level data
  world <- ggplot2::map_data("world")
  world <- world[which(!world$region=='Antarctica'),]  
  country_map <- recent_data[is.na(recent_data$admin1),]
  country_map <- dplyr::left_join(world, country_map, world, by = c("region" = "country_name"))
  
  
  # admin level data prep -------------------------------------------------------
  admin_location <- hitRcovid::admin_location
  admin_location <- dplyr::left_join(hitRcovid::geo_lookup, admin_location,
                                     by = c("admin1_name" = "admin_name", "country" = "ISO"))
  admin_location <- admin_location[, c("country", "country_name", "admin1_name", "long", "lat",
                                       "admin1", "continent")]

   # merge and delete duplicate (some country are counted in >1 continents in geo_lookup)
  admin_map <- recent_data[!is.na(recent_data$admin1),]
  admin_map <- dplyr::left_join(admin_map, admin_location, by = c("admin1", "country"))
  admin_map <- dplyr::summarise(dplyr::group_by(admin_map, 
                         .data$country, .data$country_name.x, .data$admin1, .data$admin1_name, 
                         .data$date_of_update, .data$recent_status, .data$long, .data$lat),
                         continent = max(.data$continent))
   
  # HX: some coordinates (lat and long) of admin1 are missing from gadm_map database,
  #     currently just delete them when mapping, to avoid warning message
  admin_map <- admin_map[which(!is.na(admin_map$long)), ]

  # Setting legends -----------------------------------------------------------------
  # create country level data legend
  country_map$recent_status <- as.character(country_map$recent_status)
  country_map[which(is.na(country_map$recent_status)), ]$recent_status <- "No data"
  country_map$recent_status <- ordered(country_map$recent_status, 
                                        levels = c("Strongly Implemented", 
                                                   "Partially Implemented", 
                                                   "Implementation Suspended", 
                                                   "No data"))
   
  # set color palette and legend for country level data (considering some recent_status may have 0 record)
  country_legend <- as.data.frame(table(country_map$recent_status))
  country_legend$country_colors = c("red", "darkorange", "gray66", "white")
  country_mapping_colors <- country_legend[which(country_legend$Freq > 0), ]$country_colors
   
   
  # set color palette and legend for admin data points
  admin_legend <- as.data.frame(table(admin_map$recent_status))
  admin_legend$admin_colors = c("gray33", "darkorange3", "red3")
  admin_mapping_colors <- admin_legend[which(admin_legend$Freq > 0), ]$admin_colors
   
  # map data of both level ---------------------------------------------------------
  p <- ggplot2::ggplot() +
     ggplot2::geom_polygon(data = country_map, ggplot2::aes(x = .data$long, y = .data$lat,
                                                            group = .data$group, fill = .data$recent_status), 
                  color = "black", size = 0.2, alpha = 0.5) +
     ggplot2::scale_fill_manual(values = country_mapping_colors, 
                                guide = ggplot2::guide_legend(title.position = "top")) +
     ggplot2::geom_point(data = admin_map, ggplot2::aes(x = .data$long, y = .data$lat, col = .data$recent_status),
                alpha = 0.6) +
     ggplot2::scale_color_manual(values = admin_mapping_colors) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")
                    ,plot.background = ggplot2::element_rect(fill = "white")
                    ,panel.grid = ggplot2::element_blank()
                    ,axis.text = ggplot2::element_blank()
                    ,axis.title = ggplot2::element_blank()
                    ,axis.ticks = ggplot2::element_blank())+
     ggplot2::labs(fill = "Intervention intensity")+
     ggplot2::guides(color = FALSE) +
     ggplot2::theme(legend.title.align = 0.5)+
     ggplot2::theme(legend.text = ggplot2::element_text(size=8),
                    legend.title = ggplot2::element_text(size=10)) +
     ggplot2::coord_fixed(ratio = 1.3) +
     ggplot2::theme(legend.position = "bottom")
   
  print(p)
}


