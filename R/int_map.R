# Load R packages and datasets

library(maps)
library(tidyverse)
library(dplyr)
library(ggplot2)

source("R/hit_filter.R")
intervention_lookup <- read_csv("data/intervention_lookup.csv")
geo_lookup <- read_csv("data/geo_lookup.csv")


### int_map is the function of generating map for each intervention at a given time
###################################################################################
### currently just making static map, may add hover feature if needed 

int_map <- function(time_point = Sys.Date(), 
                    intervention = NULL,
                    date.format = "%m/%d/%Y") {
  
  # Error handling-----------------------------------------------------------------
  
  # Determine if the intervention entered is valid, if not, return warning
  if(!intervention %in% intervention_lookup$intervention_group){
    warning("The intervention code entered here is not valid.")
    return()
  }
  
  # Determine if the time_point entered is valid, if not, return warning
  time_point <- as.Date(time_point, date.format)  
  if(is.na(time_point)) {
    warning('Time point entered here is not valid, please enter valid time in "%m/%d/%y" format.')
    return()
  } else if(time_point < as.Date("1/1/2020", date.format) |
            time_point > Sys.Date()) {
    warning("Time point entered here is not valid, please enter valid time between 1/1/2020 and present.")
    return()
  } 
  
  # -------------------------------------------------------------------------------------
  # pull the hit_data
  hit_data <-hit_pull()
  
  # update country names
  hit_data$country_name=as.character(hit_data$country_name)
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
  hit_data <- hit_filter(hit_data, intervention_group = intervention, # will this cause confusion?
                         usa_county_data = FALSE)
  

  # Prepare data for visualization -----------------------------------------------
  # Prepare the dates
  hit_data$date_of_update <- as.Date(as.character(hit_data$date_of_update, format = "%y-%m-%d"))
  
  # order the status_simp levels
  hit_data$status_simp <- ordered(hit_data$status_simp, 
                                  levels = c("Implementation Suspended",
                                             "Partially Implemented",
                                             "Strongly Implemented"))
  
  # Get latest & strongest implementation status at country or admin1 level
  hit_data$admin1 <- as.character(hit_data$admin1)
  recent_data <- hit_data %>%
    filter(date_of_update <= time_point) %>%
    group_by(country_name, admin1) %>%
    filter(date_of_update == max(date_of_update)) %>%
    group_by(country_name, admin1, date_of_update) %>%
    summarize(recent_status = max(status_simp))
  
  
  # Mapping of country level data --------------------------------------------
  
  # load the world map (remove Antarctica) and merge with country level data
  world <- map_data("world")
  world=world[which(!world$region=='Antarctica'),]  
  country_map <- recent_data %>% filter(is.na(admin1))
  country_map <- left_join(world, country_map, world, by = c("region" = "country_name"))
  
  # create country level data legend
  country_map$recent_status <- as.character(country_map$recent_status)
  country_map[which(is.na(country_map$recent_status)), ]$recent_status = "No data"
  country_map$recent_status <- ordered(country_map$recent_status, 
                                       levels = c("Strongly Implemented", 
                                                  "Partially Implemented", 
                                                  "Implementation Suspended", 
                                                  "No data"))
  
  # set the legend colors (considering some recent_status may have 0 record)
  legend <- as.data.frame(table(country_map$recent_status))
  legend$country_colors = c("lightpink2", "khaki2", "aquamarine2", "gray93")
  country_mapping_colors <- legend[which(legend$Freq > 0), ]$country_colors
  
  # draw country level intervention map with ggplot
  tiff("int_map_country.tiff", units="in", width=16, height=6.5, res=300)
  ggplot(data = country_map, 
         aes(x = long, y = lat, group= group, fill=recent_status)) +
    scale_fill_manual(values = country_mapping_colors) +
    geom_polygon(colour='black')+
    theme(panel.background = element_rect(fill = "white")
          ,plot.background = element_rect(fill = "white")
          ,panel.grid = element_blank()
          ,axis.text = element_blank()
          ,axis.title = element_blank())+
    labs(fill = "Intervention intensity")+
    theme(legend.title.align = 0.5)+
    theme(legend.text=element_text(size=12),
          legend.title = element_text(size=14))
  
  # Mapping of admin1 level data -----------------------------------------------------------
 

  
  
   ggsave("int_map_country.tiff")
  
}

# temporary testing (country level map)
# hit_data <- hit_pull()
# int_map(intervention = "quar_iso", time_point = "3/24/2020")
# wrong intervention name
# int_map(intervention = "quar_", time_point = "3/24/2020")
# wrong date
# int_map(intervention = "quar_iso", time_point = "3/24/2050")


