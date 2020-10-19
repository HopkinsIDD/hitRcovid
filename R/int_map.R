
## UNDER DEVELOPMENT ##


## SVL - Todo:
## 1. Remove dplyr and rewrite in base R (so no pipes, etc.) I know this is annoying but when you use
## a package, the fact that dplyr does not put column names in quotes causes issues.
## 2. Add documentation (see structure below and the hit_filter() function)
## 3. Add examples (see structure below and the hit_filter() function)


## SVL - documentation template

#' Sentence title
#' 
#' Paragraph description (comes before arguments in output)
#' 
#' Multiple paragraphs of details as necessary to explain arguments and functioning in more detail
#' (comes after arguments in output)
#' 
#' @param argument_name description of the argument
#' 
#' @return 
#' What the function returns (here a plot)
#' 
#' @examples 
#' Add examples of usage showing what different options do
#' 
#' @seealso \link{hit_filter}
#' 
#' @export




## SVL - I added maps to the package dependencies, but it is better to avoid the tidyverse
## SVL - I also removed maps because it appears you are not using it. If you need to use other packages,
## use package:: to refer to it. Then run use_package("packagename") to add it to the dependencies.


## SVL - remove these comments at some point

### int_map is the function of generating map for each intervention at a given time
###################################################################################
### currently just making static map, may add hover feature if needed 

## SVL - changed name to be consistent

intervention_map <- function(hit_data,
                    time_point = Sys.Date(), 
                    intervention_group = NULL, ## SVL - changed to match name in other functions
                    date.format = "%m/%d/%Y") {
  
  # Error handling-----------------------------------------------------------------
  
  ## SVL - Changed warning() to stop() to give an error
  
  # Determine if the intervention_group entered is valid, if not, return warning
  if(!intervention_group %in% intervention_lookup$intervention_group){
    stop("The intervention code entered here is not valid.")
  }
  
  # Determine if the time_point entered is valid, if not, return warning
  time_point <- as.Date(time_point, date.format)  
  if(is.na(time_point)) {
    stop('Time point entered here is not valid, please enter valid time in "%m/%d/%y" format.')
  } else if(time_point < as.Date("1/1/2020", date.format) |
            time_point > Sys.Date()) {
    stop("Time point entered here is not valid, please enter valid time between 1/1/2020 and present.")
  } 
  
  ## SVL - I removed pulling the data from inside the function to make it faster - assume someone
  ## has loaded it before using it and add as an argument.
  
  # Prepare data for visualization ----------------------------------------------------------------
  
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
  hit_data <- hit_filter(hit_data, intervention_group = intervention_group, usa_county_data = FALSE)
  
  # Prepare the dates
  hit_data$date_of_update <- as.Date(as.character(hit_data$date_of_update, format = "%y-%m-%d"))
  
  # order the status_simp levels
  hit_data$status_simp <- ordered(hit_data$status_simp, 
                                  levels = c("Implementation Suspended",
                                             "Partially Implemented",
                                             "Strongly Implemented"))
  
  ## SVL - I think if we are plotting national data, we should just use national data (not admin1)
  ## Thoughts? If so you could add include_admin1 = FALSE to the hit_filter() statement
  
  # Get latest & strongest implementation status at country or admin1 level
  hit_data$admin1 <- as.character(hit_data$admin1)
  recent_data <- hit_data %>%
    filter(date_of_update <= time_point) %>%
    group_by(country_name, admin1) %>%
    filter(date_of_update == max(date_of_update)) %>%
    group_by(country_name, admin1, date_of_update) %>%
    summarize(recent_status = max(status_simp))
  
  
  # Mapping of country level data -----------------------------------------------------------------
  
  # load the world map (remove Antarctica) and merge with country level data
  world <- ggplot2::map_data("world")
  world <- world[which(!world$region=='Antarctica'),]  
  country_map <- recent_data %>% filter(is.na(admin1))
  country_map <- left_join(world, country_map, world, by = c("region" = "country_name"))
  
  # create country level data legend
  country_map$recent_status <- as.character(country_map$recent_status)
  country_map[which(is.na(country_map$recent_status)), ]$recent_status <- "No data"
  country_map$recent_status <- ordered(country_map$recent_status, 
                                       levels = c("Strongly Implemented", 
                                                  "Partially Implemented", 
                                                  "Implementation Suspended", 
                                                  "No data"))
  
  # set the legend colors (considering some recent_status may have 0 record)
  legend <- as.data.frame(table(country_map$recent_status))
  legend$country_colors = c("lightpink2", "khaki2", "aquamarine2", "gray93")
  country_mapping_colors <- legend[which(legend$Freq > 0), ]$country_colors
  
  ## SVL - When writing a package you don't want to save things because that changes the users device
  ## instead print things then users can save.
  
  # draw country level intervention map with ggplot
   p <- ggplot2::ggplot(data = country_map, 
         ggplot2::aes(x = long, y = lat, group= group, fill=recent_status)) +
    ggplot2::scale_fill_manual(values = country_mapping_colors) +
    ggplot2::geom_polygon(colour='black')+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white")
          ,plot.background = ggplot2::element_rect(fill = "white")
          ,panel.grid = ggplot2::element_blank()
          ,axis.text = ggplot2::element_blank()
          ,axis.title = ggplot2::element_blank())+
    ggplot2::labs(fill = "Intervention intensity")+
    ggplot2::theme(legend.title.align = 0.5)+
    ggplot2::theme(legend.text = ggplot2::element_text(size=12),
          legend.title = ggplot2::element_text(size=14))
  
  # Mapping of admin1 level data ------------------------------------------------------------------
 

  
  
   print(p)
}

## SVL - Removed examples and moved to "Package_Testing.R"


