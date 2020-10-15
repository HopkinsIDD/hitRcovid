

## UNDER DEVELOPMENT ##

intervention_timeline <- function(hit_data,
                                  continent = NULL,
                                  country = NULL,
                                  admin1 = NULL,
                                  locality = NULL,
                                  intervention_group = NULL,
                                  include_national = TRUE,
                                  include_admin1 = TRUE,
                                  include_locality = FALSE,
                                  usa_county_data = FALSE,
                                  facet_by = c("none", "continent", "country", "admin1"),
                                  first_case_line = TRUE,
                                  first_death_line = TRUE){
  
  
  ## Error handling -------------------------------------------------------------------------------
  
  #Errors and warnings if too many facets
  if(facet_by == "country" & (is.null(country) | length(country) > 10)){
    stop("The plot will not render correctly if you specify more than 10 facets")
  }else if(facet_by == "admin1" & (is.null(admin1) | length(admin1) > 10)){
    stop("The plot will not render correctly if you specify more than 10 facets")
  }
  
  if(facet_by == "country" & (is.null(country) | length(country) > 5)){
    warning("The plot many be too condensed if you specify more than 5 facets")
  }else if(facet_by == "admin1" & (is.null(admin1) | length(admin1) > 5)){
    warning("The plot many be too condensed if you specify more than 5 facets")
  }else if(facet_by == "continent" & (is.null(continent) | length(continent) > 5)){
    warning("The plot many be too condensed if you specify more than 5 facets")
  }
  
  
  
  ## Formatting dataset ---------------------------------------------------------------------------
  
  #Creating intervention labels with line breaks
  int_types <- cbind.data.frame("names" = c("Restrictions of travel and movement",
                                            "Social and physical distancing measures",
                                            "Surveillance and response measures" ,
                                            "Other measures"),
                                "labels" = c("Restrictions\nof travel and\nmovement",
                                             "Social and\nphysical\ndistancing\nmeasures",
                                             "Surveillance\nand response\nmeasures",
                                             "Other \nmeasures"))
  
  #Using hit_filter to filter the dataset as specified
  data <- hit_filter(hit_data,
                     continent = continent,
                     country = country,
                     admin1 = admin1,
                     locality = locality,
                     intervention_group = intervention_group,
                     include_national = include_national,
                     include_admin1 = include_admin1,
                     include_locality = include_locality,
                     usa_county_data = usa_county_data,
                     remove_columns = FALSE)
  
  #Making sure date is a date variable
  data$date_of_update <- as.Date(as.character(data$date_of_update))
  
  #Merging in intervention names and type
  data <- merge(data, intervention_lookup[, !names(intervention_lookup) %in% "unique_id"],
                by = c("intervention_group", "intervention_name"))
  
  #Making status_simp into a factor
  data$status_simp <- factor(data$status_simp,
                             levels = c("Strongly Implemented",
                                        "Partially Implemented",
                                        "Implementation Suspended"))
  
  #Making intervention_type into a factor
  int_new <- int_types[int_types$names %in% unique(data$intervention_type), ]
  data$intervention_type <- factor(data$intervention_type,
                                   levels = int_new$names,
                                   labels = int_new$labels)
  
  #Creating a geographic level variable
  data$level <- factor(is.na(data$admin1), levels = c(FALSE, TRUE),
                       labels = c("Sub-national", "National"))
  
  #If include_national = TRUE and facet_by = admin1
  #Then include copies of national data under each admin1 unit so that data shows up in each facet
  if(include_national == TRUE & facet_by == "admin1"){
    
    #Extracting national data
    national <- data[data$level == "National",  ]
    
    #Finding admin1 codes and names (make sure order matches)
    admin1 <- admin1[order(admin1)]
    data <- data[order(data$admin1), ]
    admin1_names <- unique(data[!is.na(data$admin1), "admin1_name"])
    
    #Removing the old national data from data
    data <- data[!is.na(data$admin1), ]
    
    #Adding copies of national data for each admin1 as if it was in that admin1 unit
    for(i in 1:length(admin1)){
      national_temp <- national
      national_temp$admin1 <- admin1[i]
      national_temp$admin1_name <- admin1_names[i]
      data <- rbind(data, national_temp)
    }
  }
  
  #If facet_by = continent, add continent data (note this will duplicate data from countries in Eurasia)
  if(facet_by == "continent"){
    continent_data <- unique(geo_lookup[geo_lookup$continent %in% continent,
                                        c("continent", "country")])
    data <- merge(data, continent_data)
  }
  
  
  ## Setting plotting paramters -------------------------------------------------------------------
  
  #Determining facet variable
  if(facet_by == "country"){
    facet_var <- "country_name"
  }else if(facet_by == "admin1"){
      facet_var <- "admin1_name"
  }else{
      facet_var <- facet_by
  }
  
  #Find first case for continent/country/admin unit
  if(first_case_line == TRUE | first_death_line == TRUE){
   
    #If it is not included in data, add information
    if(!"first_case" %in% names(data)){
      firsts <- get_first_case()
      data <- merge(data, firsts, all.x = TRUE)
    }
    
    #Finding first case and first death by facet variable
    if(facet_var != "none"){
      first_case <- data[order(data[, facet_var], data[, "first_case"]),
                         c(facet_var, "first_case")]
      first_case <- first_case[!duplicated(first_case[, facet_var]), ]
      
      first_death <- data[order(data[, facet_var], data[, "first_death"]),
                          c(facet_var, "first_death")]
      first_death <- first_death[!duplicated(first_death[, facet_var]), ]
      
      firsts <- merge(first_case, first_death, by = facet_var)
    }else{
      first_case <- min(data$first_case, na.rm = TRUE)
      first_death <- min(data$first_death, na.rm = TRUE)
      
      firsts <- cbind.data.frame("first_case" = first_case, "first_death" = first_death)
    }
  }
  
  #Setting upper bound of plot to be two weeks into the future
  end_date <- Sys.Date() + 14

    
  
  ## Creating plot --------------------------------------------------------------------------------
  
  #Create ggplot
  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes(x = data[, "date_of_update"],
                                    y = data[, "intervention_group_name"]))
  
  #Drawing points including national if specified
  if(include_national == TRUE & include_admin1 == TRUE){
    p <- p + ggplot2::geom_jitter(ggplot2::aes(col = data[, "status_simp"],
                                               shape = data[, "level"]), 
                    alpha=0.3, size=2, width=0, height=0.2)
  }else{
    p <- p + ggplot2::geom_jitter(ggplot2::aes(col = data[, "status_simp"]), 
                    alpha=0.3, size=2, width=0, height=0.2)
  }
  
  #Faceting by provided level
  if(facet_by != "none"){
    p <- p + ggplot2::facet_grid(data$intervention_type ~ data[, facet_var],
                                 scales = "free", space="free")
  }
  
  #FACETTING ISN'T WORKING HERE, LIKELY NEED TO COPY firsts FOR EACH INTERVENTION TYPE
  
  #Adding line for date of first case and death
  if(first_case_line == TRUE){
    p <- p + ggplot2::geom_vline(data = firsts, ggplot2::aes(xintercept = firsts[, "first_case"]),
                                 lty = 2, size = 1)
  }
  if(first_death_line == TRUE){
    p <- p + ggplot2::geom_vline(data = firsts, ggplot2::aes(xintercept = firsts[, "first_death"]),
                                 lty = 3, size = 1)
  }
   
  #Formatting legend, labels, axes, and colors 
  p <- p + ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   legend.position="bottom",
                   legend.box="vertical",
                   legend.margin=ggplot2::margin()) +
    
    ggplot2::labs(x = "Date of Implementation",
                  y = "Intervention \nType",
                  shape = "Geographic Level") +
    
    ggplot2::scale_color_manual(name="Status", values = c("red","darkorange","black")) +
    
    ggplot2::scale_x_date(date_labels="%b", limits=as.Date(c("2020-01-01", end_date)))
  
  #Printing plot
  print(p)
}
  