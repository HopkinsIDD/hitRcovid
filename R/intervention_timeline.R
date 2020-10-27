

#' Plots a time-line of the intervention data in HIT-COVID
#' 
#' This function creates a scatter plot of all of the intervention updates in the HIT-COVID 
#' database over time filtered to locations or intervention types as desired. It runs \link{hit_filter}
#' and has many of the same arguments allowing great flexibility in what locations to include with
#' the option to facet the plot by different location levels. If desired, it also draws vertical lines 
#' indicating the date of first case and first death in each facet provided.
#' 
#' The dataset must first be loaded with \link{hit_pull} and fed into this function.
#' See \link{hit_filter} for details about each of the different filtering arguments. If desired the
#' plot can have facets for different location levels - continent, country, admin1 using the
#' \code{facet-by} argument. The default plot will also facet by broad intervention categories.
#' This behavior can be turned off by setting \code{intervention_facet} to FALSE.
#' 
#' Additionally, by default, vertical lines will be drawn indicating the date of the first case and
#' first death using data from the \code{source} specified either within this function or in the
#' original pull of the dataset (if no source is specified the default is the WHO). This behavior
#' can be turned off by setting one or both of \code{first_case_line} and \code{first_death_line} 
#' to FALSE.
#' 
#' 
#' @param hit_data the full HIT-COVID database pulled from GitHub, pulled using \link{hit_pull}
#' @param facet_by the location level for the facet columns (one of "continent", "country", "admin1").
#' If no facets are desired specify \code{facet_by = "none"} which is the default.
#' @param intervention_facet a logical indicating if the different intervention groups should be
#' faceted by their broad type ("Restrictions of travel and movement", "Social and physical distancing
#' measures", "Surveillance and response measures", and "Other measures")
#' @param first_case_line a logical indicating if a vertical line at the date of the first case 
#' should be drawn (default is TRUE).
#' @param first_death_line a logical indicating if a vertical line at the date of the first death
#' should be drawn (defulat is TRUE).
#' @param source the source of the case data that is used to determine the date of first case and
#' first death if \code{hit_data} does not already include that information and either
#' \code{first_case_line} or \code{first_death_line} are set to TRUE (default is "WHO").
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
#' @param usa_county_data character string indicating how to deal with USA county-level data: one
#' of "include", "exclude" or "restrict_to" (default is "exclude").
#' @param verbose a logical indicating if notes about excluded points and lines should be printed 
#' (default is TRUE)
#' 
#' 
#' @examples 
#' 
#' #Pulling HIT-COVID database
#' hit_data <- hit_pull()
#' 
#' #Plotting all national and admin1 data in the database, in North America, Asia, Europe and Africa
#' #faceting by continent
#' intervention_timeline(hit_data, continent = c("Asia", "Europe", "Africa", "North America"),
#' facet_by = "continent")
#' 
#' #Plotting all data from India and New Zealand
#' intervention_timeline(hit_data, country = c("IND", "NZL"), facet_by = "country")
#' 
#' #Plotting all data from the USA
#' intervention_timeline(hit_data, country = "USA")
#' 
#' #Plotting just state-level data from the USA
#' intervention_timeline(hit_data, country = "USA", include_national = FALSE)
#' 
#' #Removing vertical lines
#' intervention_timeline(hit_data, country = "USA",
#' first_case_line = FALSE, first_death_line = FALSE)
#' 
#' 
#' @seealso \link{hit_filter}, \link{get_first_case}, \link[covidregionaldata]{get_national_data}
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


intervention_timeline <- function(hit_data,
                                  facet_by = c("none", "continent", "country", "admin1"),
                                  intervention_facet = TRUE,
                                  first_case_line = TRUE,
                                  first_death_line = TRUE,
                                  source = c("WHO", "ECDC"),
                                  continent = NULL,
                                  country = NULL,
                                  admin1 = NULL,
                                  locality = NULL,
                                  intervention_group = NULL,
                                  include_national = TRUE,
                                  include_admin1 = TRUE,
                                  include_locality = FALSE,
                                  usa_county_data = c("include", "exclude", "restrict_to"),
                                  verbose = TRUE){
  
  ## Filtering data -------------------------------------------------------------------------------
  
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
  
  
  ## Error handling -------------------------------------------------------------------------------
  
  #Setting default facet to none and throwing an error if the facet variable is not valid
  if(all(facet_by == c("none", "continent", "country", "admin1"))){
    facet_by <- "none"
  }else if(length(facet_by) > 1){
    stop("facet_by should be one of 'none', 'continent', 'country', 'admin1'")
  }else if(!facet_by %in% c("none", "continent", "country", "admin1")){
    stop("facet_by should be one of 'none', 'continent', 'country', 'admin1'")
  }
  
  #Errors and warnings if too many facets
  if(facet_by == "country" & length(unique(data$country)) > 20){
    stop("The plot will not draw correctly if you specify more than 20 facets")
  }else if(facet_by == "admin1" & length(unique(data$admin1)) > 20){
    stop("The plot will not draw correctly if you specify more than 20 facets")
  }
  
  
  ## Formatting dataset ---------------------------------------------------------------------------

  #Making sure date is a date variable
  data$date_of_update <- as.Date(as.character(data$date_of_update))

  #Merging in intervention names and type
  data <- merge(data, hitRcovid::intervention_lookup[, !names(hitRcovid::intervention_lookup) %in% "unique_id"],
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
    if(!is.null(continent)){
      continent_data <- unique(hitRcovid::geo_lookup[hitRcovid::geo_lookup$continent %in% continent,
                                          c("continent", "country")]) 
    }else{
      continent_data <- unique(hitRcovid::geo_lookup[, c("continent", "country")])
    }
    data <- merge(data, continent_data)
  }


  ## Setting plotting parameters ------------------------------------------------------------------

  #Determining facet variable
  if(facet_by == "country"){
    data$facet_var <- data$country_name
  }else if(facet_by == "admin1"){
    data$facet_var <- data$admin1_name
  }else if(facet_by != "none"){
    data$facet_var <- data[, facet_by]
  }

  #Find first case for continent/country/admin unit
  if(first_case_line == TRUE | first_death_line == TRUE){

    #If it is not included in data, add information
    if(!"first_case" %in% names(data)){
      firsts <- get_first_case(source = source)
      data <- merge(data, firsts, all.x = TRUE)
    }

    #Finding first case and first death by facet variable
    if(facet_by != "none"){
      first_case <- data[order(data$facet_var, data$first_case),
                         c("facet_var", "first_case")]
      first_case <- first_case[!duplicated(first_case$facet_var), ]

      first_death <- data[order(data$facet_var, data$first_death),
                          c("facet_var", "first_death")]
      first_death <- first_death[!duplicated(first_death$facet_var), ]

      firsts <- merge(first_case, first_death, by = "facet_var")
    }else{
      first_case <- min(data$first_case, na.rm = TRUE)
      first_death <- min(data$first_death, na.rm = TRUE)

      firsts <- cbind.data.frame("first_case" = first_case, "first_death" = first_death)
    }
    firsts <- merge(firsts, int_types$labels)
    #names(firsts)[names(firsts) == "y"] <- "intervention_type"
  }

  #Setting upper bound of plot to be two weeks into the future
  end_date <- as.character(Sys.Date() + 14)



  ## Creating plot --------------------------------------------------------------------------------

  #Create ggplot
  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes(x = .data$date_of_update,
                                    y = .data$intervention_group_name))

  #Drawing points including national if specified
  if(include_national == TRUE & include_admin1 == TRUE){
    p <- p + ggplot2::geom_jitter(ggplot2::aes(col = .data$status_simp, shape = .data$level),
                                  alpha=0.3, size=2, width=0, height=0.2, na.rm = TRUE)
  }else{
    p <- p + ggplot2::geom_jitter(ggplot2::aes(col = .data$status_simp),
                    alpha=0.3, size=2, width=0, height=0.2, na.rm = TRUE)
  }

  #Faceting by provided level
  if(facet_by != "none" & intervention_facet == TRUE){
    p <- p + ggplot2::facet_grid(ggplot2::vars(.data$intervention_type),
                                 ggplot2::vars(.data$facet_var), 
                                 scales = "free_y", space="free")
  }else if(facet_by != "none" & intervention_facet == FALSE){
    p <- p + ggplot2::facet_grid(cols = ggplot2::vars(.data$facet_var),
                                 scales = "free_y", space="free")
  }else if(facet_by == "none" & intervention_facet == TRUE){
    p <- p + ggplot2::facet_grid(ggplot2::vars(.data$intervention_type),
                                 scales = "free_y", space="free")
  }

  #Adding line for date of first case and death
  if(first_case_line == TRUE){
    p <- p + ggplot2::geom_vline(data = firsts,
                                 ggplot2::aes(xintercept = .data$first_case, lty = "dashed",),
                                   size = 1, na.rm = TRUE)
  }
  if(first_death_line == TRUE){
    p <- p + ggplot2::geom_vline(data = firsts,
                                 ggplot2::aes(xintercept = .data$first_death, lty = "dotted"),
                                  size = 1, na.rm = TRUE)
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
    
    ggplot2::scale_linetype_identity(name = "",
                                     breaks = c("dashed", "dotted"), 
                                     labels = c("First case", "First death"),
                                     guide = "legend") +

    ggplot2::scale_x_date(date_labels="%b", limits=as.Date(c("2019-12-25", end_date)))

  #Printing plot
  return(p)
  
  
  #### Noting missing values in plot --------------------------------------------------------------
  if(verbose == TRUE){
    
    note_list <- NULL
    
    #Number of points outside the limits
    limits <- as.Date(c("2019-12-25", end_date))
    n_early <- sum(data$date_of_update < limits[1])
    if(n_early > 0){
      note_list[[1]] <- (paste0(n_early, " point(s) were excluded because they are before ", limits[1], "\n"))
    }
    n_late <- sum(data$date_of_update > limits[2])
    if(n_late > 0){
      note_list[[2]] <- (paste0(n_late, " point(s) were excluded because they are after ", limits[2], "\n"))
    }
    
    #Facets missing first case
    if(first_case_line == TRUE | first_death_line == TRUE){
      
      miss_first_case <- unique(firsts[is.na(firsts$first_case), "facet_var"]) 
      miss_first_death <- unique(firsts[is.na(firsts$first_death), "facet_var"]) 
      
      if(first_case_line == TRUE & length(miss_first_case) > 0){
        note_list[[3]] <- paste0(paste0(miss_first_case, collapse = ", "),
                     " is/are missing data on the date of the first case\n")
      }
      if(first_death_line == TRUE & length(miss_first_death) > 0){
        note_list[[4]] <- paste0(paste0(miss_first_case, collapse = ", "),
                     " is/are missing data on the date of the first death\n")
      }
    }

    #Printing notes
    for(i in 1:length(note_list)){cat(note_list[[i]])}
    
    #Test if there are any notes
    if(!is.null(note_list)){
      cat("\nTo remove this note use verbose = FALSE\n")  
    }
    
  }
}
  