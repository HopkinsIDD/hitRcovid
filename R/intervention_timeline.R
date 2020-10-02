

intervention_timeline <- function(hit_data, continent = NULL, country = NULL, admin1 = NULL, locality = NULL,
                                  intervention_group = NULL, include_national = TRUE, include_admin1 = TRUE,
                                  facet_by = c("continent", "country", "admin1"),...){
  
  if(facet_by == "country" & (is.null(country) | length(country) > 5)){
    stop("The plot will not render correctly if you specify more than 5 facets")
  }else if(facet_by == "admin1" & (is.null(admin1) | length(admin1) > 5)){
    stop("The plot will not render correctly if you specify more than 5 facets")
  }else if(facet_by == "continent" & (is.null(continent) | length(continent) > 5)){
    stop("The plot will not render correctly if you specify more than 5 facets")
  }
  
  int_types <- cbind.data.frame("names" = c("Restrictions of travel and movement",
                                            "Social and physical distancing measures",
                                            "Surveillance and response measures" ,
                                            "Other measures"),
                                "labels" = c("Restrictions\nof travel and\nmovement",
                                             "Social and\nphysical\ndistancing\nmeasures",
                                             "Surveillance\nand response\nmeasures",
                                             "Other \nmeasures"))
  
  #Using hit_filter to filter the dataset as specified
  data <- hit_filter(hit_data, continent = continent, country = country,
                     admin1 = admin1, locality = locality,
                     intervention_group = intervention_group)
  
  #Making sure date is a date variable
  data$date_of_update <- as.Date(as.character(data$date_of_update))
  
  #Merging in intervention names and type
  data <- merge(data, intervention_lookup,
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
  data$`Geographic Level` <- factor(is.na(data$admin1), levels = c(FALSE, TRUE),
                                    labels = c("Sub-national","National"))
  
  #Determining facet variable
  if(facet_by == "country"){
    facet_var <- "country_name"
  }else if(facet_by == "admin1"){
      facet_var <- "admin1_name"
  }else{
      facet_var <- facet_by
    }
  
  #TODO
  #Find first case for continent/country/admin unit
  
  #Subsetting to just sub-national if include_national == FALSE
  if(include_national == FALSE){
    data <- data[!is.na(data$admin1), ]
  }else if(include_admin1 == FALSE){
    data <- data[is.na(data$admin1)]
  }
  
  #Create ggplot
  p <- ggplot(data = data, aes(x=date_of_update, y=intervention_group_name))
  
  if(include_national == TRUE & include_admin1 == TRUE){
    p <- p + geom_jitter(aes(col = status_simp, shape=`Geographic Level`), 
                    alpha=0.2, size=2, width=0, height=0.3)
  }else{
    p <- p + geom_jitter(aes(col = status_simp), 
                    alpha=0.15, size=2, width=0, height=0.3)
  }
  
  p <- p + facet_grid(intervention_type ~ data[, facet_by],
               scales = "free", space="free") +
    
    theme_bw() +
    theme(strip.text.y = element_text(#size = 5,
      angle = 0 ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position="bottom",
      legend.box="vertical",
      legend.margin=margin())+
    
    ylab("Intervention \nType")+
    xlab("Date of Implementation")+
    scale_color_manual(name="Status",values = c("red","darkorange","black"))+
    
    #geom_vline(data=firstcase, aes(xintercept = date_of_update), lty=2) +
    
    scale_x_date(
      breaks = as.Date(c("2020-01-01",
                         "2020-02-01",
                         "2020-03-01",
                         "2020-04-01",
                         "2020-05-01",
                         "2020-06-01",
                         "2020-07-01",
                         "2020-08-01",
                         "2020-09-01",
                         "2020-10-01")),
      date_labels="%b", limits=as.Date(c("2020-01-01","2020-10-01")))
  
  #Printing plot
  print(p)
}
  