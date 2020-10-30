# library(dplyr)
# library(ggplot2)
# library(covidregionaldata)
# library(zoo)
# library(gridExtra)
# library(vistime)
# library(purrr)
# 
# epi_curve <- function(){
#   #Cases only plot
#   case_counts <- covidregionaldata::get_regional_data(country = "USA")
#   case_counts <- case_counts %>%
#     filter(cases_total > 0, iso_3166_2 == "US-NJ") %>%
#     mutate(cases_smooth = zoo::rollmean(cases_new, 7, fill = NA))
#   
#   first_date <- min(case_counts$date)
#   last_date <- max(case_counts$date)
#   
#   
#   #Summarizing intervention data
#   #Line 1: Household confinement
#   #Line 2: Retail stores
#   #Line 3: Leisure and entertainment
#   #Line 4: Restaurants
#   #Line 5: Primary schools
#   
#   #Function to label groups of the same intervention status
#   sameStatus <- function(intdf){
#     
#     #Setting first row values
#     intdf[1, "interval"] <- 1
#     intdf[1, "start"] <- intdf[1, "date_of_update"]
#     
#     #Initializing values for loop
#     currentInterval <- intdf[1,]
#     interval <- 1
#     start <- intdf[1, "date_of_update"]
#     for (i in 2:nrow(intdf)){
#       if (intdf[i, "status_simp"] != intdf[i, "lag_status"]){
#         interval <- interval + 1
#         start <- intdf[i, "date_of_update"]
#         currentVisit <- intdf[i, ]
#       }
#       intdf[i, "interval"] <- interval
#       intdf[i, "start"] <- start
#     }
#     return(intdf)
#   }
#   
#   
#   hit_data <- hit_pull(add_first_case = FALSE)
#   int_data <- hit_filter(hit_data, admin1 = "USA.31_1",
#                          intervention_group = c("household_confined", "entertainment_closed",
#                                                 "store_closed", "restaurant_closed", "school_closed"))
#   
#   #Creating intervention labels with line breaks
#   int_types <- cbind.data.frame("intervention_name" = c("Household confinement",
#                                                         "Leisure and entertainment venue closures",
#                                                         "Primary school closures" ,
#                                                         "Restaurant closures (excluding takeout/delivery)",
#                                                         "Retail store closures (excluding essentials)"),
#                                 "intervention_label" = c("Household confinement",
#                                                          "Leisure and entertainment\nvenue closures",
#                                                          "Primary school closures",
#                                                          "Restaurant closures\n(excluding takeout/delivery)",
#                                                          "Retail store closures\n(excluding essentials)"))
#   
#   int_data <- int_data %>%
#     filter(!intervention_name %in% c("Nursery school closures", "Secondary school closures",
#                                      "Post-secondary school closures")) %>%
#     select(intervention_group, intervention_name, status, status_simp, date_of_update) %>%
#     arrange(intervention_group, date_of_update) %>%
#     group_by(intervention_group) %>%
#     mutate(lag_status = lag(status_simp)) %>%
#     full_join(int_types, by = "intervention_name")
#   
#   int_data2 <- int_data %>%
#     split(int_data$intervention_name) %>%
#     map_dfr(sameStatus) %>%
#     group_by(intervention_name) %>%
#     mutate(end = lead(start)) %>%
#     mutate(end = ifelse(is.na(end), as.character(last_date), end)) %>%
#     group_by(intervention_name, interval) %>%
#     slice_max(end) %>%
#     mutate(start = as.Date(start),
#            end = as.Date(end),
#            color = ifelse(status_simp == "Strongly Implemented", "red",
#                           ifelse(status_simp == "Partially Implemented", "darkorange", "grey")),
#            label = "")
#   
#   
#   #Plot of case counts
#   p1 <- ggplot(data = case_counts) +
#     geom_bar(aes(x = date, y = cases_new), stat = "identity") +
#     geom_line(aes(x = date, y = cases_smooth), color = "darkblue", size = 1.3, na.rm = TRUE) +
#     labs(x = "Date", y = "Number of new cases") +
#     xlim(c(first_date - 1, last_date + 1)) +
#     theme_bw()
#   
#   #Plot of interventions
#   p2 <- vistime::gg_vistime(int_data2, col.event = "status_simp",
#                             col.group = "intervention_label",
#                             show_labels = FALSE, linewidth = 6) +
#     scale_color_identity(name = "",
#                          breaks = c("red", "darkorange", "grey"),
#                          labels = c("Strongly Implemented",
#                                     "Partially Implemented",
#                                     "Implementation Suspended"),
#                          guide = "legend") +
#     theme(legend.position = "bottom",
#           legend.margin=margin(0,0,0,0),
#           legend.box.margin=margin(-10,-10,-5,-10)) +
#     xlim(as.POSIXct(c(first_date - 1, last_date + 1)))
#   
#   #Need to figure out how to line up axes.
#   
#   lay <- rbind(c(NA, NA, rep(1, 14)),
#                c(NA, NA, rep(1, 14)),
#                rep(2, 16))
#   
#   grid.arrange(p1, p2, layout_matrix = lay)
#   
# }

