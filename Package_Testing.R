

## Set directory to hitRcovid
#setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(devtools)
library(roxygen2)
library(dplyr)

#Loading package
load_all()

#Updating documentation
document()

#Looking at documentation
?hit_pull
?hit_filter
?intervention_map

#Running tests
#test()

#Running check
#check()

#Testing map function
# temporary testing (country level map)
hit_data <- hit_pull(add_first_case = FALSE)
intervention_map(hit_data, intervention_group = "quar_iso", time_point = "3/24/2020")
# wrong intervention name
intervention_map(hit_data, intervention_group = "quar_", time_point = "3/24/2020")
# wrong date
intervention_map(intervention_group = "quar_iso", time_point = "3/24/2050")


#Testing timeline function

hit1 <- hit_pull(add_first_case = FALSE)
intervention_timeline(hit1, country = "USA")

hit <- hit_pull()

intervention_timeline(hit, country = "USA")
intervention_timeline(hit, country = "USA", include_admin1 = FALSE)
intervention_timeline(hit, country = "USA", include_national = FALSE)
intervention_timeline(hit, country = "USA", first_case_line = FALSE)
intervention_timeline(hit, country = "USA", first_death_line = FALSE)
intervention_timeline(hit, country = "USA", first_case_line = FALSE, first_death_line = FALSE)

intervention_timeline(hit, country = c("IND", "USA"), facet_by = "country")

intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                      facet_by = "continent")

intervention_timeline(hit, country = c("IND", "USA"),
                      intervention_group = c("closed_border", "school_closed", "household_confined"),
                      facet_by = "country")

intervention_timeline(hit, admin1 = c("USA.1_1", "USA.2_1", "USA.4_1"),
                      facet_by = "admin1", include_national = TRUE)

#Errors/warnings/notes
intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                      facet_by = "country")

intervention_timeline(hit, country = "USA", facet_by = "admin1")

intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                      facet_by = "garbage")

intervention_timeline(hit, facet_by = "continent", include_admin1 = FALSE)
intervention_timeline(hit, facet_by = "continent", include_admin1 = FALSE, verbose = FALSE)


