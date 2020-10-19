

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


