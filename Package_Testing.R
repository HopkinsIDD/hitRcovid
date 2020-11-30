

## Set directory to hitRcovid
setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(devtools)
library(roxygen2)
library(dplyr)
library(pkgdown)

#Loading package
load_all()

#Updating documentation
document()

#Building site
build_site()

#Looking at documentation
?hit_pull
?hit_filter
?intervention_map
?get_interventions

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

intervention_map(hit_data, intervention_group = "school_closed", time_point = "3/24/2020")



#Testing timeline function

hit1 <- hit_pull(add_first_case = FALSE)
intervention_timeline(hit1, country = "USA")

hit <- hit_pull()

intervention_timeline(hit, country = "USA")
intervention_timeline(hit, country = "USA", include_admin1 = FALSE, intervention_facet = FALSE)
intervention_timeline(hit, country = "USA", include_national = FALSE)
intervention_timeline(hit, country = "USA", first_case_line = FALSE)
intervention_timeline(hit, country = "USA", first_death_line = FALSE)
intervention_timeline(hit, country = "USA", first_case_line = FALSE, first_death_line = FALSE)

intervention_timeline(hit, country = c("IND", "USA"), facet_by = "country")

intervention_timeline(hit, continent = c("Africa", "North America", "Europe"),
                      facet_by = "continent")

intervention_timeline(hit, country = c("IND", "USA"),
                      intervention_group = c("closed_border", "school_closed", "household_confined"),
                      facet_by = "country")

intervention_timeline(hit, admin1 = c("USA.22_1", "USA.31_1", "USA.39_1"),
                      facet_by = "admin1", include_national = TRUE)

#Errors/warnings/notes
intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                      facet_by = "country")

intervention_timeline(hit, country = "USA", facet_by = "admin1")

intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                      facet_by = "garbage")

intervention_timeline(hit, facet_by = "continent", include_admin1 = TRUE)

intervention_timeline(hit, facet_by = "continent", include_admin1 = FALSE, verbose = FALSE)


#Testing epi-curve function
hit <- hit_pull(add_first_case = FALSE)

#Country level
intervention_epi(hit, country = "IND")
intervention_epi(hit, country = "IND", case_threshold = 100)
intervention_epi(hit, country = "IND", case_threshold = 100, first_date = "2/1/2020")
intervention_epi(hit, country = "IND", case_threshold = 100, last_date = "9/30/2020")
intervention_epi(hit, country = "IND",first_date = "2/1/2020", last_date = "9/30/2020")
intervention_epi(hit, country = "NZL", first_date = "3/1/2020", last_date = "5/31/2020")

#Error testing
intervention_epi(hit, country = "garbage")
intervention_epi(hit, admin1 = "garbage")
intervention_epi(hit, admin1 = "CHN.1_1")
intervention_epi(hit, country = "SWE")
intervention_epi(hit_data, admin1 = "BEL.1_1")
intervention_epi(hit)

intervention_epi(hit, country = "IND", first_date = "garbage")
intervention_epi(hit, country = "IND", last_date = "garbage")
intervention_epi(hit, country = "IND", first_date = "12/31/2019")
intervention_epi(hit, country = "IND", first_date = Sys.Date() + 10)
intervention_epi(hit, country = "IND", last_date = "12/31/2019")
intervention_epi(hit, country = "IND", last_date = Sys.Date() + 10)
intervention_epi(hit, country = "IND", first_date = "5/31/2020", last_date = "4/1/2020")
intervention_epi(hit, country = "IND", last_date = "1/1/2020")

#Admin1 level
intervention_epi(hit_data, admin1 = "BEL.1_1")
intervention_epi(hit_data, admin1 = "DEU.1_1")
intervention_epi(hit_data, admin1 = "GBR.1_1")
intervention_epi(hit_data, admin1 = "ITA.1_1")
intervention_epi(hit_data, admin1 = "RUS.10_1")
intervention_epi(hit_data, admin1 = "BRA.1_1")
intervention_epi(hit_data, admin1 = "USA.1_1")
intervention_epi(hit_data, admin1 = "CAN.1_1")
intervention_epi(hit_data, admin1 = "COL.2_1")

#HIT-COVID doesn't have any admin1 data for Belgium
#intervention_epi(hit_data, admin1 = "BEL.1_1")
#The case-count data for Afghanistan is not good quality
#intervention_epi(hit_data, admin1 = "AFG.12_1")
#The case-count data for India isn't loading
#intervention_epi(hit_data, admin1 = "IND.1_1")




