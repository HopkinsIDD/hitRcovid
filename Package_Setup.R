
setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(devtools)
library(roxygen2)
library(testthat)
library(pkgdown)
library(dplyr)

#create_package(".")
#build()
#install()

load_all()
document()
test()
check()

#Add package dependence
use_package()

#Create license
use_gpl3_license()

#Add to ignore file
use_build_ignore("Package_Setup.R")

#Create a vingnette
usethis::use_vignette("my-vignette")

#Create a directory of tests
usethis::use_testthat()

geo_lookup1 <- read.csv("../covid19-interventions/hit-covid-timeline/geo_lookup.csv")
continent <- read.csv("../covid19-interventions/hit-covid-timeline/Country_Continent.csv")

#Finding duplicated country codes
dup <- continent %>% filter(duplicated(Three_Letter_Country_Code) & Three_Letter_Country_Code != "")
dupAll <- continent %>% filter(Three_Letter_Country_Code %in% dup$Three_Letter_Country_Code)

continent2 <- continent %>%
  select(country = Three_Letter_Country_Code, continent = Continent_Name) %>%
  filter(country != "")

geo_lookup <- geo_lookup1 %>%
  select(country = admin0, admin1 = GID_1, country_name = NAME_0, admin1_name = NAME_1) %>%
  mutate(admin1_name = iconv(admin1_name, "UTF-8", "ASCII//TRANSLIT")) %>%
  left_join(continent2, by = "country")

use_data(geo_lookup, overwrite = TRUE)

# intervention_lookup <- read.csv("data/intervention_lookup.csv")
# use_data(intervention_lookup, overwrite = TRUE)

