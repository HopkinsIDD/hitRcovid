
setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(devtools)
library(roxygen2)
library(testthat)
library(pkgdown)
library(dplyr)

#create_package(".")
#build()
#install()

#Create a vingnette
#usethis::use_vignette("my-vignette")

#Create a directory of tests
#usethis::use_testthat()

#Create license
#use_gpl3_license()


#Create package datasets
geo_lookup1 <- read.csv("../covid19-interventions/hit-covid-timeline/geo_lookup.csv")
continent <- read.csv("../covid19-interventions/hit-covid-timeline/Country_Continent.csv")

#Finding duplicated country codes
dup <- continent %>% filter(duplicated(Three_Letter_Country_Code) & Three_Letter_Country_Code != "")
dupAll <- continent %>% filter(Three_Letter_Country_Code %in% dup$Three_Letter_Country_Code)

#Adding continent info
continent2 <- continent %>%
  select(country = Three_Letter_Country_Code, continent = Continent_Name) %>%
  filter(country != "")

#Adding Alpha_2 code to link to other covidregionaldata and EpiNow2 packages
#Filling in missing codes
codes <- ISOcodes::ISO_3166_1 %>% select(country = Alpha_3, alpha_2 = Alpha_2)
codes$country <- ifelse(codes$alpha_2 == "UK", "GBR", codes$country)
codes$country <- ifelse(codes$alpha_2 == "EL", "GRC", codes$country)
codes$country <- ifelse(codes$alpha_2 == "XK", "XKO", codes$country)

#Adding selected admin1 codes
admin1_codes <- read.csv("ISO_to_GID_admin1_codes.csv")

geo_lookup <- geo_lookup1 %>%
  select(country = admin0, admin1 = GID_1, country_name = NAME_0, admin1_name = NAME_1) %>%
  mutate(admin1_name = iconv(admin1_name, "UTF-8", "ASCII//TRANSLIT"),
         country_name = iconv(country_name, "UTF-8", "ASCII//TRANSLIT")) %>%
  left_join(continent2, by = "country") %>%
  left_join(codes, by = "country") %>%
  left_join(select(admin1_codes, ISO_code, GID_code, ons_region_code),
            by = c("admin1" = "GID_code")) %>%
  rename(admin1_ISO = ISO_code,
         ons_region_code_uk = ons_region_code) %>%
  filter(!is.na(country))

#use_data(geo_lookup, overwrite = TRUE)

intervention_lookup <- read.csv("data/intervention_lookup.csv")
intervention_lookup <- intervention_lookup %>%
  arrange(intervention_type, intervention_group)

#use_data(intervention_lookup, overwrite = TRUE)

load_all()
document()
test()
check()
pkgdown::build_site()

#Add package dependence
use_package("ggplot2")
use_package("covidregionaldata")
use_package("rlang")
use_package("dplyr")
use_package("maps")
use_package("purrr")
use_package("zoo")
use_package("egg")
use_package("vistime")
use_package("knitr")


#Add to ignore file
use_build_ignore("Package_Setup.R")
use_build_ignore("Paper_Figures.R")
use_build_ignore("Package_Testing.R")
use_build_ignore("Admin1_Codes.R")
use_build_ignore("name_corrections.csv")
use_build_ignore(".github")
use_build_ignore("ISO_to_GID_admin1_codes.csv")

