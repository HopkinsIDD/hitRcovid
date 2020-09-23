
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
# 
# geo_lookup <- read.csv("data/geo_lookup.csv")
# geo_lookup <- geo_lookup %>%
#   mutate(admin1_name = iconv(admin1_name, "UTF-8", "ASCII//TRANSLIT"))
# use_data(geo_lookup, overwrite = TRUE)

# intervention_lookup <- read.csv("data/intervention_lookup.csv")
# use_data(intervention_lookup, overwrite = TRUE)

