
setwd("~/Boston University/COVID_Interventions/hitRcovid")

library(devtools)
library(roxygen2)
library(testthat)
library(pkgdown)
library(dplyr)

create_package(".")

build()
install()
check()
document()

#Add package dependence
use_package()

#Add to ignore file
use_build_ignore("Package_Setup")

#Create a vingnette
usethis::use_vignette("my-vignette")

#Create a directory of tests
usethis::use_testthat()

