
context("hit_pull and hit_filter")
library(hitRcovid)

continent_test <- c("Asia", "North America")
country_test <- c("USA", "CAN", "CHN")
admin1_test <- c("USA.33_1", "USA.39_1", "CHN.24_1")
local_test <- "New York City"
int_test <- c("household_confined", "school_closed")

#Pulling data
hit <- hit_pull()
hit2 <- hit_pull(add_first_case = FALSE)
hit3 <- hit_pull(add_first_case = TRUE, source = "ECDC")

test_that("hit_pull returns a dataframe",{
  
  expect_true(is.data.frame(hit))
  expect_true(is.data.frame(hit2))
  expect_true(is.data.frame(hit3))
  
  expect_true("first_case" %in% names(hit))
  expect_false("first_case" %in% names(hit2))
  expect_true("first_case" %in% names(hit3))
  
})


test_that("hit_filter returns a dataframe",{
  
  expect_true(is.data.frame(hit_filter(hit)))
  expect_true(is.data.frame(hit_filter(hit,
                                        continent = continent_test,
                                        country = country_test,
                                        admin1 = admin1_test,
                                        intervention_group = int_test)))
  expect_true(is.data.frame(hit_filter(hit, admin1 = NA)))
  expect_true(is.data.frame(hit_filter(hit, locality = local_test)))
  expect_true(is.data.frame(hit_filter(hit, usa_county_data = "include")))
  expect_true(is.data.frame(hit_filter(hit, usa_county_data = "exclude")))
  expect_true(is.data.frame(hit_filter(hit, usa_county_data = "restrict_to")))

})


test_that("Descriptive warning messages returned from hit_filter",{
  
  expect_warning(hit_filter(hit, continent = c("Asia", "garbage")),
                 "At least one continent provided is not valid")
  
  expect_warning(hit_filter(hit, country = c("USA", "garbage")),
                 "The following country codes are not valid: garbage")
  
  expect_warning(hit_filter(hit, admin1 = c("USA.39_1", "garbage")),
                 "The following admin1 codes are not valid: garbage")
  
  expect_warning(hit_filter(hit, intervention_group = c("household_confined", "garbage")),
                 "The following intervention codes are not valid: garbage")
  
  expect_warning(hit_filter(hit, country = c("USA", "SWE")),
                 "The following countries are not represented in the database with the provided continent: SWE")
  
  expect_warning(hit_filter(hit, country = "USA", admin1 = "CHN.24_1"),
                 "The following admin1 units are not represented in the database with the provided countries: CHN.24_1")
  
  expect_warning(hit_filter(hit, country = "USA", admin1 = "USA.39_1", locality = "New York City"),
                 "The following localities are not represented in the database with the provided countries and admin1 units: New York City")
  
  expect_warning(hit_filter(hit, country = "USA", admin1 = "USA.39_1", intervention_group = "closed_border",
                            include_national = FALSE),
                 "The following interventions are not represented in the database with the provided countries, admin1 units, and localities: closed_border")
  
  expect_warning(hit_filter(hit, admin1 = "USA.22_1", usa_county_data = "restrict_to"),
                 "The set of filters provided did not match any records in the database.")
  
  expect_error(hit_filter(hit, usa_county_data = "garbage"),
               "usa_county_data should be one of 'include', 'exclude', 'restrict_to'")
  
})

test_that("include_usa_counties works correctly",{
  
  a <- hit_filter(hit, country = "USA", usa_county_data = "restrict_to")
  b <- hit_filter(hit, country = "USA", usa_county_data = "exclude")
  c <- hit_filter(hit, country = c("USA", "CAN"), usa_county_data = "restrict_to")
  d <- hit_filter(hit, country = "USA", usa_county_data = "include")
  
  expect_true(sum(!is.na(a$usa_county)) == nrow(a))
  expect_false("usa_county" %in% names(b))
  expect_true(nrow(a) == nrow(c))
  expect_true(nrow(d) > nrow(b))
  
})

test_that("inclusion arguments work correctly",{
  
  #Admin1 and national
  a <- hit_filter(hit, admin1 = admin1_test)
  #Admin1 only
  b <- hit_filter(hit, admin1 = admin1_test, include_national = FALSE)
  #Admin1, national, locality
  c <- hit_filter(hit, admin1 = admin1_test, include_locality = TRUE)
  #Admin1, national
  d <- hit_filter(hit, country = country_test)
  #National only
  e <- hit_filter(hit, country = country_test, include_admin1 = FALSE)
  #Admin1 only
  f <- hit_filter(hit, country = country_test, include_national = FALSE)
  #Admin1, national, locality
  g <- hit_filter(hit, country = country_test, include_locality = TRUE)
  #Locality only
  h <- hit_filter(hit, locality = local_test)
  
  #Admin1 and national (conflicting arguments)
  i <- hit_filter(hit, admin1 = admin1_test, include_admin1 = FALSE)
  #Locality only (conflicting arguments)
  j <- hit_filter(hit, locality = local_test, include_locality = FALSE)
  
  #Confirming conflicting commands works
  expect_true(nrow(a) == nrow(i))
  expect_true(nrow(h) == nrow(j))
  
  #Confirming national is included when it should be
  expect_true(sum(is.na(a$admin1)) > 0)
  expect_true(sum(is.na(c$admin1)) > 0)
  expect_true(sum(is.na(d$admin1)) > 0)
  expect_true(sum(is.na(g$admin1)) > 0)
  
  #Confirming national is excluded when it should be
  expect_true(sum(is.na(b$admin1)) == 0)
  expect_true(sum(is.na(f$admin1)) == 0)
  expect_true(sum(is.na(h$admin1)) == 0)
  
  #Confirming admin1 is included when it should be
  expect_true(sum(!is.na(a$admin1)) > 0)
  expect_true(sum(!is.na(b$admin1)) > 0)
  expect_true(sum(!is.na(c$admin1)) > 0)
  expect_true(sum(!is.na(d$admin1)) > 0)
  expect_true(sum(!is.na(f$admin1)) > 0)
  expect_true(sum(!is.na(g$admin1)) > 0)
  
  #Confirming admin1 is excluded when it should be
  expect_true(!"admin1" %in% names(e))
  
  #Confirming locality is included when it should be
  expect_true(sum(!is.na(c$locality)) > 0)
  expect_true(sum(!is.na(g$locality)) > 0)
  expect_true(sum(!is.na(h$locality)) > 0)
  
  #Confirming locality is excluded when it should be
  expect_true(!"locality" %in% names(a))
  expect_true(!"locality" %in% names(b))
  expect_true(!"locality" %in% names(d))
  expect_true(!"locality" %in% names(e))
  expect_true(!"locality" %in% names(f))
  
})

test_that("remove_columns works correctly",{
  
  a <- hit_filter(hit, intervention_group = "closed_border", remove_columns = TRUE)
  b <- hit_filter(hit, intervention_group = "closed_border", remove_columns = FALSE)
  
  expect_true(ncol(a) < ncol(b))
  
})




