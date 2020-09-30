
context("hit_pull and hit_filter")
library(hitRcovid)

continent_test <- c("Asia", "North America")
country_test <- c("USA", "CHN")
admin1_test <- c("USA.39_1", "CHN.24_1")
local_test <- "New York City"
int_test <- c("household_confined", "school_closed")

#Pulling data
hit <- hit_pull()


test_that("hit_pull and hit_filter return a dataframes",{
  
  expect_true(is.data.frame(hit))
  expect_true(is.data.frame(hit_filter(hit)))
  expect_true(is.data.frame(hit_filter(hit,
                                        continent = continent_test,
                                        country = country_test,
                                        admin1 = admin1_test,
                                        intervention_group = int_test)))
  expect_true(is.data.frame(hit_filter(hit, locality = local_test)))
  expect_true(is.data.frame(hit_filter(hit, include_usa_county = TRUE)))
  expect_true(is.data.frame(hit_filter(hit, include_usa_county = FALSE)))

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
  
  expect_warning(hit_filter(hit, country = "USA", admin1 = "USA.39_1", intervention_group = "closed_border"),
                 "The following interventions are not represented in the database with the provided countries, admin1 units, and localities: closed_border")
  
  expect_warning(hit_filter(hit, admin1 = "USA.22_1", include_usa_county = TRUE),
                 "The set of filters provided did not match any records in the database.")
  
})

test_that("include_usa_counties works correctly",{
  
  a <- hit_filter(hit, country = "USA", include_usa_county = TRUE)
  b <- hit_filter(hit, country = "USA", include_usa_county = FALSE)
  c <- hit_filter(hit, country = c("USA", "CAN"), include_usa_county = TRUE)
  
  expect_true(sum(!is.na(a$usa_county)) == nrow(a))
  expect_false("usa_county" %in% names(b))
  expect_true(nrow(a) == nrow(c))
  
})

test_that("remove_columns works correctly",{
  
  d <- hit_filter(hit, intervention_group = "closed_border", remove_columns = TRUE)
  e <- hit_filter(hit, intervention_group = "closed_border", remove_columns = FALSE)
  
  expect_true(ncol(d) < ncol(e))
  
})




