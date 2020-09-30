
context("pull_data Function")
library(hitRcovid)

continent_test <- c("Asia", "North America")
country_test <- c("USA", "CAN")
admin1_test <- c("USA.39_1", "CAN.7_1")
local_test <- "New York City"
int_test <- c("household_confined", "school_closed")
county_test <- FALSE


test_that("pull_data returns a dataframe",{
  
  expect_true(is.data.frame(pull_data()))

})

test_that("filter_data")



test_that("Descriptive warning messages returned",{
  
  expect_error(estimateRWrapper(nbResults, indIDVar = "garbage"),
               "garbage.1 is not in the data frame.")
  
  
})




