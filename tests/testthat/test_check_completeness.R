
context("check_completeness")
library(hitRcovid)

hit <- hit_pull(add_first_case = FALSE)

test_that("check_completeness prints a report with no errors",{
  
  expect_output(check_completeness(hit, country = "USA"), "Completeness Report for")
  
  expect_message(check_completeness(hit, country = "USA"),
                 "Fewer than 20% of the updates for this country are at the national level so this table\n may be misleading. You can run this function at the admin1 level for more detailed information.")
  
  expect_output(check_completeness(hit, country = "ZWE"), "Completeness Report for")
  
  expect_output(check_completeness(hit, admin1 = "GBR.1_1"), "Completeness Report for")
  
  expect_output(check_completeness(hit, country = "ZWE", admin1 = "GBR.1_1"), "Completeness Report for")
  
})


test_that("Descriptive error messages returned from check_completeness for country/admin1 inputs",{
  
  expect_error(check_completeness(hit, country = "garbage"),
               "The country code provided is not valid.")
  
  expect_error(check_completeness(hit, admin1 = "garbage"),
               "The admin1 code provided is not valid.")
  
  expect_error(check_completeness(hit, country = "SWE"),
               "The country code provided is not represented in the HIT-COVID database.")
  
  expect_error(check_completeness(hit, admin1 = "BEL.1_1"),
               "The admin1 code provided is not represented in the HIT-COVID database.")
  
  expect_error(check_completeness(hit),
               "Please provide either a country or admin1 code.")
})

