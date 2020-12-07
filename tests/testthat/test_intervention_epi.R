
context("intervention_epi")
library(hitRcovid)

hit <- hit_pull()

test_that("intervention_epi for countries returns grob objects with no errors",{
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100)))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   first_date = "2/1/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   last_date = "9/30/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", first_date = "2/1/2020",
                                                   last_date = "9/30/2020")))
})

test_that("intervention_epi for admin1 units returns grob objects with no errors",{
  
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "DEU.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "GBR.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "ITA.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "RUS.10_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "BRA.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "USA.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "CAN.1_1")))
  expect_true("grob" %in% class(intervention_epi(hit, admin1 = "COL.2_1")))
})


test_that("Descriptive error messages returned from intervention_epi for country/admin1 inputs",{
  
  expect_error(intervention_epi(hit, country = "garbage"),
               "The country code provided is not valid.")
  
  expect_error(intervention_epi(hit, admin1 = "garbage"),
               "The admin1 code provided is not valid.")
  
  expect_error(intervention_epi(hit, admin1 = "CHN.1_1"),
               "There are no case count data available for the admin1 code provided.") 
  
  expect_error(intervention_epi(hit, country = "SWE"),
               "The country code provided is not represented in the HIT-COVID database.")
  
  expect_error(intervention_epi(hit, admin1 = "BEL.1_1"),
               "The admin1 code provided is not represented in the HIT-COVID database.")
  
  expect_error(intervention_epi(hit),
               "Please provide either a country or admin1 code.")
})


test_that("Descriptive error messages returned from intervention_epi for date inputs", {
  
  expect_error(intervention_epi(hit, country = "IND", first_date = "garbage"),
               "first_date is not valid, please enter a valid date in right format")
  
  expect_error(intervention_epi(hit, country = "IND", last_date = "garbage"),
               "last_date is not valid, please enter a valid date in right format")
  
  expect_error(intervention_epi(hit, country = "IND", first_date = "12/31/2019"),
               "first_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", first_date = Sys.Date() + 10),
               "first_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", last_date = "12/31/2019"),
               "last_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", last_date = Sys.Date() + 10),
               "last_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", first_date = "5/31/2020",
                                last_date = "4/1/2020"),
               "first_date must be earlier than last_date")
  
  expect_error(intervention_epi(hit, country = "IND", last_date = "1/1/2020"),
               "first_date must be earlier than last_date")
})

