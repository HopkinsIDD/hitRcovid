
context("intervention_epi")
library(hitRcovid)

hit <- hit_pull(add_first_case = FALSE)

test_that("intervention_epi for countries returns grob objects with no errors",{
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100)))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   start_date = "2/1/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   end_date = "9/30/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", start_date = "2/1/2020",
                                                   end_date = "9/30/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", admin1 = "GBR.1_1")))

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
  
  expect_error(intervention_epi(hit, country = "IND", start_date = "garbage"),
               "start_date is not valid, please enter a valid date in right format")
  
  expect_error(intervention_epi(hit, country = "IND", end_date = "garbage"),
               "end_date is not valid, please enter a valid date in right format")
  
  expect_error(intervention_epi(hit, country = "IND", start_date = "12/31/2019"),
               "start_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", start_date = Sys.Date() + 10),
               "start_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", end_date = "12/31/2019"),
               "end_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", end_date = Sys.Date() + 10),
               "end_date is not valid, please enter valid date between 1/1/2020 and present.")
  
  expect_error(intervention_epi(hit, country = "IND", start_date = "5/31/2020",
                                end_date = "4/1/2020"),
               "start_date must be earlier than end_date")
  
  expect_error(intervention_epi(hit, country = "IND", end_date = "1/1/2020"),
               "start_date must be earlier than end_date")
})

