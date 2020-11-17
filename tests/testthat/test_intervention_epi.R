
context("intervention_epi")
library(hitRcovid)

hit <- hit_pull()

test_that("intervention_epi returns grob objects with no errors",{
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100)))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   first_date = "2/1/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", case_threshold = 100,
                                                   last_date = "9/30/2020")))
  
  expect_true("grob" %in% class(intervention_epi(hit, country = "IND", first_date = "2/1/2020",
                                                   last_date = "9/30/2020")))
})


test_that("Descriptive error messages and notes returned from intervention_epi",{
  
  expect_error(intervention_epi(hit, country = "garbage"),
               "The country code provided is not valid")
  
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

