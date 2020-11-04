context("intervention_map")
library(hitRcovid)

hit <- hit_pull(add_first_case = FALSE)

test_that("intervention_map returns ggplot objects with no errors",{
  
  expect_true("ggplot" %in% class(intervention_map(hit, intervention_group = "symp_screening")))
  
  expect_true("ggplot" %in% class(intervention_map(hit, intervention_group = "household_confined")))
  
  expect_true("ggplot" %in% class(intervention_map(hit, intervention_group = "restaurant_closed", time_point = "3/30/2020")))
  
  expect_true("ggplot" %in% class(intervention_map(hit, intervention_group = "closed_border", time_point = "5/20/2020")))
})


test_that("Descriptive error messages and notes returned from intervention_map",{
  
  expect_error(intervention_map(hit, intervention_group = "hand_washing"),
               "The intervention code entered here is not valid.")
  
  expect_error(intervention_map(hit, intervention_group = "closed_border", time_point = "today"),
               "Time point entered here is not valid, please enter a valid date in right format.")
  
  expect_error(intervention_map(hit, intervention_group = "closed_border", time_point = "10/10/2016"),
               "Time point entered here is not valid, please enter valid date between 1/1/2020 and present.")

})
