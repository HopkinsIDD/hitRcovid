
context("intervention_timeline")
library(hitRcovid)

hit <- hit_pull()
hit1 <- hit_pull(add_first_case = FALSE)

test_that("intervention_timeline returns ggplot objects with no errors",{
  
  expect_true("ggplot" %in% class(intervention_timeline(hit1, country = "USA")))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA")))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA", include_admin1 = FALSE,
                                          intervention_facet = FALSE)))
              
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA",
                                          include_national = FALSE)))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA",
                                          first_case_line = FALSE)))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA",
                                          first_death_line = FALSE)))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = "USA", first_case_line = FALSE,
                                          first_death_line = FALSE)))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = c("IND", "USA"),
                                          facet_by = "country")))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, continent = c("Africa", "North America", "Europe"),
                        facet_by = "continent")))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, country = c("IND", "USA"),
                        intervention_group = c("closed_border", "school_closed", "household_confined"),
                        facet_by = "country", intervention_facet = FALSE)))
  
  expect_true("ggplot" %in% class(intervention_timeline(hit, admin1 = c("USA.1_1", "USA.2_1", "USA.4_1"),
                        facet_by = "admin1", include_national = TRUE)))
})


test_that("Descriptive error messages and notes returned from intervention_timeline",{
  
  expect_error(intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                        facet_by = "country"),
               "The plot will not draw correctly if you specify more than 20 facets")
  
  expect_error(intervention_timeline(hit, country = "USA", facet_by = "admin1"),
               "The plot will not draw correctly if you specify more than 20 facets")
  
  expect_error(intervention_timeline(hit, continent = c("Africa", "North America", "Asia"),
                        facet_by = "garbage"),
               "facet_by should be one of 'none', 'continent', 'country', 'admin1'")
  
})

test_that("Making sure verbose=FALSE turns off messages",{
  
  expect_output(intervention_timeline(hit, facet_by = "continent",
                                      include_admin1 = FALSE, verbose = FALSE), NA)
})
