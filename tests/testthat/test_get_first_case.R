
context("get_first_case")
library(hitRcovid)


test_that("get_first_case returns a dataframe with the right columns",{
  
  firsts1 <- get_first_case()
  firsts2 <- get_first_case(source = "WHO")
  firsts3 <- get_first_case(source = "ECDC")
  firsts3 <- get_first_case(source = "who")
  
  cols <- c("country", "first_case", "first_death")
  
  expect_true(is.data.frame(firsts1))
  expect_true(is.data.frame(firsts2))
  expect_true(is.data.frame(firsts3))
  expect_true(is.data.frame(firsts4))
  
  expect_true(all(names(firsts1) == cols))
  expect_true(all(names(firsts2) == cols))
  expect_true(all(names(firsts3) == cols))
  expect_true(all(names(firsts4) == cols))
  
})


test_that("Descriptive error messages returned from get_first_case",{
  
  expect_error(get_first_case("blah"), "source should be one of 'ECDC' or 'WHO'")
  expect_error(get_first_case(c("blah", "blah")), "source should be one of 'ECDC' or 'WHO'")
  
})
