


library(COVID19)
library(covidregionaldata)
library(ISOcodes)

get_first_case <- function(){
  
  #### COVID-19 Data Hub ####
  
  # Pull case and death data from COVID-19 Data Hub
  case_data1 <- COVID19::covid19()
  
  #Finding first case and first death
  have_cases1 <- case_data[case_data1$confirmed > 0, ]
  first_case1 <- aggregate(have_cases1$date, list(have_cases1$id), min)
  names(first_case1) <- c("country", "first_case_COVID19")
  
  have_deaths1 <- case_data[case_data1$deaths > 0, ]
  first_death1 <- aggregate(have_deaths1$date, list(have_deaths1$id), min)
  names(first_death1) <- c("country", "first_death_COVID19")
  
  #Combining first case and first death
  firsts1 <- merge(first_case1, first_death1, by = "country", all = TRUE)
  
  
  #### epiforcast ####
  
  #Pull data sourced from European Centre for Disease Control (ECDC)
  case_counts2 <- covidregionaldata::get_national_data(source = "ECDC")
  
  #Finding first case and first death
  have_cases2 <- case_counts2[case_counts2$cases_total > 0, ]
  first_case2 <- aggregate(have_cases2$date, list(have_cases2$iso_code), min)
  names(first_case2) <- c("iso_code", "first_case_ECDC")
  
  have_deaths2 <- case_counts2[case_counts2$deaths_total > 0, ]
  first_death2 <- aggregate(have_deaths2$date, list(have_deaths2$iso_code), min)
  names(first_death2) <- c("iso_code", "first_death_ECDC")
  
  #Combining first case and first death
  firsts2 <- merge(first_case2, first_death2, by = "iso_code", all = TRUE)
  
  #Adding Alpha_3 code
  firsts2 <- merge(firsts2, ISO_3166_1[, c("Alpha_2", "Alpha_3")],
                   by.x = "iso_code", by.y = "Alpha_2", all.x = TRUE)
  
  #Filling in missing codes
  firsts2$Alpha_3 <- ifelse(firsts2$iso_code == "UK", "GBR", firsts2$Alpha_3)
  firsts2$Alpha_3 <- ifelse(firsts2$iso_code == "EL", "GRC", firsts2$Alpha_3)
  firsts2$Alpha_3 <- ifelse(firsts2$iso_code == "XK", "XKO", firsts2$Alpha_3)
  
  firsts2 <- firsts2[, c("Alpha_3", "first_case_ECDC", "first_death_ECDC")]
  names(firsts2) <- c("country", "first_case_ECDC", "first_death_ECDC")
  
  
  
  #### Combining both sources ####
  
  firsts3 <- merge(firsts1, firsts2, by = "country", all = TRUE)
  
  firsts3$first_case <- ifelse(is.na(firsts3$first_case_ECDC), firsts3$first_case_COVID19,
                               ifelse(is.na(firsts3$first_case_COVID19), firsts3$first_case_ECDC,
                                      ifelse(firsts3$first_case_COVID19 <= firsts3$first_case_ECDC,
                                             firsts3$first_case_COVID19, firsts3$first_case_ECDC)))
  firsts3$first_case <- as.Date(firsts3$first_case, origin = "1970-01-01")
  
  firsts3$first_death <- ifelse(is.na(firsts3$first_death_ECDC), firsts3$first_death_COVID19,
                                ifelse(is.na(firsts3$first_death_COVID19), firsts3$first_death_ECDC,
                                       ifelse(firsts3$first_death_COVID19 <= firsts3$first_death_ECDC,
                                              firsts3$first_death_COVID19, firsts3$first_death_ECDC)))
  firsts3$first_death <- as.Date(firsts3$first_death, origin = "1970-01-01")
  
  firsts_final <- firsts3[, c("country", "first_case", "first_death")]
  return(firsts_final)
}



