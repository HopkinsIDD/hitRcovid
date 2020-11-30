
#' Get date of first case and first death
#' 
#' Uses the \code{covidregionaldata} package to find the date of the first case of COVID-19 and the
#' date of the first death from COVID-19. The source is either the European Centre for Disease
#' Control (ECDC) or the World Health Organization (WHO). 
#' 
#' This function can be run on its own to get the first case and death information if that is of 
#' interest. However, it is primarily meant to be run inside of \link{hit_pull} to add this
#' information to the HIT-COVID database.
#' 
#' @param source the source of the case data that is used to determine the date of first case and first
#' death: one of "ECDC" or "WHO" (default is "WHO")
#' 
#' @return 
#' A dataframe with three columns:
#' \enumerate{
#'    \item \code{country} the country code to link with the HIT-COVID database
#'    \item \code{first_case} the date of the first case of COVID-19 in that country according to
#'    the source specified.
#'    \item \code{first_death} the date of the first death in that country from COVID-19 according
#'    to the source specified.
#' }
#' 
#' @examples 
#' 
#' \donttest{
#' #Pulling from ECDC
#' firsts <- get_first_case(source = "ECDC")
#' 
#' #Pulling from WHO
#' firsts <- get_first_case(source = "WHO")
#' }
#' 
#' @seealso \link{hit_filter}, \link[covidregionaldata]{get_national_data}
#' 
#' @references 
#' Sam Abbott, Katharine Sherratt, Jonnie Bevan, Hamish Gibbs, Joel Hellewell, James Munday,
#' Paul Campbell and Sebastian Funk (2020). covidregionaldata: Subnational Data for the
#' Covid-19 Outbreak. R package version 0.6.0.
#' https://CRAN.R-project.org/package=covidregionaldata
#' 
#' ECDC national data: https://opendata.ecdc.europa.eu/covid19
#' 
#' WHO national data: https://covid19.who.int
#' 
#' @export

get_first_case <- function(source = c("WHO", "ECDC")){
  
  source <- toupper(source)
  
  #Setting default source to WHO and throwing an error if the source is not valid
  if(all(source == c("WHO", "ECDC"))){
    source <- "WHO"
  }else if(length(source) > 1){
    stop("source should be one of 'ECDC' or 'WHO'")
  }else if(!source %in% c("WHO", "ECDC")){
    stop("source should be one of 'ECDC' or 'WHO'")
  }
  
  case_counts <- covidregionaldata::get_national_data(source = source)
  
  #Finding first case
  have_cases <- case_counts[case_counts$cases_total > 0, ]
  first_case <- stats::aggregate(have_cases$date, list(have_cases$iso_code), min)
  names(first_case) <- c("iso_code", "first_case")
  
  #Finding first death
  have_deaths <- case_counts[case_counts$deaths_total > 0, ]
  first_death <- stats::aggregate(have_deaths$date, list(have_deaths$iso_code), min)
  names(first_death) <- c("iso_code", "first_death")
  
  #Combining first case and first death
  firsts <- merge(first_case, first_death, by = "iso_code", all = TRUE)
  
  #Adding Alpha_3 code to link to the HIT-COVID database
  firsts <- merge(firsts, hitRcovid::geo_lookup[, c("country", "alpha_2")],
                   by.x = "iso_code", by.y = "alpha_2", all.x = TRUE)
  
  firsts <- firsts[, c("country", "first_case", "first_death")]
  
  return(firsts)
}



