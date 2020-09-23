
#' Intervention Look-up Table
#' 
#' A dataset with the various codes and names for the intervention groups and specific
#' interventions used in the HIT-COVID database. Can be used to determine interventions
#' to be used in filtering the database.
#' 
#' @format A data frame with 41 rows and 7 variables:
#' \describe{
#' \item{unique_id}{code for the specific intervention used in the \code{unique_id} column of the 
#' database}
#' \item{intervention}{code for the intervention group used in the \code{intervention} column of 
#' the database}
#' \item{intervention_specific}{code for the specific intervention used in the \code{intervention_specific} 
#' column of the database}
#' \item{intervention_clean}{Name corresponding to the intervention group (\code{intervention})}
#' \item{intervention_specific_clean}{Name corresponding to the specific intervention 
#' (\code{intervention_specific})}
#' \item{intervention_short}{Shortened code corresponding to the intervention group (\code{intervention})}
#' \item{intervention_specific_short}{Shortened code corresponding to the specific intervention
#'  (\code{intervention_specific})}
#' 
#' }
#' 
"intervention_lookup"