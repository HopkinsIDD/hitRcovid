
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
#' \item{intervention_group}{code for the intervention group used in the \code{intervention_group} column of 
#' the database}
#' \item{intervention_specific}{code for the specific intervention used in the \code{intervention_specific} 
#' column of the database}
#' \item{intervention_group_name}{name corresponding to the intervention group (\code{intervention})}
#' \item{intervention_name}{name corresponding to the specific intervention used in the 
#' \code{intervention_name} column of the database}
#' \item{intervention_type}{broad category of the intervnetion}
#' 
#' }
#' 
"intervention_lookup"