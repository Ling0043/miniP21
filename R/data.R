#' Pinnacle 21 v34 Validation Rules Dictionary
#'
#' A data frame containing the core validation rules for SDTM data based on the 
#' Pinnacle 21 (P21) v34 standard. Team members can use this dataset directly, 
#' and the main function `fct_validate_sdtm()` relies on it as the baseline for validation.
#'
#' @format A data frame with multiple rows representing individual rules, containing the following core columns:
#' \describe{
#'   \item{code}{The unique rule identifier, e.g., "SD0002", "SD0055".}
 #'  \item{description}{A concise description of the validation rule.}
#'   \item{domains}{The SDTM domain or dataset name the rule applies to, e.g., "AE", "DM".}
#'   \item{message}{The standard error or warning message output when the rule is triggered.}
#' }
#' @source Pinnacle 21 Official Configuration Documents and CDISC Standards.
"p21_v34"


#' SDTM v3.4 dictionary
#'
#' Includes basic mappings for the variables, data types, and controlled terms (CTs) defined in the SDTM v3.4 standard.
#'
#' @format A data frame
#' @source CDISC SDTMIG v3.4 Standard Documentation.
"sdtm_v34"

#' SDTM Domain and Class Mapping Table
#'
#' Used to map specific domains (such as AE, DM, LB) to corresponding classes (such as Interventions, Events, Findings) 
#' for obtaining rule IDs
#'
#' @format A data frame
"domain_class_map"
