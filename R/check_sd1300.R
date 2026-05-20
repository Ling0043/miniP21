#' @name check_sd1300
#'
#' @title check variable length
#'
#' @description SD1300: The value of DOMAIN should be no more than 2 characters in length, excluding Associated Persons domains.
#'
#' @param df domain dataset to check
#' @param domain_name the domain of the dataset
#' @return rule code, check status, error detail, row number of the error
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-05-19  Author Name             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

# 2. Main function(s) ----
check_sd1300 <- function(df, domain_name) {
  # use check_length to check the length of DOMAIN variable
  result <- check_length(
    df = df,
    domain_name = domain_name,
    rule_id = "SD1300",
    variable_name = "DOMAIN",
    length_limit = 2
  )
  return(result)
}