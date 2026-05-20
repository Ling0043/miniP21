#' @name check_sd1259
#'
#' @title check variable length
#'
#' @description SD1259: The value of Set Code (SETCD) should be no more than 8 characters in length.
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
check_sd1259 <- function(df, domain_name) {
  # use check_length to check the length of SETCD variable
  result <- check_length(
    df = df,
    domain_name = domain_name,
    rule_id = "SD1259",
    variable_name = "SETCD",
    length_limit = 8
  )
  return(result)
}