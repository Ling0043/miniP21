#' @name check_sd2001
#'
#' @title check variable length
#'
#' @description SD2001:The value of Actual Arm Code (ACTARMCD) should be no more than 20 characters in length.
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
check_sd2001 <- function(df, domain_name) {
  # use check_length to check the length of ACTARMCD variable
  result <- check_length(
    df = df,
    domain_name = domain_name,
    rule_id = "SD2001",
    variable_name = "ACTARMCD",
    length_limit = 20
  )
  return(result)
}