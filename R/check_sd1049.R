#' @name check_sd1049
#'
#' @title check variable length
#'
#' @description SD1049: Qualifier Variable Label (QLABEL) value may have up to 40 characters.
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
check_sd1049 <- function(df, domain_name) {
  # use check_length to check the length of QLABEL variable
  result <- check_length(
    df = df,
    domain_name = domain_name,
    rule_id = "SD1049",
    variable_name = "QLABEL",
    length_limit = 40
  )
  return(result)
}