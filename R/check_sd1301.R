#' @name check_sd1301
#' @title Check invalid value for TDSTOFF variable
#' @description Validates that Offset from the Anchor (TDSTOFF) variable value 
#' must be either 0, ISO 8601 format for duration (e.g., P80Y), or null.
#' Applicable to rule: SD1301.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (expected "TD").
#' @param target_vars Character vector of length 1. The variable to check 
#' (expected "TDSTOFF").
#' @param rule_id The rule ID being checked.
#' @param ... Absorb extra parameters passed from config.
#' @author Zhu Xiuling
#'
#' @return Data frame containing error details (one row per error) or NULL if no errors.
#'
#' @import dplyr
#' @importFrom logger log_info
# ==============================================================================
# Modification History
# ==============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-08-31  Zhu Xiuling             Initial version (vectorized)
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd1301 <- function(df, domain_name, target_vars, rule_id, ...) {
  
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  if (!all(actual_target_vars %in% names(df))) return(NULL)
  
  tdstoff <- trimws(as.character(df[[actual_target_vars]]))
  
  # 3. Core logic (vectorized) ----
  # Valid patterns: "0" OR ISO 8601 duration (e.g., P80Y, P1Y2M3D, -P6M)
  is_zero <- grepl("^-?0+$", tdstoff)
  is_iso_duration <- grepl("^-?P(\\d+[YMWD])+$", tdstoff)
  
  # Only check non-missing values
  is_populated <- !is.na(tdstoff) & tdstoff != ""
  
  err_idx <- which(is_populated & !is_zero & !is_iso_duration)
  
  if (length(err_idx) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = as.character(df$rownumber_new[err_idx]),
    variable_name  = actual_target_vars,
    original_value = tdstoff[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s variable value must be either 0, ISO 8601 format for duration (e.g., P80Y), or null.",
                             actual_target_vars),
    domain_name   = domain_name
  )
  
  return(report_df)
}
