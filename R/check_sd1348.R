#' @name check_sd1348
#' @title Check missing --STINT and --ENINT when --TEST references an interval
#' @description Validates that Planned Start of Assessment Interval (--STINT) and 
#' Planned End of Assessment Interval (--ENINT) variables are present and populated 
#' when Name of Measurement, Test or Examination (--TEST) is over a time interval, 
#' i.e., matches the pattern 'T1 to T2' (e.g., "Week 1 to Week 5").
#' Applicable to rule: SD1348.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "PP", "APPP").
#' @param target_vars Character vector. The variables that must be populated 
#' (expected c("--STINT", "--ENINT")).
#' @param rule_id The rule ID being checked.
#' @param cond_vars Character vector of length 1. The condition variable 
#' (expected "--TEST").
#' @param interval_pattern Character string. Regex identifying interval-based 
#' --TEST values. Default "\\d+\\s+to\\s+\\d+" (e.g., "T1 to T2").
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

check_sd1348 <- function(df, domain_name, target_vars, rule_id, cond_vars, ...) {
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_cond_vars <- gsub("--", domain_name, cond_vars)

  # Condition variable must exist
  if (!all(actual_cond_vars %in% names(df))) return(NULL)
  
  test_val <- trimws(as.character(df[[actual_cond_vars]]))

  # Condition: --TEST references a time interval 'T1 to T2'
  interval_pattern <- "T1 to T2"
  cond_trigger <- !is.na(test_val) & test_val != "" & grepl(interval_pattern, test_val)
  
  if (!any(cond_trigger)) return(NULL)
  
  err_list <- list()
  # 3. Core logic (vectorized) ----
  # Check each target variable: must exist and be populated on triggered rows
  err_list <- lapply(actual_target_vars, function(v) {

    if (!(v %in% names(df))) {
      # Variable itself is missing entirely: all triggered rows are errors
      idx <- which(cond_trigger)
      val <- rep(NA_character_, length(idx))
      msg <- sprintf("%s variable should be present and populated when %s references a time interval ('T1 to T2').", v, actual_cond_vars)
    } else {
      v_val <- trimws(as.character(df[[v]]))
      is_missing <- is.na(v_val) | v_val == ""
      idx <- which(cond_trigger & is_missing)
      val <- v_val[idx]
      msg <- sprintf("%s should be populated when %s references a time interval ('T1 to T2').", v, actual_cond_vars)
    }

    list(row_number = as.character(df$rownumber_new[idx]),
         variable_name = v,
         original_value = val,  
         error_message = msg)
  })
  
  err_df <- bind_rows(err_list)
  
  if (nrow(err_df) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = as.character(err_df$row_number),
    variable_name  = err_df$variable_name,
    original_value = err_df$original_value,
    rule_id        = rule_id,
    error_message  = err_df$error_message,
    domain_name   = domain_name
  )

  return(report_df)
}
