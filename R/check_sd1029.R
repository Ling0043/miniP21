#' @name check_sd1029
#' @title Check non-ASCII or non-printable characters in variable
#' @description Validates that variable values do not include non-ASCII or 
#' non-printable characters (outside of 32-126 ASCII code range). Applied only 
#' to variables whose values may be converted into a new variable name or label 
#' (--TEST, --TESTCD, --PARM, --PARMCD, QLABEL, QNAM).
#' Applicable to rule: SD1029.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "LB", "SUPPQUAL").
#' @param target_vars Character vector. Variables to check, e.g. 
#' c("--TEST", "--TESTCD", "QLABEL", "QNAM").
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

check_sd1029 <- function(df, domain_name, target_vars, rule_id, ...) {
  
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  # Keep only variables that exist in the dataset
  actual_target_vars <- actual_target_vars[actual_target_vars %in% names(df)]
  
  if (length(actual_target_vars) == 0) return(NULL)
  
  # 3. Core logic (vectorized, per variable) ----
  # [^ -~] matches any character outside the printable ASCII range 32-126
  err_list <- lapply(actual_target_vars, function(v) {
    v_val <- as.character(df[[v]])
    err_idx <- which(!is.na(v_val) & grepl("[^ -~]", v_val))
    
    if (length(err_idx) == 0) return(NULL)
    
    data.frame(
      row_number     = as.character(df$rownumber_new[err_idx]),
      variable_name  = v,
      original_value = v_val[err_idx],
      error_message  = sprintf("%s must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range).", v),
      stringsAsFactors = FALSE
    )
  })
  
  err_df <- bind_rows(err_list)
  
  if (nrow(err_df) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = err_df$row_number,
    variable_name  = err_df$variable_name,
    original_value = err_df$original_value,
    rule_id        = rule_id,
    error_message  = err_df$error_message,
    domain_name   = domain_name
  )
  
  return(report_df)
}