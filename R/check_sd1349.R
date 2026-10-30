#' @name check_sd1349
#' @title Check inconsistent STUDYID values
#' @description Validates that Study Identifier (STUDYID) values are consistent 
#' for all subjects in the APDM/DM domain for one study. Records whose STUDYID 
#' differs from the dominant (most frequent) STUDYID value are reported.
#' Applicable to rule: SD1349.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "DM", "APDM").
#' @param target_vars Character vector of length 1. The variable to check 
#' (expected "STUDYID").
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

check_sd1349 <- function(df, domain_name, target_vars, rule_id, ...) {
  
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  if (!all(actual_target_vars %in% names(df))) return(NULL)
  
  studyid <- trimws(as.character(df[[actual_target_vars]]))
  
  # Only check non-missing values
  is_populated <- !is.na(studyid) & studyid != ""
  if (!any(is_populated)) return(NULL)
  
  # 3. Core logic (vectorized) ----
  # Identify the dominant (most frequent) STUDYID value
  freq_table <- sort(table(studyid[is_populated]), decreasing = TRUE)
  dominant_studyid <- names(freq_table)[1]
  
  # Violation: populated STUDYID that differs from the dominant value
  err_idx <- which(is_populated & studyid != dominant_studyid)
  
  if (length(err_idx) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = as.character(df$rownumber_new[err_idx]),
    variable_name  = actual_target_vars,
    original_value = studyid[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s should be consistent for all subjects in %s domain. Expected '%s'.",
                             actual_target_vars, domain_name, dominant_studyid),
    domain_name   = domain_name
  )

  return(report_df)
}
