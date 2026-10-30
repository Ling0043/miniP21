#' @name check_sd1026
#' @title Check invalid value for RELTYPE variable
#' @description Validates that Relationship Type (RELTYPE) must be null when 
#' Identifying Variable (IDVAR) is populated with a --SEQ value (e.g., "LBSEQ", "AEBLSEQ").
#' Applicable to rule: SD1026.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (expected "RELREC").
#' @param target_vars Character vector of length 1. The variable that must be null 
#' (expected "RELTYPE").
#' @param rule_id The rule ID being checked.
#' @param cond_vars Character vector of length 1. The condition variable to evaluate 
#' (expected "IDVAR").
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

check_sd1026 <- function(df, domain_name, target_vars, rule_id, cond_vars, ...) {
  
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_cond_vars <- gsub("--", domain_name, cond_vars)
  
  if (!all(c(actual_target_vars, actual_cond_vars) %in% names(df))) return(NULL)

  reltype <- trimws(as.character(df[[actual_target_vars]]))
  idvar <- trimws(as.character(df[[actual_cond_vars]]))

  # 3. Core logic (vectorized) ----
  # Condition: IDVAR is populated with a --SEQ value (ends with "SEQ")
  idvar_is_seq <- !is.na(idvar) & idvar != "" & grepl("SEQ$", idvar, ignore.case = TRUE)
  
  # Violation: condition met AND RELTYPE is populated
  err_idx <- which(idvar_is_seq & !is.na(reltype) & reltype != "")
  
  if (length(err_idx) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = as.character(df$rownumber_new[err_idx]),
    variable_name  = actual_target_vars,
    original_value = reltype[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s must be null when %s is populated with a --SEQ value ('%s').",
                             actual_target_vars, actual_cond_vars, idvar[err_idx]),
    domain_name   = domain_name
  )
  
  return(report_df)
}
