#' @name check_sd1053
#' @title Check unexpected ARMCD values in Trial Arms (TA) / Trial Visits (TV)
#' @description Validates that records with ARMCD values 'SCRNFAIL' or 'NOTASSGN' 
#' (subjects who failed screening or were not assigned to study treatment) 
#' are not included in the TA or TV datasets.
#' Applicable to rule: SD1053.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "TA", "TV").
#' @param target_vars Character vector of length 1. The variable to check 
#' (expected "ARMCD").
#' @param rule_id The rule ID being checked.
#' @param excl_vals Character vector. ARMCD values not allowed in the dataset. 
#' Default is c("SCRNFAIL", "NOTASSGN").
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

check_sd1053 <- function(df, domain_name, target_vars, rule_id,
                         excl_vals = c("SCRNFAIL", "NOTASSGN"), ...) {
  
  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  if (!all(actual_target_vars %in% names(df))) return(NULL)
  
  armcd <- trimws(as.character(df[[actual_target_vars]]))
  
  # 3. Core logic (vectorized) ----
  # Records with SCRNFAIL/NOTASSGN should not exist in TA/TV
  err_idx <- which(!is.na(armcd) & armcd %in% excl_vals)
  
  if (length(err_idx) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- data.frame(
    row_number     = as.character(df$rownumber_new[err_idx]),
    variable_name  = actual_target_vars,
    original_value = armcd[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s records with %s='%s' should not be included in the dataset.",
                             domain_name, actual_target_vars, armcd[err_idx]),
    domain_name   = domain_name,
    stringsAsFactors = FALSE
  )
  
  return(report_df)
}
