#' @name check_sd1267
#' @title Check invalid value for TAETORD variable
#' @description Validates that Planned Order of Element within Arm (TAETORD) 
#' variable value must be an integer.
#' Applicable to rule: SD1267.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (expected "TA").
#' @param target_vars Character vector of length 1. The variable to check 
#' (expected "TAETORD").
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

check_sd1267 <- function(df, domain_name, target_vars, rule_id, ...) {

  # 2. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)

  if (!all(actual_target_vars %in% names(df))) return(NULL)

  taetord <- trimws(as.character(df[[actual_target_vars]]))

  # 3. Core logic (vectorized) ----
  # Only check non-missing values
  is_populated <- !is.na(taetord) & taetord != ""

  # Invalid: populated value cannot be coerced to a whole number
  num_val <- suppressWarnings(as.numeric(taetord))
  err_idx <- which(is_populated & (is.na(num_val) | num_val != floor(num_val)))

  if (length(err_idx) == 0) return(NULL)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  report_df <- report_error(
    row_number     = as.character(df$rownumber_new[err_idx]),
    variable_name  = actual_target_vars,
    original_value = taetord[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s variable value must be an integer.",
                             actual_target_vars),
    domain_name   = domain_name
  )

  return(report_df)
}
