#' @name check_sd2019
#' @title Check that AGETXT values follow the 'number-number' format
#' @description Validates that non-missing values of the Age Range (AGETXT)
#'   variable in the DM/APDM domains conform to a 'number-number' pattern
#'   (e.g., "18-65"). The pattern allows optional leading/trailing spaces and
#'   decimal numbers. Applies to rule SD2019.
#' @param df Data frame to check. Must contain a column `rownumber_new`.
#' @param domain_name The domain name of the dataset (used for wildcard
#'   resolution, though not strictly required here).
#' @param target_vars Character string of target variable names (expected
#'   `"AGETXT"`); `--` placeholders will be substituted.
#' @param rule_id The rule ID being checked.
#' @param ... Absorb extra parameters passed from configuration.
#' @author Zhu Xiuling
#'
#' @return A data frame with columns: `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, `error_message`; or `NULL` if no violations
#'   are found.
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @export
# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-08-26  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd2019 <- function(df, domain_name, target_vars, rule_id, ...) {

  # 2. Pre-processing ----
  # Resolve wildcards (not used here, but kept for consistency)
  actual_target_vars <- gsub("--", domain_name, target_vars)

  # Ensure the target variable exists
  if (!all(actual_target_vars %in% names(df))) {
    return(NULL)
  }

  # 3. Define the valid pattern ----
  # Pattern: optional spaces, number (integer or decimal), hyphen, number, optional spaces
  valid_pattern <- "^\\s*\\d+(\\.\\d+)?\\s*-\\s*\\d+(\\.\\d+)?\\s*$"

  # 4. Identify invalid rows ----
  agetxt_vals <- df[[actual_target_vars]]
  non_missing <- !is.na(agetxt_vals) & trimws(as.character(agetxt_vals)) != ""

  # Only check non-missing values
  check_idx <- which(non_missing)
  if (length(check_idx) == 0) return(NULL)

  invalid <- !grepl(valid_pattern, as.character(agetxt_vals[check_idx]))
  invalid_idx <- check_idx[invalid]

  # 5. Error Reporting ----
  if (length(invalid_idx) > 0) {
    var_name_str <- actual_target_vars
    error_message_str <- sprintf("%s must be in 'number-number' format", var_name_str)

    report_df <- report_error(
      row_number    = as.character(df$rownumber_new[invalid_idx]),
      variable_name = var_name_str,
      original_value = agetxt_vals[invalid_idx],
      rule_id       = rule_id,
      error_message = error_message_str,
      domain_name   = domain_name
    )

    return(report_df)
  }

  return(NULL)
}