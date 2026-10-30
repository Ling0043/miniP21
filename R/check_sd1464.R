#' @name check_sd1464
#' @title Check that --DOSTXT is not a numeric value
#' @description Dose Description (--DOSTXT) is a textual description of the
#'   quantity of an agent (e.g., drug, substance, radiation) taken or absorbed
#'   at a single administration. For numeric representation of dose, the Dose
#'   (--DOSE) variable should be utilized. This function verifies that values
#'   in --DOSTXT do not consist solely of a numeric representation.
#' @param df Data frame to check. Must contain a column `rownumber_new`.
#' @param domain_name The domain name of the dataset (used for `--` wildcard
#'   resolution).
#' @param target_vars Character string of target variable names (expected
#'   `"--DOSTXT"`); `--` placeholders will be substituted.
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

check_sd1464 <- function(df, domain_name, target_vars, rule_id, ...) {

  # 2. Pre-processing ----
  # Replace '--' wildcard with domain_name
  actual_vars <- gsub("--", domain_name, target_vars)

  # Ensure the target variable exists
  if (!all(actual_vars %in% names(df))) {
    return(NULL)
  }

  # 3. Define the numeric pattern ----
  # Accepts optional sign, integer/decimal, scientific notation (optional)
  numeric_pattern <- "^[+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?$"

  # 4. Identify problematic rows ----
  var_vals <- df[[actual_vars]]
  non_missing <- !is.na(var_vals) & trimws(as.character(var_vals)) != ""

  check_idx <- which(non_missing)
  if (length(check_idx) == 0) return(NULL)

  is_numeric <- grepl(numeric_pattern, as.character(var_vals[check_idx]))
  invalid_idx <- check_idx[is_numeric]

  # 5. Error Reporting ----
  if (length(invalid_idx) > 0) {
    var_name_str <- actual_vars
    error_message_str <- sprintf("%s should not be a numeric value; use --DOSE for numeric representation", var_name_str)

    report_df <- report_error(
      row_number    = as.character(df$rownumber_new[invalid_idx]),
      variable_name = var_name_str,
      original_value = var_vals[invalid_idx],
      rule_id       = rule_id,
      error_message = error_message_str,
      domain_name   = domain_name
    )

    return(report_df)
  }

  return(NULL)
}