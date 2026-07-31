#' @name check_expected_cond
#'
#' @title Check Mandatory Value with Condition
#'
#' @description Checks whether values in a specified variable equal a mandatory fixed value
#'   when one or more condition variables meet specific criteria (fixed value/empty/non-empty).
#'   Used for rules SD0023, SD0042, SD0090, SD0091, SD1045, SD1046, SD1062, SD1249, SD1314,
#'   SD2004, SD2240, SD2241, SD2242, SD2243, SD2256, SD2266.
#'
#' @param df A data frame containing the domain data to check.
#' @param domain_name Character string. Name of the domain to which the columns belong.
#' @param target_vars Character string. Name of the column whose values must equal the mandatory value.
#' @param cond_vars Character vector. Names of the condition columns.
#' @param cond_vals Character vector. Matching criteria for condition columns:
#'   fixed value (e.g., 'Y'), 'EMPTY' (null/blank), 'NOT_EMPTY' (non-null/non-blank).
#' @param expected_val Atomic value. Mandatory fixed value that target_vars must equal.
#' @param rule_id Character string. Identifier of the validation rule (e.g., "SD0023").
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, and `error_message` for each invalid value,
#'   or `NULL` if no errors or required columns are missing.
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
# 1.0      2026-06-09  Zhu Xiuling             Initial version
#
# =============================================================================

check_expected_cond <- function(df, domain_name, target_vars, cond_vars, cond_vals, expected_val, rule_id, ...) {
  # Check if all required columns exist
  if (any(grepl("^--", target_vars))) {
    target_vars <- paste0(domain_name, gsub("^--", "", target_vars))
    cond_vars <- paste0(domain_name, gsub("^--", "", cond_vars))
  }

  required_cols <- c(cond_vars, target_vars)
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }

  # ---- Build logical index for each condition ----
  idx_list <- mapply(
    function(var, val) {
      x <- df[[var]]
      if (val == "__MISSING__") {
        is.na(x) | x == ""
      } else if (val == "__NON_MISSING__") {
        !is.na(x) & x != ""
      } else {
        x %in% val
      }
    },
    var = cond_vars,
    val = cond_vals,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  # Combine conditions with AND
  combined_idx <- Reduce(`&`, idx_list)
  if (!any(combined_idx)) {
    return(NULL)
  }

  # ---- Check target variable against expected value ----
  target_vals <- df[[target_vars]][combined_idx]
  # Treat missing as not equal to expected
  is_bad <- is.na(target_vals) | !any(as.character(target_vals) == expected_val)
  error_positions <- which(is_bad)

  if (length(error_positions) == 0) {
    return(NULL)
  }

  # ---- Build human‑readable condition description ----
  cond_parts <- mapply(
    function(var, val) {
      if (val == "missing") {
        paste0(var, " is missing")
      } else if (val == "non_missing") {
        paste0(var, " is provided")
      } else {
        paste0(var, "='", val, "'")
      }
    },
    var = cond_vars,
    val = cond_vals,
    USE.NAMES = FALSE
  )
  cond_str <- paste(cond_parts, collapse = " and ")

  error_message <- sprintf("Value must be '%s' when %s",
                           paste0(expected_val, collapse = "/"), cond_str)




  # ---- Report errors ----
  idx <- which(combined_idx)[error_positions]
  original_value <- as.character(df[[target_vars]][idx])

  report_df <- report_error(
    row_number   = as.character(idx),
    variable_name = target_vars,
    original_value = original_value,
    rule_id      = rule_id,
    error_message = error_message
  )

  return(report_df)
}