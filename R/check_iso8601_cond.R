#' @name check_iso8601_cond
#'
#' @title Check ISO 8601 Format with Condition
#'
#' @description Checks whether values in a specified variable conform to the
#'   ISO 8601 format (date or duration) when a condition variable equals
#'   certain values. Used for rules SD1215, SD1217, SD1219, SD2245, SD2247,
#'   SD2248.
#'
#' @param df A data frame containing the domain data to check.
#' @param target_vars Character string. Name of the column whose values are
#'   checked for ISO 8601 compliance.
#' @param cond_var Character string. Name of the condition column.
#' @param cond_val Vector of values in `cond_var` that trigger the check.
#' @param type Character string. Either `"date"` for ISO 8601 date/time formats
#'   or `"duration"` for ISO 8601 duration format (e.g. P1Y2M3DT4H5M6S).
#' @param rule_id Character string. Identifier of the validation rule (e.g.
#'   "SD1215").
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
# 1.0      2026-05-26  Zhu Xiuling             Initial version
#
# =============================================================================


check_iso8601_cond <- function(df, target_vars, cond_var, cond_val, type, rule_id, ...) {

  if (!cond_var %in% names(df) || !target_vars %in% names(df)) {
    return(NULL)
  }

  target_idx <- which(df[[cond_var]] %in% cond_val)
  
  if (length(target_idx) == 0) {
    return(NULL) 
  }

  vals_to_check <- df[[target_vars]][target_idx]

  if (type == "date") {
    iso_regex <- "^(\\d{4})-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])(T([01]\\d|2[0-3]):[0-5]\\d(:[0-5]\\d(\\.\\d+)?)?(Z|[+-]([01]\\d|2[0-3]):?[0-5]\\d)?)?$"
  } else if (type == "duration") {
    iso_regex <- "^P(?!$)(\\d+Y)?(\\d+M)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+S)?)?$"
  }

  is_bad_format <- !grepl(iso_regex, vals_to_check, perl = TRUE)

  error_positions <- which(is_bad_format)

  if (length(error_positions) > 0) {
    idx <- target_idx[error_positions]
    original_value <- as.character(df[[target_vars]][idx])

    report_df <- report_error(
      row_number = as.character(idx),
      variable_name = target_vars,
      original_value = original_value,
      rule_id = rule_id,
      error_message = sprintf("Value must be ISO 8601 format when %s='%s'",cond_var, cond_val)
    )
    return(report_df)
  }
  
  return(NULL)
}