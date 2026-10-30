#' @name check_sd1011
#' @title Check that multiple variables conform to ISO 8601 duration/interval format
#' @description Validates that the values of one or more specified variables
#'   (typically duration, elapsed time, or interval variables such as --DUR,
#'   --ELTM, --EVLINT, --STINT, --ENINT, TDSTOFF, TDTGTPAI, TDMINPAI, TDMAXPAI)
#'   conform to the ISO 8601 standard for durations. Supports `--` wildcard
#'   replacement using the domain name. Applies to rule SD1011.
#' @param df Data frame to check. Must contain a column `rownumber_new`.
#' @param domain_name The domain name of the dataset (used to resolve `--`).
#' @param target_vars A comma-separated string of variable names to check.
#'   `--` placeholders will be substituted with `domain_name`.
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

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-08-27  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd1011 <- function(df, domain_name, target_vars, rule_id, ...) {

  var_names <- trimws(unlist(strsplit(target_vars, ",")))
  actual_vars <- gsub("--", domain_name, var_names)

  existing_vars <- actual_vars[actual_vars %in% names(df)]
  if (length(existing_vars) == 0) return(NULL)

  iso_duration_pattern <-
    "^P(?!$)(\\d+Y)?(\\d+M)?(\\d+W)?(\\d+D)?(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+(\\.\\d+)?S)?)?$"

  all_errors <- list()

  for (var in existing_vars) {
    vals <- df[[var]]
    non_missing <- !is.na(vals) & trimws(as.character(vals)) != ""
    idx <- which(non_missing)
    if (length(idx) == 0) next

    is_valid <- grepl(iso_duration_pattern, as.character(vals[idx]), perl = TRUE)
    invalid_abs <- idx[!is_valid]
    if (length(invalid_abs) == 0) next

    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = as.character(df$rownumber_new[invalid_abs]),
      variable_name = var,
      original_value = as.character(vals[invalid_abs]),
      rule_id = rule_id,
      error_message = "Values must conform to ISO 8601 format for this variable",
      domain_name   = domain_name
    )
  }

  if (length(all_errors) == 0) {
    return(NULL)
  } else {
     return(bind_rows(all_errors))
  }
}