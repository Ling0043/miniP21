#' @name check_sd0003
#'
#' @title Check ISO 8601 date/time format
#'
#' @description SD0003: Value of Dates/Time variables (*DTC) must contain 
#'     valid date or valid date and time values and must conform to the 
#'     ISO 8601 international standard.
#'
#' @param df domain dataset to check
#' @return rule code, check status, error detail, row number of the error
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
# 1.0      2026-05-26   Xiuling                Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

# 2. Main function(s) ----
check_sd0003 <- function(df, ...) {
  # # 1. Logging start ----
  # logger::log_info("[check_sd0003] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'df' must be a data frame.")
  }
  
  # 3. Pre-processing ----
   # Identify all date/time variables (ending with "DTC", case-insensitive)
    dtc_vars <- grep("DTC$", names(df), value = TRUE, ignore.case = TRUE)
    if (length(dtc_vars) == 0) {
     # logger::log_info("[check_sd0003] No DTC variables found. Exit.")
      return(NULL)
    }
  # 4. Core logic ----
  # ISO 8601 complete date or datetime pattern:
  # YYYY-MM-DD
  # YYYY-MM-DDThh:mm
  # YYYY-MM-DDThh:mm:ss
  # YYYY-MM-DDThh:mm:ss.sss...
  # Optional timezone: Z, ±hh:mm, ±hh
  iso8601_pattern <- paste0(
    "^\\d{4}-\\d{2}-\\d{2}",
    "(T\\d{2}:\\d{2}(:\\d{2})?(\\.\\d+)?(Z|[+-]\\d{2}(:?\\d{2})?)?)?$"
  )

  all_errors <- list()

  for (var in dtc_vars) {
    vals <- df[[var]]

    # Ignore missing or empty values
    is_blank <- is.na(vals) | trimws(as.character(vals)) == ""
    idx_to_check <- which(!is_blank)

    if (length(idx_to_check) == 0) next

    # Extract trimmed character values
    vals_trim <- trimws(as.character(vals[idx_to_check]))

    # Validate each non-blank value against the ISO 8601 pattern
    valid_flags <- grepl(iso8601_pattern, vals_trim)

    # Rows that fail the check
    error_rows <- idx_to_check[!valid_flags]
    orig_value <- as.character(df[[var]][error_rows])
    
    if (length(error_rows) > 0) {
      all_errors[[length(all_errors) + 1]] <- report_error(
          row_number = as.character(error_rows),
          variable_name = var,
          original_value = orig_value,
          rule_id = "SD0003",
          error_message = sprintf(
            "Value '%s' of variable '%s' does not conform to ISO 8601 format.",
            orig_value, var
          ))
      }
    }

  # # 5. Logging end ----
  # logger::log_info("[check_sd0003] End.")
    return(bind_rows(all_errors))
}