#' @name check_sd1448
#' @title Check that --STRESN is null when --STRESC is not numeric
#' @description Standardized Result in Numeric Format (--STRESN) should be null
#'   when the Standardized Result in Character Format (--STRESC) is not a
#'   numeric value (e.g., 'BLQ', values containing '<' or '>') or represents
#'   common categorical values +1, +2, +3, +4, +5. This function identifies
#'   records where --STRESC is non-numeric and --STRESN is populated.
#' @param df Data frame to check. Must contain a column `rownumber_new`.
#' @param domain_name The domain name of the dataset (used for `--` wildcard
#'   resolution).
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

check_sd1448 <- function(df, domain_name, rule_id, ...) {

  # 2. Pre-processing ----
  # Resolve '--' wildcards
  stresc_var <- gsub("--", domain_name, "--STRESC")
  stresn_var <- gsub("--", domain_name, "--STRESN")

  # Ensure both variables exist in the data frame
  if (!all(c(stresc_var, stresn_var) %in% names(df))) {
    return(NULL)
  }

  # 3. Define numeric pattern ----
  # Allows optional leading sign, integer or decimal, scientific notation
  numeric_pattern <- "^[+-]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?$"

  # 4. Identify problematic rows ----
  stresc_vals <- df[[stresc_var]]
  stresn_vals <- df[[stresn_var]]

  # --STRESC considered non-numeric if:
  #   - not matching numeric pattern, OR
  #   - equals one of the categorical +1, +2, +3, +4, +5
  categorical_vals <- c("+1", "+2", "+3", "+4", "+5")

  stresc_nonnum <- !is.na(stresc_vals) &
                   trimws(as.character(stresc_vals)) != "" &
                   (!grepl(numeric_pattern, as.character(stresc_vals)) |
                    as.character(stresc_vals) %in% categorical_vals)

  stresn_present <- !is.na(stresn_vals) &
                    trimws(as.character(stresn_vals)) != ""

  invalid_idx <- which(stresc_nonnum & stresn_present)

  # 5. Error Reporting ----
  if (length(invalid_idx) > 0) {
    error_message_str <- sprintf("%s should be null when %s is not numeric",
                                 stresn_var, stresc_var)

    report_df <- report_error(
      row_number    = as.character(df$rownumber_new[invalid_idx]),
      variable_name = stresn_var,
      original_value = as.character(stresn_vals[invalid_idx]),
      rule_id       = rule_id,
      error_message = error_message_str,
      domain_name   = domain_name
    )

    return(report_df)
  }

  return(NULL)
}