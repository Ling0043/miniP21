#' @name check_sd1266
#' @title Check that ETCD is not 'UNPLAN' when SEUPDES is null
#' @description When the Description of Unplanned Element (SEUPDES) is not
#'   populated, the Element Code (ETCD) should not be equal to 'UNPLAN'.
#'   This function identifies records in the SE domain where SEUPDES is null
#'   or blank and ETCD equals 'UNPLAN'.
#' @param df Data frame to check. Must contain a column `rownumber_new`.
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
# 1.0      2026-08-25  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd1266 <- function(df, rule_id, ...) {

  # Ensure required columns exist
  required_cols <- c("SEUPDES", "ETCD")
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }

  # Identify rows where SEUPDES is null/blank AND ETCD equals 'UNPLAN' ----
  seupdes_blank <- is.na(df$SEUPDES) | trimws(as.character(df$SEUPDES)) == ""
  etcd_unplan <- !is.na(df$ETCD) & as.character(df$ETCD) == "UNPLAN"

  invalid_idx <- which(seupdes_blank & etcd_unplan)

  # Error Reporting ----
  if (length(invalid_idx) > 0) {
    report_df <- report_error(
      row_number    = as.character(df$rownumber_new[invalid_idx]),
      variable_name = "ETCD",
      original_value = df$ETCD[invalid_idx],
      rule_id       = rule_id,
      error_message = "ETCD should not be 'UNPLAN' when SEUPDES is not populated",
      domain_name   = domain_name
    )
    return(report_df)
  }

  return(NULL)
}