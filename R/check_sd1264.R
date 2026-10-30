#' @name check_sd1264
#' @title Check that IDVAR does not equal --SEQ when USUBJID and IDVARVAL are null
#' @description When the RELREC dataset is used to identify relationships between
#'   datasets (USUBJID and IDVARVAL are left null), --SEQ cannot be used for
#'   IDVAR because --SEQ only has meaning within a subject within a dataset,
#'   not across datasets. This function checks that for records with both
#'   USUBJID and IDVARVAL null, IDVAR does not match the pattern of a
#'   domain-specific sequence variable (e.g., AESEQ, LBSEQ).
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

check_sd1264 <- function(df, rule_id, ...) {

  # 2. Pre-processing ----
  # Ensure required columns exist in the data frame
  required_cols <- c("USUBJID", "IDVARVAL", "IDVAR")
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }

  # 3. Identify rows where both USUBJID and IDVARVAL are null/blank ----
  usubjid_blank <- is.na(df$USUBJID) | trimws(as.character(df$USUBJID)) == ""
  idvarval_blank <- is.na(df$IDVARVAL) | trimws(as.character(df$IDVARVAL)) == ""

  cond_idx <- which(usubjid_blank & idvarval_blank)
  if (length(cond_idx) == 0) return(NULL)

  # 4. Check if IDVAR equals a domain-specific sequence variable (--SEQ) ----
  # Pattern: 2-3 uppercase letters followed by "SEQ" (e.g., AESEQ, LBSEQ)
  seq_pattern <- "^[A-Z]{2,3}SEQ$"

  idvar_vals <- as.character(df$IDVAR)
  invalid_idx <- cond_idx[grepl(seq_pattern, idvar_vals[cond_idx])]

  # 5. Error Reporting ----
  if (length(invalid_idx) > 0) {
    report_df <- report_error(
      row_number    = as.character(df$rownumber_new[invalid_idx]),
      variable_name = "IDVAR",
      original_value = df$IDVAR[invalid_idx],
      rule_id       = rule_id,
      error_message = "IDVAR cannot be --SEQ when USUBJID and IDVARVAL are null",
      domain_name   = domain_name
    )

    return(report_df)
  }

  return(NULL)
}