#' @name check_sd1298
#' @title Check LBTOXGR is numeric when numeric scale used
#' @description If LBTOXGR value is from a numeric scale, represent only the 
#'   number (e.g., "2" and not "Grade 2").
#' @param df Data frame to check.
#' @param domain_name Domain name (unused).
#' @param target_vars Target variable (expected "LBTOXGR").
#' @param rule_id Rule ID.
#' @param ... Extra arguments (ignored).
#' @return Error report data frame (one row per violation) or NULL.
#' @author Zhu Xiuling
#' @import dplyr
#' @importFrom logger log_info
#' @export
# =============================================================================
# Modification History
# =============================================================================
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-09-01  Zhu Xiuling             Initial version
# =============================================================================

library(dplyr)
library(logger)

check_sd1298 <- function(df, domain_name, target_vars, rule_id, ...) {

  actual_var <- gsub("--", domain_name, target_vars)
  if (!(actual_var %in% names(df))) return(NULL)

  vals <- df[[actual_var]]
  non_miss <- !is.na(vals) & trimws(as.character(vals)) != ""

  pre_digits <- "^\\w+[ ]*[0-9ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ]"
  lat_digits <- "[0-9ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ][ ]*\\w+$"
  is_error <- grepl(pre_digits, vals) | grepl(lat_digits, vals)

  err_idx <- which(non_miss & is_error)
  if (length(err_idx) == 0) return(NULL)

  report_error(
    row_number    = as.character(df$rownumber_new[err_idx]),
    variable_name = actual_var,
    original_value = as.character(vals[err_idx]),
    rule_id       = rule_id,
    error_message = "LBTOXGR is not a numeric value when a numeric scale is used.",
    domain_name   = domain_name
  )
}