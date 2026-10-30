#' @name check_sd1474
#' @title Check variable names follow SAS XPORT v5 limitations
#' @description Variable Name should follow limitations of SAS XPORT format v5:
#'   not exceed 8 characters, contain only uppercase letters, numbers, or 
#'   underscores, and cannot start with a number.
#' @param df Data frame to check.
#' @param domain_name Domain name (unused).
#' @param rule_id Rule ID.
#' @param ... Extra arguments (ignored).
#' @return Error report data frame (one row per invalid variable) or NULL.
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

check_sd1474 <- function(df, domain_name, rule_id, ...) {
  # Exclude internal row number column
  col_names <- setdiff(names(df), "rownumber_new")
  if (length(col_names) == 0) return(NULL)
  
  # SAS XPORT v5 pattern: 1-8 chars, only A-Z, 0-9, underscore, not start with digit
  valid_pattern <- "^(?![0-9])[A-Z0-9_]{1,8}$"
  is_valid <- grepl(valid_pattern, col_names, perl = TRUE)
  
  invalid_cols <- col_names[!is_valid]
  if (length(invalid_cols) == 0) return(NULL)
  
  report_error(
    row_number    = "-",
    variable_name = invalid_cols,
    original_value = invalid_cols,
    rule_id       = rule_id,
    error_message = "Variable name does not follow SAS XPORT v5 limitations.",
    domain_name   = domain_name
  )
}