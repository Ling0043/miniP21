#' @name check_length
#'
#' @title check the value length
#'
#' @description SD1004、SD1009、SD1022、SD1049、SD1231、SD1259、SD1300、SD1475、SD2001
#'
#' @param df domain dataset to check
#' @param domain_name the domain name to check
#' @param rule_id the rule id to check
#' @param target_vars the variable name to check
#' @param length_limit the length limit for the variable
#' @param ... Additional arguments (currently unused).
#'
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
# 1.0      2026-05-10  Zhu Xiuling             Initial version
#
# =============================================================================

check_length <- function(df,domain_name, rule_id, target_vars, length_limit, ...) {

  # 1. Pre-processing ----
  target_vars <- gsub("--", domain_name, target_vars)
  # if any of the target_vars are missing in df, return NULL (no error report)
  if (!all(target_vars %in% names(df))) {
    return(NULL)
  }

  target_vars_values <- as.character(df[[target_vars]])

  # 2. Core logic ----
  err_idx <- which(nchar(target_vars_values) > length_limit & !is.na(df[[target_vars]]))
  actual_value <- target_vars_values[err_idx]

  # 3. Logging end ----
  if (length(err_idx) > 0) {
    return(report_error(
    row_number = as.character(err_idx),
    variable_name = target_vars,
    original_value = actual_value,
    rule_id = rule_id,
    error_message = sprintf("The value of '%s' should be <= %d characters in length. But actual value is '%s'", 
                            target_vars, length_limit, actual_value)))
    }
  # logger::log_info("[check_length] End.")
  return(NULL)
}