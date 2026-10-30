#' @name check_sd1022
#'
#' @title check variable length
#'
#' @description SD1022: The value of Qualifier Variable Name (QNAM)
#'     should be limited to 8 characters, cannot start with a number,
#'     and cannot contain characters other than letters in upper case, numbers, or underscores.
#'
#' @param df domain dataset to check
#' @param domain_name name of the domain dataset
#' @param target_vars variable to check
#' @param rule_id rule ID
#' @param ... Additional arguments (currently unused).
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
# 1.0      2026-05-19  Zhu Xiuling             Initial version
#
# =============================================================================

check_sd1022 <- function(df, domain_name, target_vars, rule_id, ...) {
  
  # 1. Pre-processing ----
  target_vars <- gsub("--", domain_name, target_vars)
  # if any of the check_cols are missing in df, return NULL (no error report)
  if (!target_vars %in% names(df)) {
    return(NULL)
  }

  check_values <- as.character(df[[target_vars]])
  is_blank <- is.na(check_values) | trimws(check_values) == ""
  
  valid_pattern <- "^[A-Z_][A-Z0-9_]{0,7}$"
  err_idx <- which(!grepl(valid_pattern, check_values) & !is_blank)
  
  if (length(err_idx) == 0) {
    return(NULL)
  }
  
  all_errors <- list()
  for (idx in err_idx) {
    val <- check_values[idx]

    if (nchar(val) > 8) {
      specific_msg <- "should be <= 8 characters in length"
    } else if (grepl("^[0-9]", val)) {
      specific_msg <- "can't starts with a number"
    } else {
      specific_msg <- "contains invalid characters (lowercase or symbols)"
    }
    
    # 组合最终的 Message
    msg <- sprintf("Invalid %s value '%s' (%s).", target_vars, val, specific_msg)
    
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = as.character(idx),
      variable_name = target_vars,
      original_value = val,
      rule_id = rule_id,
      error_message = msg,
      domain_name   = domain_name
    )
  }
 
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}