#' @name check_sd1022
#'
#' @title check variable length
#'
#' @description SD1022: The value of Qualifier Variable Name (QNAM)
#'     should be limited to 8 characters, cannot start with a number,
#'     and cannot contain characters other than letters in upper case, numbers, or underscores.
#'
#' @param df domain dataset to check
#' @param domain_name the domain of the dataset
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
# 1.0      2026-05-19  Author Name             Initial version
#
# =============================================================================

check_sd1022 <- function(df, domain_name) {

  qnam_values <- as.character(df$QNAM)
  is_blank <- is.na(qnam_values) | trimws(qnam_values) == ""
  
  valid_pattern <- "^[A-Z_][A-Z0-9_]{0,7}$"
  err_idx <- which(!grepl(valid_pattern, qnam_values) & !is_blank)
  
  if (length(err_idx) == 0) {
    return(NULL)
  }
  
  all_errors <- list()
  for (idx in err_idx) {
    val <- qnam_values[idx]

    if (nchar(val) > 8) {
      specific_msg <- "should be <= 8 characters in length"
    } else if (grepl("^[0-9]", val)) {
      specific_msg <- "can't starts with a number"
    } else {
      specific_msg <- "contains invalid characters (lowercase or symbols)"
    }
    
    # 组合最终的 Message
    msg <- sprintf("Invalid QNAM value '%s' (%s).", val, specific_msg)
    
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = as.character(idx),
      variable_name = "QNAM",
      rule_id = "SD1022",
      error_message = msg
    )
  }
 
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}