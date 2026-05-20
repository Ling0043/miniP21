#' @name check_length
#'
#' @title check the value length
#'
#' @description SD1004、SD1009、SD1022、SD1049、SD1231、SD1259、SD1300、SD1475、SD2001
#'
#' @param df domain dataset to check
#' @param domain_name this is an empty parameter
#' @param rule_id the rule id to check
#' @param variable_name the variable name to check
#' @param length_limit the length limit for the variable
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
# 1.0      2026-05-10  Author Name             Initial version
#
# =============================================================================

check_length <- function(df, domain_name, rule_id, variable_name, length_limit) {

  # 1. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  # 2. Pre-processing ----
  target_variable_values <- as.character(df[[variable_name]])

  # 4. Core logic ----
  err_idx <- which(nchar(target_variable_values) > length_limit & !is.na(df[[variable_name]]))

  all_errors <- list()
  for (idx in err_idx) {
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = as.character(idx),
      variable_name = variable_name,
      rule_id = rule_id,
      error_message = sprintf("The value of '%s' should be <= %d characters in length.", 
                              variable_name, length_limit)
    )
  }

  # 5.Logging end ----
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}