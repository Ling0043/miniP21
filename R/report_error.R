# =============================================================================
# Description of file contents
# =============================================================================

#' @name report_error
#'
#' @title a error per line
#'
#' @description Summary of results
#'
#' @param row_number specific error row number
#' @param variable_name specific error variable name
#' @param original_value the original value of the error
#' @param rule_id error rule ID
#' @param error_message the description of the error
#'
#' @return a date.frame
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

report_error <- function(row_number, variable_name, original_value, rule_id, error_message) {
  data.frame(
    Row = row_number,
    Variable = as.character(variable_name),
    Original_Value = as.character(original_value),
    Rule_ID = as.character(rule_id),
    Message = as.character(error_message),
    stringsAsFactors = FALSE
  )
}
