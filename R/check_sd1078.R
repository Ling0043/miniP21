#' @name check_SD1078
#' @title Check permissible variables for all-missing values
#' @description Validates that permissible variables defined in the configuration 
#' are not entirely missing (all NA/blank) for the entire dataset. 
#' If a permissible variable is completely empty, it should be removed or flagged.
#' 
#' This function scans all columns present in the dataframe that match 
#' permissible variable patterns (e.g., start with domain prefix).
#' Applicable to rule: SD1078.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param rule_id The rule ID being checked (e.g., "SD1078").
#' @param permissible_vars Character vector (Optional). If provided, strictly checks 
#' only this list. If NULL (default), checks all variables in the dataset.
#' @param ... Absorb extra parameters.
#' @author Zhu Xiuling
#'
#' @return Data frame containing error details (variable_name, rule_id, 
#' error_message) or NULL if no errors.
#'
#' @import dplyr
#' @importFrom logger log_info
# ==============================================================================
# Modification History
# ==============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-07-08  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_SD1078 <- function(df, domain_name, rule_id, permissible_vars = NULL, ...) {
  
  # 1. Pre-processing ----
  # If no specific list provided, check ALL columns in the dataframe
  if (is.null(permissible_vars)) {
    cols_to_check <- names(df)
  } else {
    # If list provided, check only those existing in df
    cols_to_check <- permissible_vars[permissible_vars %in% names(df)]
  }
  
  # 2. Core logic ----
  # Identify columns that are completely empty (all NA or "")
  empty_vars <- character(0)
  
  for (col in cols_to_check) {
    # Check if every value in the column is NA or blank string
    all_missing <- all(is.na(df[[col]]) | trimws(as.character(df[[col]])) == "")
    
    if (all_missing) {
      empty_vars <- c(empty_vars, col)
    }
  }
  
  # 3. Error Reporting ----
  if (length(empty_vars) > 0) {
    # Format output
    # variable_name_str <- paste(empty_vars, collapse = ", ")
    
    error_message_str <- sprintf("Permissible variable(s) %s should not be present in domain, as they have missing value for all records.", empty_vars)
    
    report_df <- report_error(
      row_number     = "-", # Dataset level issue
      variable_name  = empty_vars,
      original_value = "", 
      rule_id        = rule_id,
      error_message  = error_message_str,
      domain_name   = domain_name
    )
    
    return(report_df)
  }
  
  return(NULL)
}
