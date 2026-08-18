#' @name check_cond_numeric
#' @title Check if a variable is numeric under specific conditions
#' @description Validates that a target variable is numeric (optionally allowing 
#' prefixes like <, >, =) when a condition variable meets specific criteria 
#' (either equals a specific value or is simply populated). 
#'
#' Supports wildcard '--' resolution for domain-specific variables.
#' Applicable to rules: SD1221, SD2246, SD2249, SD1470, SD1471, SD1472, SD1473.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of variables to be checked for numeric type.
#' @param rule_id The rule ID being checked.
#' @param cond_var Character string, the variable name used as a condition (e.g., "TSPARMCD", "--ORNRLO").
#' @param cond_val Character string, optional. The value that \code{cond_var} must equal 
#' to trigger the check (e.g., "PLANSUB"). If NULL, the check triggers when \code{cond_var} is populated.
#' @param allow_prefix Logical, default FALSE. If TRUE, allows leading symbols (<, >, =, space) 
#' before the numeric value (for rules like SD1470 series).
#' @param ... Absorb extra parameters.
#' @author Zhu Xiuling
#'
#' @return Data frame containing error details or NULL if no errors.
#'
#' @import dplyr
#' @importFrom logger log_info
# ==============================================================================
# Modification History
# ==============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-08-06  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_cond_numeric <- function(df, domain_name, target_vars, rule_id, cond_var, cond_val = NULL, allow_prefix = FALSE, ...) {
  
  # 1. Pre-processing ----
  # Resolve wildcards
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_cond_var    <- gsub("--", domain_name, cond_var)

  # Check if required columns exist
  required_cols <- c(actual_cond_var, actual_target_vars)
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }

  # 2. Determine rows where the check is applicable ----
  # Select rows where condition is met
  if (!is.null(cond_val)) {
    # Condition: cond_var == cond_val (e.g., TSPARMCD == 'PLANSUB')
    check_rows_idx <- which(!is.na(df[[actual_cond_var]]) & df[[actual_cond_var]] == cond_val)
  } else {
    # Condition: cond_var is populated (e.g., --ORNRLO is not null)
    check_rows_idx <- which(!is.na(df[[actual_cond_var]]) & trimws(as.character(df[[actual_cond_var]])) != "")
  }
  
  if (length(check_rows_idx) == 0) return(NULL)
  
  # Subset for checking
  df_check <- df[check_rows_idx, , drop = FALSE]
  
  # 3. Core logic: Numeric validation ----
  # Function to check if a string is numeric, optionally allowing prefixes
  is_valid_numeric <- function(x, allow_pre) {
    x <- as.character(x)
    # Remove leading whitespace
    x <- trimws(x, "left")
    
    if (allow_pre) {
      # Remove allowed prefixes: <, <=, >, >=, = (and potential spaces)
      # Pattern: Start of string, optional [<>=], optional [=], optional spaces, optional minus
      x <- sub("^[<>=]?[=]?\\s*", "", x)
    }
    
    # Check if remaining is a valid number (int or double)
    !is.na(suppressWarnings(as.numeric(x)))
  }
  
  # Apply check to target variables
  # We report error if ANY of the target vars is NOT numeric (and is populated)
  err_list <- lapply(actual_target_vars, function(v) {
    vals <- df_check[[v]]
    is_populated <- !is.na(vals) & trimws(as.character(vals)) != ""

    # Check numeric validity
    is_num_valid <- is_valid_numeric(vals, allow_prefix)

    # Errors occur where populated but not numeric
    err_row_indices_in_subset <- which(is_populated & !is_num_valid)
    
    if (length(err_row_indices_in_subset) > 0) {
      # Map back to original df row number using rownumber_new
      data.frame(
        row_idx   = df_check$rownumber_new[err_row_indices_in_subset],
        var_name  = v,
        orig_val  = vals[err_row_indices_in_subset]
      )
    } else {
      NULL
    }
  })
  
  err_df <- bind_rows(err_list)

  # 4. Error Reporting ----
  if (nrow(err_df) > 0) {
    # Aggregate row numbers and values for report
    row_number_str <- paste(unique(err_df$row_idx), collapse = ", ")
    variable_name_str <- paste(unique(err_df$var_name), collapse = ", ")
    
    # Construct specific error message
    if (!is.null(cond_val)) {
      error_msg <- sprintf("Value for %s is not numeric when %s equals '%s'.", variable_name_str, actual_cond_var, cond_val)
    } else {
      error_msg <- sprintf("Value for %s is not numeric when %s is populated.", variable_name_str, actual_cond_var)
    }
    
    report_df <- report_error(
      row_number     = row_number_str,
      variable_name  = variable_name_str,
      original_value = "", # Omit detailed values for brevity in summary
      rule_id        = rule_id,
      error_message  = error_msg
    )
    return(report_df)
  }
  
  return(NULL)
}
