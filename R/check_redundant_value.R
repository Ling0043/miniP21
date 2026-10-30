#' @name check_redundant_value
#' @title Check that target variable value is not redundant with other variables
#' @description Validates that the value of the target variable (e.g., --CAT, --SCAT) 
#' is not identical to the value of any comparison variables (e.g., DOMAIN, --TERM, 
#' --TESTCD, --TRT, --DECOD, --BODSYS). Comparison is performed on trimmed, 
#' case-insensitive character values.
#' 
#' Supports wildcard '--' resolution.
#' Applicable to rules: SD1039, SD1040.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of length 1. The variable to check 
#' (e.g., "--CAT" or "--SCAT").
#' @param rule_id The rule ID being checked.
#' @param compare_vars Character vector. Variables whose values the target 
#' should not duplicate (e.g., c("DOMAIN", "--TERM", "--TESTCD", "--TRT", 
#' "--DECOD", "--BODSYS")).
#' @param ignore_case Logical. If TRUE (default), comparison is case-insensitive.
#' @param ... Absorb extra parameters passed from config.
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
# 1.0      2026-08-31  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_redundant_value <- function(df, domain_name, target_vars, rule_id,
                                  compare_vars, ignore_case = TRUE, ...) {
  
  # 2. Pre-processing ----
  # Resolve wildcards
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_compare_vars <- gsub("--", domain_name, compare_vars)
  
  if (length(actual_target_vars) != 1) return(NULL)
  
  # Target variable must exist
  if (!all(actual_target_vars %in% names(df))) return(NULL)
  
  # Keep only comparison variables that exist in the dataset
  existing_compare_vars <- actual_compare_vars[actual_compare_vars %in% names(df)]
  # Remove the target itself if accidentally included
  existing_compare_vars <- setdiff(existing_compare_vars, actual_target_vars)
  
  if (length(existing_compare_vars) == 0) return(NULL)
  
  # Extract target values as trimmed character
  target_val <- trimws(as.character(df[[actual_target_vars]]))
  
  # Only check non-missing target values
  target_pop <- !is.na(target_val) & target_val != ""
  
  if (!any(target_pop)) return(NULL)
  
  if (ignore_case) {
    target_cmp <- toupper(target_val)
  } else {
    target_cmp <- target_val
  }
  
  # 3. Core logic: Check redundancy against each comparison variable ----
  # A row is an error if target value equals ANY comparison variable value
  redundant_matrix <- sapply(existing_compare_vars, function(v) {
    comp_val <- trimws(as.character(df[[v]]))
    if (ignore_case) comp_val <- toupper(comp_val)

    # Equal when both populated and identical
    comp_pop <- !is.na(comp_val) & comp_val != ""
    target_pop & comp_pop & (target_cmp == comp_val)
  })
  
  if (is.null(dim(redundant_matrix))) {
    redundant_matrix <- matrix(redundant_matrix, ncol = 1)
  }
  
  is_error <- apply(redundant_matrix, 1, any)
  
  # 4. Error Reporting ----
  if (any(is_error)) {
    err_idx <- which(is_error)
    error_message_str <- sprintf(
      "Value of %s should not be identical to that in %s.",
      actual_target_vars,
      paste(existing_compare_vars, collapse = ", ")
    )
    
    report_df <- report_error(
      row_number     = as.character(df$rownumber_new[err_idx]),
      variable_name  = actual_target_vars,
      original_value = as.character(target_val[err_idx]),
      rule_id        = rule_id,
      error_message  = error_message_str,
      domain_name   = domain_name
    )
    
    return(report_df)
  }
  
  return(NULL)
}
