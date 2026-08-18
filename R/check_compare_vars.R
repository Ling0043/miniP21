#' @name check_compare_vars
#' @title Check relationship between two variables
#' @description Validates that the relationship between two variables meets 
#' expectations (e.g., they should not be identical).
#' 
#' Supports wildcard '--' resolution.
#' Applicable to rules: SD1041, SD1327, SD1328.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param var1 Name of the first variable.
#' @param var2 Name of the second variable.
#' @param relation Character string, expected relation. 
#'   Allowed values: "not_equal" (values should not be identical).
#'   Default is "not_equal".
#' @param rule_id The rule ID being checked.
#' @param check_na Logical. If TRUE, treats NA values as comparable (NA==NA).
#'   Default is FALSE (ignores rows where either value is NA/blank).
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
# 1.0      2026-08-17  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_compare_vars <- function(df, domain_name, var1, var2, 
                               relation = "not_equal", rule_id, 
                               check_na = FALSE, ...) {
  
  # 1. Pre-processing ----
  # Resolve wildcards
  actual_var1 <- gsub("--", domain_name, var1)
  actual_var2 <- gsub("--", domain_name, var2)
  
  # Check if columns exist
  if (!all(c(actual_var1, actual_var2) %in% names(df))) {
    return(NULL)
  }
  
  # Extract data
  val1 <- df[[actual_var1]]
  val2 <- df[[actual_var2]]
  
  # 2. Core logic ----
  # Normalize values for comparison (trim whitespace, handle NA)
  is_valid_val1 <- !is.na(val1) & trimws(as.character(val1)) != ""
  is_valid_val2 <- !is.na(val2) & trimws(as.character(val2)) != ""
  
  if (relation == "not_equal") {
    # Rule: Values should NOT be identical.
    # Error occurs if: val1 == val2 (and both are populated, unless check_na is TRUE)
    
    if (check_na) {
      # Compare including NAs
      are_same <- (is.na(val1) & is.na(val2)) | (!is.na(val1) & !is.na(val2) & val1 == val2)
    } else {
      # Only compare if both are populated (standard interpretation of "Values ... are identical")
      are_same <- is_valid_val1 & is_valid_val2 & (val1 == val2)
    }
    
    err_idx <- which(are_same)
    
  } else {
    # Placeholder for future relations (e.g., "equal")
    stop("Relation type '", relation, "' not implemented.")
  }
  
  # 3. Error Reporting ----
  if (length(err_idx) > 0) {
    err_rows <- df$rownumber_new[err_idx]
    row_number_str <- paste(err_rows, collapse = ", ")
    
    # Use var1 as the primary variable for reporting
    variable_name_str <- paste(actual_var1, actual_var2, sep = ", ")
    
    # Construct error message based on rule text style
    if (relation == "not_equal") {
      error_msg <- sprintf("Values of %s and %s are identical, but they should not be.", 
                           actual_var1, actual_var2)
    }
    
    report_df <- report_error(
      row_number     = row_number_str,
      variable_name  = variable_name_str,
      original_value = "", # Values are identical, can be omitted for summary
      rule_id        = rule_id,
      error_message  = error_msg
    )
    
    return(report_df)
  }
  
  return(NULL)
}
