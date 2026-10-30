#' @name check_compare_vars
#' @title Check relationship between two variables
#' @description Validates that the relationship between two variables meets 
#' expectations. Supports "not_equal" (values should not be identical) and 
#' "equal" (values should be identical, e.g., SD1353/SD2236/SD2237), with 
#' optional pre-conditions for the "equal" relation: restrict to numeric 
#' values (require_numeric) and/or rows where two unit variables are equal 
#' or both null (unit_vars).
#' 
#' Supports wildcard '--' resolution.
#' Applicable to rules: SD1041, SD1327, SD1328 (not_equal);
#' SD0085, SD1353, SD1462, SD1463, SD2236, SD2237 (equal).
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Name of the first variable (primary, used for reporting).
#' @param var2 Name of the second variable.
#' @param relation Character string, expected relation. 
#'   Allowed values: "not_equal" (values should not be identical), 
#'   "equal" (values should be identical). Default is "not_equal".
#' @param rule_id The rule ID being checked.
#' @param check_na Logical. If TRUE, treats NA values as comparable (NA==NA).
#'   Default is FALSE (ignores rows where either value is NA/blank).
#' @param require_numeric Logical. Only for relation "equal". If TRUE, only 
#'   checks rows where both values are numeric (for SD1353). Default FALSE.
#' @param unit_vars Character vector of length 2. Only for relation "equal". 
#'   If provided, only checks rows where the two unit variables are populated 
#'   with the same value or both null (for SD1353). Default NULL.
#' @param ... Absorb extra parameters.
#' @author Zhu Xiuling
#'
#' @return Data frame containing error details (one row per error) or NULL if no errors.
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
# 1.1      2026-08-31  Zhu Xiuling             Added "equal" relation with 
#                                              require_numeric/unit_vars 
#                                              pre-conditions (SD1353)
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_compare_vars <- function(df, domain_name, target_vars, var2, 
                               relation, rule_id, 
                               check_na = FALSE,
                               require_numeric = FALSE, unit_vars = NULL, ...) {
  
  # 2. Pre-processing ----
  # Resolve wildcards
  actual_var1 <- gsub("--", domain_name, target_vars)
  actual_var2 <- gsub("--", domain_name, var2)
  
  # Check if columns exist (including optional unit variables)
  required_cols <- c(actual_var1, actual_var2)
  if (!is.null(unit_vars)) {
    required_cols <- c(required_cols, gsub("--", domain_name, unit_vars))
  }
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }
  
  # Extract and normalize data
  val1 <- df[[actual_var1]]
  val2 <- df[[actual_var2]]
  v1 <- trimws(as.character(val1))
  v2 <- trimws(as.character(val2))
  
  is_valid <- function(x) !is.na(x) & x != ""
  
  # 3. Core logic (vectorized) ----
  if (relation == "not_equal") {
    # Rule: Values should NOT be identical.
    # Error occurs if: val1 == val2 (and both are populated, unless check_na is TRUE)
    
    if (check_na) {
      # Compare including NAs
      is_error <- (is.na(val1) & is.na(val2)) | 
        (is_valid(v1) & is_valid(v2) & v1 == v2)
    } else {
      # Only compare if both are populated
      is_error <- is_valid(v1) & is_valid(v2) & (v1 == v2)
    }
    
  } else if (relation == "equal") {
    # Rule: Values should be identical.
    # Error occurs if: val1 != val2 (both populated, subject to pre-conditions)
    
    # Base condition: both values populated
    valid_cond <- is_valid(v1) & is_valid(v2)
    
    # Optional pre-condition: both values must be numeric
    if (require_numeric) {
      n1 <- suppressWarnings(as.numeric(v1))
      n2 <- suppressWarnings(as.numeric(v2))
      valid_cond <- valid_cond & !is.na(n1) & !is.na(n2)
    }
    
    # Optional pre-condition: unit variables equal or both null
    if (!is.null(unit_vars)) {
      u_names <- gsub("--", domain_name, unit_vars)
      u1 <- trimws(as.character(df[[u_names[1]]]))
      u2 <- trimws(as.character(df[[u_names[2]]]))
      valid_cond <- valid_cond & 
        ((is_valid(u1) & is_valid(u2) & u1 == u2) |
         (!is_valid(u1) & !is_valid(u2)))
    }
    
    is_error <- valid_cond & (v1 != v2)
    
  } else {
    # Placeholder for future relations
    stop("Relation type '", relation, "' not implemented.")
  }
  
  err_idx <- which(is_error)
  
  # 4. Error Reporting (one row per error, vectorized) ----
  if (length(err_idx) > 0) {
    
    error_msg <- switch(relation,
                        "not_equal" = sprintf("Values of %s and %s are identical, but they should not be.", 
                                              actual_var1, actual_var2),
                        "equal"     = sprintf("Values of %s and %s should be identical.", 
                                              actual_var1, actual_var2))
    
    report_df <- report_error(
      row_number     = as.character(df$rownumber_new[err_idx]),
      variable_name  = paste(actual_var1, actual_var2, sep = ", "),
      original_value = paste(v1[err_idx], v2[err_idx], sep = ", "),
      rule_id        = rule_id,
      error_message  = error_msg,
      domain_name    = domain_name
    )
    
    return(report_df)
  }
  
  return(NULL)
}