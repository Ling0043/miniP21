#' @name check_sd1212
#' @title Check equality of --STRESN and --STRESC for numeric values
#' @description Validates that Standardized Result in Numeric Format (--STRESN) 
#' is equal to Standardized Result in Character Format (--STRESC) when --STRESC 
#' represents a numeric value.
#' Applicable to rule: SD1212.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "LB", "VS").
#' @param target_vars Character vector of length 2. The variables to compare 
#' (e.g., c("--STRESN", "--STRESC")). The second variable is evaluated for numeric type.
#' @param rule_id The rule ID being checked.
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
# 1.0      2026-07-08  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd1212 <- function(df, domain_name, target_var, cond_var, rule_id, ...) {
  
  # 1. Pre-processing ----
  # Resolve wildcards
  target_var <- gsub("--", domain_name, target_var)
  cond_var <- gsub("--", domain_name, cond_var)

  # Extract target data
  v1 <- df[[target_var]] # --STRESN
  v2 <- df[[cond_var]] # --STRESC
  
  v1_char <- as.character(v1)
  v2_char <- as.character(v2)
  
  # 2. Build conditions ----
  # Condition: --STRESC (v2) is populated and represents a numeric value
  v2_pop <- !is.na(v2) & trimws(v2_char) != ""
  v2_num <- suppressWarnings(as.numeric(v2_char))
  v2_is_numeric <- v2_pop & !is.na(v2_num)

  if (!any(v2_is_numeric)) return(NULL)

  # 3. Core logic: Value Comparison ----
  # Error occurs when v2 is numeric, but v1 is either missing or not equal to v2
  # We compare as numeric values to handle cases like "5" vs "5.0"
  v1_num <- suppressWarnings(as.numeric(v1_char))
  v1_is_num <- !is.na(v1_num)
  all_errors <- list()
  # SD1122: Standardized Result in Numeric Format (--STRESN) variable value should be populated when 
  # Standardized Result in Character Format (--STRESC) variable value represents a numeric value.
  is_missing_v1 <- v2_is_numeric & !v1_is_num
  if (any(is_missing_v1)) {
    err_rows <- df$rownumber_new[is_missing_v1]
    var_name_str <- paste(c(target_var, cond_var), collapse = ", ")

    error_message_str1 <- sprintf("Value of %s should be populated when %s represents a numeric value.", 
                                 target_var, cond_var)

    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number     = as.character(err_rows),
      variable_name  = var_name_str,
      original_value = "",
      rule_id        = "SD1122",
      error_message  = error_message_str1,
      domain_name   = domain_name
    )}

  # SD1212: Standardized Result in Numeric Format (--STRESN) variable value should be equal to
  # Standardized Result in Character Format (--STRESC) variable value when --STRES
  # If v2 is numeric, v1 must also be numeric and they must be equal
  is_error <- v2_is_numeric & (!v1_is_num | (v1_num != v2_num))
  # Error Reporting ----
  if (any(is_error)) {
    err_rows <- df$rownumber_new[is_error]
    var_name_str <- paste(c(target_var, cond_var), collapse = ", ")

    error_message_str2 <- sprintf("Value of %s should be equal to %s when %s represents a numeric value.", 
                                 target_var, cond_var, cond_var)

    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number     = as.character(err_rows),
      variable_name  = var_name_str,
      original_value = "",
      rule_id        = rule_id,
      error_message  = error_message_str2,
      domain_name   = domain_name
    )
  }

  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }

}
