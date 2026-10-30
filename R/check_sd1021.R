#' @name check_sd1021
#' @title Check for unexpected character values
#' @description Validates that character values do not contain leading space 
#' characters, carriage returns, or consist of a single '.' as the entire value.
#' The exceptions are COVALn and TSVALn variables, which are excluded from the check.
#' Applicable to rule: SD1021.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset (e.g., "DM", "LB").
#' @param target_vars Character vector (Optional). If provided, only these variables 
#' are checked. If NULL (default), all character variables in the dataset are checked.
#' @param exclude_pattern Character string. Regex pattern of variable names to exclude 
#' from the check. Default is "^(COVAL|TSVAL)\\d*$" (i.e., COVALn and TSVALn).
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
# 1.0      2026-08-28  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_sd1021 <- function(df, domain_name, target_vars = NULL, 
                         exclude_pattern = "^(COVAL|TSVAL)\\d*$", rule_id, ...) {

  # 2. Pre-processing ----
  # Resolve wildcards if target_vars provided
  if (!is.null(target_vars)) {
    actual_target_vars <- gsub("--", domain_name, target_vars)
    cols_to_check <- actual_target_vars[actual_target_vars %in% names(df)]
  } else {
    # Scan all columns in the dataset
    cols_to_check <- names(df)
  }

  if (length(cols_to_check) == 0) return(NULL)
  
  # Keep only character-like columns (character or factor)
  cols_to_check <- cols_to_check[sapply(df[cols_to_check], function(x) {
    is.character(x) || is.factor(x)
  })]
  
  # Exclude COVALn and TSVALn variables per rule exception
  cols_to_check <- cols_to_check[!grepl(exclude_pattern, cols_to_check)]
  
  # Exclude helper column
  cols_to_check <- setdiff(cols_to_check, "rownumber_new")
  
  if (length(cols_to_check) == 0) return(NULL)
  
  # 3. Core logic: Check each column ----
  # Violations:
  #   a) Leading space: value starts with " "
  #   b) Carriage return: value contains "\r" or "\n"
  #   c) Entire value is "."
  
  err_list <- lapply(cols_to_check, function(v) {
    col_char <- trimws(as.character(df[[v]]), which = "right") # keep leading to detect
    
    is_populated <- !is.na(df[[v]]) & trimws(as.character(df[[v]])) != ""
    
    has_leading_space <- is_populated & grepl("^ ", as.character(df[[v]]))
    has_carriage_ret  <- is_populated & grepl("[\r\n]", as.character(df[[v]]))
    is_dot_value      <- is_populated & trimws(as.character(df[[v]])) == "."
    
    is_error <- has_leading_space | has_carriage_ret | is_dot_value
    
    if (any(is_error)) {
      data.frame(
        rownumber_new   = df$rownumber_new[is_error],
        variable_name   = v,
        original_value  = as.character(df[[v]][is_error]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })
  
  err_df <- bind_rows(err_list)
  
  # 4. Error Reporting ----
  if (nrow(err_df) > 0) {

    report_df <- report_error(
      row_number     = as.character(err_df$rownumber_new),
      variable_name  = err_df$variable_name,
      original_value = err_df$original_value,
      rule_id        = rule_id,
      error_message  = "Character values should not have leading space, carriage return, or '.' as an entire value.",
      domain_name   = domain_name
    )
    
    return(report_df)
  }
  
  return(NULL)
}
