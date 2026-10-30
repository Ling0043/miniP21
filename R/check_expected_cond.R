#' @name check_expected_cond
#' @title Check variable value consistency based on conditions
#' @description Validates that a target variable equals a specific value 
#' (or is in a set of values) when specified conditions are met.
#' 
#' Supports wildcard '--' resolution and vectorized condition logic.
#' Applicable to rules: SD0042, SD0023, SD0090, SD0091, SD1045, SD1046, 
#' SD1062, SD1249, SD1314, SD2004, SD2240, SD2241, SD2242, SD2243, 
#' SD2256, SD2266, SD2244.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of variables to validate.
#' @param rule_id The rule ID being checked.
#' @param expected_values Character string, comma-separated list of allowed 
#' values for the target variable (e.g., "Y" or "GENC,ISO 3166-1 alpha-3").
#' @param cond_vars Character vector. Names of the condition columns.
#' @param cond_ops Character vector. Operators: "non_missing", "missing", 
#'   "equal", "not_in", "not_equal".
#' @param cond_vals Character vector. Values for operators.
#' @param logic_op Character string, logic between conditions: "AND" or "OR".
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
# 1.0      2026-07-08  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_expected_cond <- function(df, domain_name, target_vars, rule_id,
                             expected_values,
                             cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                             logic_op = "AND", ...) {
  
  # 1. Pre-processing ----
  # Resolve wildcards
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  if (!is.null(cond_vars)) {
    actual_cond_vars <- gsub("--", domain_name, cond_vars)
  } else {
    actual_cond_vars <- NULL
  }
  
  # Check if required columns exist
  required_cols <- c(actual_target_vars, actual_cond_vars)
  required_cols <- required_cols[!is.null(required_cols)]
  
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }
  
  # Parse expected values
  expected_set <- trimws(unlist(strsplit(expected_values, ",", fixed = TRUE)))
  
  # 2. Build condition indices ----
  if (!is.null(actual_cond_vars) && length(actual_cond_vars) > 0) {
    idx_list <- mapply(
      function(var, op, val) {
        x <- df[[var]]
        switch(op,
               non_missing = !is.na(x) & x != "",
               missing     = is.na(x) | x == "",
               equal       = !is.na(x) & x == val,
               not_equal   = !is.na(x) & x != val,
               not_in = {
                 excl <- trimws(unlist(strsplit(val, ",", fixed = TRUE)))
                 if ("__MISSING__" %in% excl) {
                   excl <- setdiff(excl, "__MISSING__")
                   (!is.na(x)) & (x != "") & (!x %in% excl)
                 } else {
                   (!is.na(x)) & (x != "") & (!x %in% excl)
                 }
               },
               stop("Unknown operator: ", op)
        )
      },
      var = actual_cond_vars,
      op  = cond_ops,
      val = cond_vals,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
    
    if (logic_op == "OR") {
      combined_idx <- Reduce(`|`, idx_list)
    } else {
      combined_idx <- Reduce(`&`, idx_list)
    }
  } else {
    combined_idx <- rep(TRUE, nrow(df))
  }
  
  if (!any(combined_idx)) return(NULL)
  
  # 3. Core logic: Value Validation ----
  existing_target_vars <- actual_target_vars[actual_target_vars %in% names(df)]
  if (length(existing_target_vars) == 0) return(NULL)
  
  err_list <- lapply(existing_target_vars, function(v) {
    vals <- df[[v]]
    
    # Convert to character for consistent comparison
    vals_char <- as.character(vals)
    
    # Errors: Condition met, value is populated, but NOT in expected set
    is_populated <- !is.na(vals) & trimws(vals_char) != ""
    is_invalid   <- !vals_char %in% expected_set
    
    # Note: If value is missing, it is technically not equal to expected 'Y' or specific value.
    # Usually these rules imply the variable must be populated with that value.
    # So missing values are also errors.
    err_mask <- combined_idx & is_invalid
    
    if (any(err_mask)) {
      data.frame(
        row_idx  = df$rownumber_new[err_mask],
        var_name = v,
        orig_val = vals[err_mask]
      )
    } else {
      NULL
    }
  })
  
  err_df <- bind_rows(err_list)
  
  # 4. Error Reporting ----
  if (nrow(err_df) > 0) {
    variable_name_str <- paste(unique(err_df$var_name), collapse = ", ")
    
    # Build human-readable condition description
    if (!is.null(actual_cond_vars) && length(actual_cond_vars) > 0) {
      cond_descs <- mapply(
        function(var, op, val) {
          switch(op,
                 non_missing = paste0(var, " is provided"),
                 missing     = paste0(var, " is missing"),
                 equal       = paste0(var, "='", val, "'"),
                 not_equal   = paste0(var, "!='", val, "'"),
                 not_in = {
                   excl <- trimws(unlist(strsplit(val, ",", fixed = TRUE)))
                   excl_clean <- setdiff(excl, "__MISSING__")
                   if ("__MISSING__" %in% excl) {
                     if (length(excl_clean) == 0) paste0(var, " is not missing")
                     else paste0(var, " is not in ('", paste(excl_clean, collapse = "','"), "') and is not missing")
                   } else {
                     paste0(var, " is not in ('", paste(excl_clean, collapse = "','"), "')")
                   }
                 }
          )
        },
        var = actual_cond_vars,
        op  = cond_ops,
        val = cond_vals,
        USE.NAMES = FALSE
      )
      connector <- if (logic_op == "OR") " or " else " and "
      cond_str <- paste(cond_descs, collapse = connector)
      
      error_msg <- sprintf("Value of %s must be '%s' when %s.", 
                           variable_name_str, expected_values, cond_str)
    } else {
      error_msg <- sprintf("Value of %s must be '%s'.", 
                           variable_name_str, expected_values)
    }
    
    report_df <- report_error(
      row_number     = as.character(err_df$row_idx),
      variable_name  = variable_name_str,
      original_value = as.character(err_df$orig_val),
      rule_id        = rule_id,
      error_message  = error_msg,
      domain_name   = domain_name
    )
    
    return(report_df)
  }
  
  return(NULL)
}