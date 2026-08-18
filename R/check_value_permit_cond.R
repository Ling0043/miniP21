#' @name check_value_permit_cond
#' @title Check if variable values are in a valid set
#' @description Validates that target variable values belong to a specific set 
#' of allowed values, optionally triggered by condition variables.
#' Supports wildcard '--' resolution and vectorized condition logic.
#' Applicable to rules: SD1128, SD1223, SD1295, SD1296.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of variables to validate.
#' @param rule_id The rule ID being checked.
#' @param valid_values Character string, comma-separated list of allowed 
#' values (e.g., "Y,N" or "ONE,MANY").
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
# 1.0      2026-08-17  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_value_permit_cond <- function(df, domain_name, target_vars, rule_id, 
                               valid_values,
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
  
  # Parse valid values
  allowed_set <- trimws(unlist(strsplit(valid_values, ",", fixed = TRUE)))
  
  # 2. Build condition indices ----
  if (!is.null(actual_cond_vars) && length(actual_cond_vars) > 0) {
    idx_list <- mapply(
      function(var, op, val) {
        x <- df[[var]]
        switch(op,
               non_missing = !is.na(x) & x != "",
               missing     = is.na(x) | x == "",
               equal       = !is.na(x) & x == val,
               not_equal   = is.na(x) | x != val,
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
  # Check only existing target variables
  existing_target_vars <- actual_target_vars[actual_target_vars %in% names(df)]
  if (length(existing_target_vars) == 0) return(NULL)
  
  err_list <- lapply(existing_target_vars, function(v) {
    vals <- df[[v]]
    
    # Errors: Condition met, value is populated, but NOT in allowed set
    is_populated <- !is.na(vals) & trimws(as.character(vals)) != ""
    is_invalid   <- !vals %in% allowed_set
    
    err_mask <- combined_idx & is_populated & is_invalid
    
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
    row_number_str <- paste(unique(err_df$row_idx), collapse = ", ")
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
      
      error_msg <- sprintf("Invalid value for %s when %s. Expected values are: '%s'.", 
                           variable_name_str, cond_str, valid_values)
    } else {
      error_msg <- sprintf("Invalid value for %s. Expected values are: '%s'.", 
                           variable_name_str, valid_values)
    }
    
    report_df <- report_error(
      row_number     = row_number_str,
      variable_name  = variable_name_str,
      original_value = "",
      rule_id        = rule_id,
      error_message  = error_msg
    )
    
    return(report_df)
  }
  
  return(NULL)
}
