#' @name check_at_least_one_cond
#' @title Check that at least one variable meets a requirement under conditions
#' @description Validates that for each row meeting specified conditions, 
#' at least one of the target variables is populated (or equals a specific value).
#' 
#' Supports wildcard '--' resolution and vectorized condition logic.
#' Applicable to rules: SD0009, SD1121, SD1333, SD2021 (and previous SD0089 etc).
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of variables to check.
#' @param rule_id The rule ID being checked.
#' @param cond_vars Character vector. Names of the condition columns.
#' @param cond_ops Character vector. Operators: "non_missing", "missing", 
#'   "equal", "not_in", "not_equal".
#' @param cond_vals Character vector. Values for operators.
#' @param logic_op Character string, logic between conditions: "AND" or "OR".
#' @param expected_val Character string, optional. If specified, checks that 
#'   at least one target variable equals this value. If NULL (default), 
#'   checks that at least one target variable is populated.
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
# 1.0      2026-08-13  Zhu Xiuling             Initial version
#
# ==============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_at_least_one_cond <- function(df, domain_name, target_vars, rule_id,
                                     cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                     logic_op = "AND", expected_val = NULL, ...) {

  # 1. Pre-processing ----
  # Resolve wildcards
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  if (!is.null(cond_vars)) {
    actual_cond_vars <- gsub("--", domain_name, cond_vars)
  } else {
    actual_cond_vars <- NULL
  }
  
  # Filter to existing columns
  existing_target_vars <- actual_target_vars[actual_target_vars %in% names(df)]
  if (length(existing_target_vars) == 0) return(NULL)
  
  required_cols <- c(actual_cond_vars)
  required_cols <- required_cols[!is.null(required_cols)]
  if (!all(required_cols %in% names(df))) return(NULL)
  
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
  
  # 3. Core logic: Check At Least One ----
  # For rows meeting the condition, check if ALL target vars fail the requirement
  
  # Determine validity for each target variable
  # If expected_val is set, check equality; otherwise check non-missing
  check_matrix <- sapply(existing_target_vars, function(v) {
    col_data <- df[[v]]
    if (!is.null(expected_val)) {
      !is.na(col_data) & col_data == expected_val
    } else {
      !is.na(col_data) & trimws(as.character(col_data)) != ""
    }
  })
  
  # Handle single column case (sapply simplifies to vector)
  if (is.null(dim(check_matrix))) {
    check_matrix <- matrix(check_matrix, ncol = 1)
  }
  
  # Rows where NONE of the targets meet the requirement
  all_fail <- apply(check_matrix, 1, function(row) !any(row))
  
  # Errors occur where condition is met AND all targets fail
  err_idx <- which(combined_idx & all_fail)
  
  # 4. Error Reporting ----
  if (length(err_idx) > 0) {
    err_rows <- df$rownumber_new[err_idx]
    row_number_str <- paste(err_rows, collapse = ", ")
    var_name_str <- paste(existing_target_vars, collapse = ", ")
    
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
      
      if (!is.null(expected_val)) {
        error_message_str <- sprintf("At least one of %s must equal '%s' when %s.", var_name_str, expected_val, cond_str)
      } else {
        error_message_str <- sprintf("At least one of %s should be populated when %s.", var_name_str, cond_str)
      }
    } else {
      if (!is.null(expected_val)) {
        error_message_str <- sprintf("At least one of %s must equal '%s'.", var_name_str, expected_val)
      } else {
        error_message_str <- sprintf("At least one value of ('%s') should be populated.", var_name_str)
      }
    }
    
    report_df <- report_error(
      row_number     = row_number_str,
      variable_name  = var_name_str,
      original_value = "",
      rule_id        = rule_id,
      error_message  = error_message_str
    )
    
    return(report_df)
  }
  
  return(NULL)
}
