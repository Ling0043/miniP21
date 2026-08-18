#' @name check_numeric_range
#' @title Check numeric values against a range condition
#' @description Validates that numeric variables meet specified conditions 
#' (e.g., >= 0, > 0, != 0), optionally triggered by condition variables.
#' 
#' Supports wildcard '--' resolution for domain-specific variables.
#' Applicable to rules: SD0014, SD0015, SD0038, SD0084, SD1135, SD1247, 
#' SD1248, SD1452.
#'
#' @param df Data frame to check.
#' @param domain_name The domain of the dataset.
#' @param target_vars Character vector of variables to validate.
#' @param rule_id The rule ID being checked.
#' @param operator Character string, comparison operator: ">=", "<=", ">", 
#' "<", "==", "!=", "in". Default is ">=".
#' @param threshold Numeric value to compare against. Default is 0.
#' @param cond_vars Character vector. Names of the condition columns. Length
#'   must equal length of `cond_ops` and `cond_vals`.
#' @param cond_ops Character vector. Operators to apply to each condition
#'   column. Allowed values: `"non_missing"`, `"missing"`, `"equal"`, 
#'   `"not_in"`, `"not_equal"`.
#' @param cond_vals Character vector. Values corresponding to each operator.
#'   Ignored for `"non_missing"` and `"missing"` (use `""` as placeholder).
#' @param logic_op Character string, logic between conditions: "AND" or "OR". 
#'   Default is "AND".
#' @param lower Numeric. Lower bound for range check. If provided, `upper` must also be provided.
#' @param upper Numeric. Upper bound for range check. If provided, `lower` must also be provided.
#' @param include_lower Logical. Whether to include the lower bound in the range check.
#' @param include_upper Logical. Whether to include the upper bound in the range check.
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

check_numeric_range <- function(df, domain_name, target_vars, rule_id,
                                operator = NULL, threshold = NULL,
                                cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                logic_op = "AND",
                                lower = NULL, upper = NULL,   # 新增
                                include_lower = TRUE, include_upper = TRUE, ...) {
  
  # 1. Pre-processing ----
  # Resolve wildcards in target variables
  actual_target_vars <- gsub("--", domain_name, target_vars)
  
  # Resolve wildcards in condition variables
  if (!is.null(cond_vars)) {
    actual_cond_vars <- gsub("--", domain_name, cond_vars)
  } else {
    actual_cond_vars <- NULL
  }
  
  # Check if all required columns exist
  required_cols <- c(actual_target_vars, actual_cond_vars)
  required_cols <- required_cols[!is.null(required_cols)]
  
  if (!all(required_cols %in% names(df))) {
    return(NULL)
  }
  
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
    # No conditions: check all rows
    combined_idx <- rep(TRUE, nrow(df))
  }

  if (!any(combined_idx)) {
    return(NULL)
  }

  # 3. Core logic: Numeric validation ----
  # Helper function to evaluate numeric comparison
 check_value <- function(x, op, thresh, low, high, incl_low, incl_high) {
  num_x <- suppressWarnings(as.numeric(x))

  # If both lower and upper bounds are provided, perform range check
  if (!is.null(low) && !is.null(high)) {
    if (incl_low && incl_high) {
      return(!is.na(num_x) & num_x >= low & num_x <= high)
    } else if (incl_low && !incl_high) {
      return(!is.na(num_x) & num_x >= low & num_x < high)
    } else if (!incl_low && incl_high) {
      return(!is.na(num_x) & num_x > low & num_x <= high)
    } else {
      return(!is.na(num_x) & num_x > low & num_x < high)
    }
  }
  
  # If only threshold is provided, perform comparison based on operator
  result <- if (op == ">=") {
    !is.na(num_x) & num_x >= thresh
  } else if (op == "<=") {
    !is.na(num_x) & num_x <= thresh
  } else if (op == ">") {
    !is.na(num_x) & num_x > thresh
  } else if (op == "<") {
    !is.na(num_x) & num_x < thresh
  } else if (op == "==") {
    !is.na(num_x) & num_x == thresh
  } else if (op == "!=") {
    !is.na(num_x) & num_x != thresh
  } else {
    !is.na(num_x)
  }
  result
}
  
  # Collect errors for each target variable
  err_list <- lapply(actual_target_vars, function(v) {
    if (!v %in% names(df)) return(NULL)

    vals <- df[[v]]

    # Identify rows that are:
    # 1. Applicable (condition met)
    # 2. Target variable is populated
    # 3. Target variable violates the numeric condition
    is_populated <- !is.na(vals) & trimws(as.character(vals)) != ""
    is_valid_num <- check_value(vals, operator, threshold, lower, upper, include_lower, include_upper)

    # Errors: applicable, populated, but invalid
    err_mask <- combined_idx & is_populated & !is_valid_num

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
                if (length(excl_clean) == 0) {
                  paste0(var, " is not missing")
                } else {
                  paste0(var, " is not in ('", paste(excl_clean, collapse = "','"), "') and is not missing")
                }
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
      error_msg <- sprintf("Value of %s must be %s %s when %s.",
                           variable_name_str, operator, threshold, cond_str)
    } else {
      error_msg <- sprintf("Value of %s must be %s %s.",
                           variable_name_str, operator, threshold)
    }

    if (!is.null(lower) && !is.null(upper)) {
      boundary_str <- paste0(if (include_lower) "[" else "(", lower, ", ", upper, if (include_upper) "]" else ")")
      error_msg <- sprintf("Value of %s must be within %s when %s.", 
                           variable_name_str, boundary_str, cond_str)
    }

    report_df <- report_error(
      row_number     = as.character(err_df$row_idx),
      variable_name  = variable_name_str,
      original_value = err_df$orig_val,
      rule_id        = rule_id,
      error_message  = error_msg
    )
    
    return(report_df)
  }
  
  return(NULL)
}