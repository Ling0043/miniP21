#' @name check_be_null_cond
#'
#' @title Check Prohibited (Must Be Missing) Under Conditions
#'
#' @description Verifies that a target variable is missing (NA or empty string)
#'   when one or more condition variables meet specified criteria. Supports
#'   conditions of type “non‑missing”, “missing”, “equal to a value” and “not
#'   in a set of values”. Conditions can be combined using AND (default) or OR
#'   logic.
#'
#' @details Implements SDTM validation rules where a variable must be blank
#'   given the state of other variables. Applicable rules: SD1010, SD1013,
#'   SD1019, SD1066, SD1067, SD1123, SD1137, SD1238, SD1251.
#'
#' @param df A data frame containing the domain data to check.
#' @param domain_name Character string. Name of the domain to which the columns belong.
#' @param target_vars Character string. Name of the variable that must be
#'   missing when the conditions are met.
#' @param cond_vars Character vector. Names of the condition columns. Length
#'   must equal length of `cond_ops` and `cond_vals`.
#' @param cond_ops Character vector. Operators to apply to each condition
#'   column. Allowed values: `"non_missing"` (not NA and not empty string),
#'   `"missing"` (NA or empty string), `"equal"` (exact match, case‑sensitive),
#'   `"not_in"` (value not in a comma‑separated list; `__MISSING__` token
#'   excludes missing values).
#' @param cond_vals Character vector. Values corresponding to each operator.
#'   Ignored for `"non_missing"` and `"missing"` (use `""` as placeholder).
#'   For `"equal"` a single string, for `"not_in"` a comma‑separated list
#'   (e.g. `"A,B,__MISSING__"`).
#' @param rule_id Character string. Identifier of the validation rule.
#' @param logic_op Character string. Logical operator to combine multiple
#'   conditions. Either `"AND"` (default) or `"OR"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, and `error_message` for each record where
#'   the target variable is **not** missing despite the conditions being met,
#'   or `NULL` if no violations or required columns are missing.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-07-15  Zhu Xiuling             Initial version
#
# =============================================================================

check_be_null_cond <- function(df, domain_name, target_vars, cond_vars, cond_ops, cond_vals,
                               logic_op = "AND", rule_id, ...) {
  # ---- Existence checks ----
  if (any(grepl("^--", target_vars))) {
    target_vars <- gsub("^--", domain_name, target_vars)
    cond_vars <- gsub("^--", domain_name, cond_vars)
  }

  if (!all(cond_vars %in% names(df)) || !target_vars %in% names(df)) {
    return(NULL)
  }

  # ---- Build condition indices ----
  idx_list <- mapply(
    function(var, op, val) {
      x <- df[[var]]
      switch(op,
        non_missing = !is.na(x) & x != "",
        missing     = is.na(x) | x == "",
        equal       = !is.na(x) & x == val,
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
    var = cond_vars,
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

  if (!any(combined_idx)) {
    return(NULL)
  }

  # ---- Check that target variable IS missing ----
  target_vals <- df[[target_vars]][combined_idx]
  is_not_missing <- !is.na(target_vals) & as.character(target_vals) != ""
  error_positions <- which(is_not_missing)

  if (length(error_positions) == 0) {
    return(NULL)
  }

  # ---- Build human‑readable condition description ----
  cond_descs <- mapply(
    function(var, op, val) {
      switch(op,
        non_missing = paste0(var, " is provided"),
        missing     = paste0(var, " is missing"),
        equal       = paste0(var, "='", val, "'"),
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
    var = cond_vars,
    op  = cond_ops,
    val = cond_vals,
    USE.NAMES = FALSE
  )

  connector <- if (logic_op == "OR") " or " else " and "
  cond_str <- paste(cond_descs, collapse = connector)

  error_message <- sprintf("Value of %s must be null when %s",
                           target_vars, cond_str)

  # ---- Report errors ----
  idx <- which(combined_idx)[error_positions]
  original_value <- as.character(df[[target_vars]][idx])

  report_df <- report_error(
    row_number    = as.character(idx),
    variable_name = target_vars,
    original_value = original_value,
    rule_id       = rule_id,
    error_message = error_message
  )

  return(report_df)
}