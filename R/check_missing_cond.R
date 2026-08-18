#' @name check_missing_cond
#'
#' @title Check Required Value Under Conditions
#'
#' @description Verifies that a target variable is not missing when one or more
#'   condition variables satisfy specified operators. Supports conditions of
#'   type “non‑missing”, “missing”, “equal to a value”, “not in a set of values”,
#'   and “greater than a numeric threshold”. Conditions can be combined using
#'   AND (default) or OR logic.
#'
#' @details Implements SDTM validation rules where a variable must be populated
#'   depending on other variables’ states. Applicable rules:
#'   SD0016, SD0024, SD0026, SD0027, SD0029, SD0030, SD0032, SD0033, SD0034,
#'   SD0035, SD0036, SD0043, SD0044, SD0045, SD0049, SD0050, SD0087, SD0092,
#'   SD1036, SD1037, SD1065, SD1098, SD1131, SD1209, SD1213, SD1241, SD1265,
#'   SD1268, SD1289, SD1291, SD1292, SD1313, SD1320, SD1342, SD1343, SD1350,
#'   SD1351, SD1369, SD1370, SD1372, SD1454, SD1461, SD1465, SD2005, SD2023,
#'   SD2286.
#'
#' @param df A data frame containing the domain data to check.
#' @param domain_name Character string. Name of the domain (e.g., `"DM"`).
#' @param target_vars Character string. Name of the variable that must not be
#'   missing when the conditions hold.
#' @param cond_vars Character vector. Names of the condition columns. Length
#'   must equal length of `cond_ops` and `cond_vals`.
#' @param cond_ops Character vector. Operators to apply to each condition
#'   column. Allowed values: `"non_missing"` (not NA and not empty string),
#'   `"missing"` (NA or empty string), `"equal"` (exact match, case‑sensitive),
#'   `"not_in"` (value not in a comma‑separated list; the token `__MISSING__`
#'   inside the list excludes missing values), `"greater_than"` (numeric
#'   comparison, NA is treated as not satisfying).
#' @param cond_vals Character vector. Values corresponding to each operator.
#'   Ignored for `"non_missing"` and `"missing"` (use `""` as placeholder).
#'   For `"equal"` a single string, for `"not_in"` a comma‑separated list
#'   (e.g. `"A,B,__MISSING__"`), for `"greater_than"` a numeric string.
#' @param rule_id Character string. Identifier of the validation rule.
#' @param logic_op Character string. Logical operator to combine multiple
#'   conditions. Either `"AND"` (default) or `"OR"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, and `error_message` for each record where
#'   the target variable is missing despite the conditions being met, or
#'   `NULL` if no violations or required columns are missing.
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
# 1.0      2026-06-11  Zhu Xiuling             Initial version
#
# =============================================================================

check_missing_cond <- function(df, domain_name, target_vars, cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                               logic_op = "AND", rule_id, ...) {
  # ---- Existence checks ----
  if (any(grepl("^--", target_vars))) {
    target_vars <- paste0(domain_name, gsub("^--", "", target_vars))
  }
  if (!is.null(cond_vars) && any(grepl("^--", cond_vars))) {
    cond_vars <- paste0(domain_name, gsub("^--", "", cond_vars))
  }
  
  # ---- Existence checks ----
  if (!target_vars %in% names(df)) return(NULL)
  if (!is.null(cond_vars) && !all(cond_vars %in% names(df))) return(NULL)

  # ---- Build condition indices ----
  if (!is.null(cond_vars) && length(cond_vars) > 0) {
  idx_list <- mapply(
    function(var, op, val) {
      x <- df[[var]]
      switch(op,
        non_missing = !is.na(x) & x != "",
        missing     = is.na(x) | x == "",
        equal       = !is.na(x) & x == val,
        not_in = {
          # parse exclusion list, allowing __MISSING__ token
          excl <- trimws(unlist(strsplit(val, ",", fixed = TRUE)))
          if ("__MISSING__" %in% excl) {
            excl <- setdiff(excl, "__MISSING__")
            # non-missing AND not in excl
            (!is.na(x)) & (x != "") & (!x %in% excl)
          } else {
            # only exclude the listed values; missing values are not excluded
            # but we still need to handle them: !x %in% excl returns NA for NA
            # treat NA as satisfying the condition? With rules that say
            # "not in (...)", missing values are typically not in the list,
            # but we follow the explicit condition: if list contains __MISSING__
            # we already handled it. If not, we assume missing does not satisfy
            # "not in" because rule intends a non-missing value to be checked.
            # However, the rules that use not_in always include __MISSING__.
            # For safety we implement: condition TRUE if not missing and not in list.
            (!is.na(x)) & (x != "") & (!x %in% excl)
          }
        },
        greater_than = {
          xn <- suppressWarnings(as.numeric(x))
          (!is.na(xn)) & xn > as.numeric(val)
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

  # Combine conditions
  if (logic_op == "OR") {
    combined_idx <- Reduce(`|`, idx_list)
  } else {
    combined_idx <- Reduce(`&`, idx_list)
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
        },
        greater_than = paste0(var, " > ", val),
        var
      )
    },
    var = cond_vars,
    op  = cond_ops,
    val = cond_vals,
    USE.NAMES = FALSE
  )

    connector <- if (logic_op == "OR") " or " else " and "
    cond_str <- paste(cond_descs, collapse = connector)
  } else {
    combined_idx <- rep(TRUE, nrow(df))
    cond_str <- NULL
  }

  if (!any(combined_idx)) return(NULL)
  
  # ---- Check target missing ----
  target_vals <- df[[target_vars]][combined_idx]
  is_missing <- is.na(target_vals) | as.character(target_vals) == ""
  error_positions <- which(is_missing)
  if (length(error_positions) == 0) return(NULL)

  # ---- Build error message ----
  if (is.null(cond_str)) {
    error_message <- sprintf("Value for %s must not be null.", target_vars)
  } else {
    error_message <- sprintf("Value for %s must not be null when %s", target_vars, cond_str)
  }
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