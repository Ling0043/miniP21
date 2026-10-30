#' @name check_cross_lookup_match
#' @title Check that a variable combination exists in a reference dataset
#' @description For rows of the current domain (optionally filtered), checks
#'   whether the combination of target_vars exists among the combinations of
#'   ref_vars in the reference dataset.
#'   Applicable to: SD0066, SD0067, SD0068, SD0071, SD1012, SD1014, SD1015,
#'   SD1016, SD1017, SD1018, SD1023, SD2002, SD2003, SD1354, SD1378, SD1379.
#'
#' @param main_df The current domain dataset.
#' @param ref_dfs The reference domain datasets (a named list).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain.
#' @param target_vars The variables in the current domain to check.
#' @param ref_vars The corresponding variables in the reference domain.
#' @param rule_id The ID of the rule being checked.
#' @param cond_vars Variables for inclusion conditions.
#' @param cond_ops Operators for inclusion conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains".
#' @param cond_vals Values for inclusion conditions.
#' @param logic_op Logical operator to combine conditions ("AND" or "OR").
#' @param distinct_check If TRUE, rows with duplicated key combinations are
#'   reported only once per distinct key (useful for trial design domains
#'   such as TA/TE, e.g. SD1354, SD1378, SD1379).
#' @param skip_missing_key If TRUE, rows whose key is entirely missing are
#'   skipped (useful for trial design domains where blank ARMCD/ETCD rows
#'   should not be reported).
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-10-04  Zhu Xiuling             Initial version
# 2.0      2026-10-26  Zhu Xiuling             Added distinct_check and skip_missing_key parameters for trial design domain checks
# =============================================================================

check_cross_lookup_match <- function(main_df, ref_dfs, domain_name, ref_domain,
                               target_vars, ref_vars = NULL, rule_id,
                               cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                               logic_op = "AND",
                               distinct_check = FALSE, skip_missing_key = FALSE, ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  if (is.null(ref_vars)) ref_vars <- target_vars
  ref_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL

  # 3. Reference domain existence ----
  if (is.null(ref_df) || nrow(ref_df) == 0) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Reference domain %s is missing or empty.", ref_domain),
      domain_name    = domain_name
    ))
  }

  # 4. Helpers ----
  make_key <- function(data, vars) {
    vals <- lapply(vars, function(v) {
      x <- trimws(as.character(data[[v]]))
      x[is.na(x) | x == ""] <- "<NA>"
      x
    })
    do.call(paste, c(vals, sep = " | "))
  }

  eval_conds <- function(data, vars, ops, vals) {
    if (is.null(vars) || length(vars) == 0) return(rep(TRUE, nrow(data)))
    idx_list <- mapply(function(var, op, val) {
      x <- trimws(as.character(data[[var]]))
      x[is.na(x)] <- ""
      switch(op,
             missing      = x == "",
             non_missing  = x != "",
             equal        = x == val,
             not_equal    = x != val,
             contains     = grepl(val, x, fixed = TRUE),
             not_contains = !grepl(val, x, fixed = TRUE),
             stop("Unknown operator: ", op))
    }, var = vars, op = ops, val = vals,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    if (logic_op == "OR") Reduce(`|`, idx_list) else Reduce(`&`, idx_list)
  }

  # 5. Rows to check ----
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)
  if (!any(rows_to_check)) return(NULL)

  # 6. Core lookup (vectorized) ----
  ref_keys <- unique(make_key(ref_df, ref_vars))
  cur_keys <- make_key(main_df, target_vars)

  if (skip_missing_key) {
    rows_to_check <- rows_to_check & cur_keys != "<NA>"
    if (!any(rows_to_check)) return(NULL)
  }

  err_idx <- which(rows_to_check & !cur_keys %in% ref_keys)

  # report only once per distinct key when requested
  if (distinct_check && length(err_idx) > 0) {
    err_idx <- err_idx[!duplicated(cur_keys[err_idx])]
  }

  if (length(err_idx) == 0) return(NULL)

  # 7. Report (vectorized, one row per error) ----
  tvars_str <- paste(target_vars, collapse = ", ")
  ref_str   <- sprintf("%s(%s)", ref_domain, paste(ref_vars, collapse = ", "))
  filter_desc <- NULL
  if (!is.null(cond_vars)) filter_desc <- paste0("only checked when ",
    paste(sprintf("%s is %s", cond_vars, ifelse(cond_ops == "missing", "missing", cond_ops)), collapse = " and "))
  filter_str <- if (is.null(filter_desc)) "." else paste0(" (", paste(filter_desc, collapse = "; "), ").")

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = tvars_str,
    original_value = cur_keys[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s value(s) not found in %s%s", tvars_str, ref_str, filter_str),
    domain_name    = domain_name
  )
}
