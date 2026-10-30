#' @name check_cross_condition_value
#' @title Check that a flag variable equals an expected value when the subject
#'   has a qualifying record in a reference domain
#' @description For subjects of the current domain, checks whether the target
#'   variable equals the expected value when the subject has at least one
#'   record in the reference domain (optionally filtered by reference-side
#'   conditions).
#'   Applicable to: SD1252, SD1253, SD1254, SD1255, SD1256.
#'
#' @param main_df The current domain dataset (subject-level, e.g. DM).
#' @param ref_dfs The reference domain datasets (a named list).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain.
#' @param target_vars The flag variable in the current domain to check
#'   (e.g. "DTHFL").
#' @param ref_vars The subject identifier variable(s) in the reference domain
#'   (e.g. "USUBJID"; defaults to target_vars).
#' @param rule_id The ID of the rule being checked.
#' @param expected_val The value the target variable must equal when a
#'   qualifying reference record exists (e.g. "Y").
#' @param ref_cond_vars Variables for reference record conditions (on ref_df).
#' @param ref_cond_ops Operators for reference record conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains".
#' @param ref_cond_vals Values for reference record conditions.
#' @param logic_op Logical operator to combine reference conditions
#'   ("AND" or "OR").
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_condition_value <- function(main_df, ref_dfs, domain_name, ref_domain,
                                     target_vars, ref_vars = NULL, rule_id,
                                     expected_val = "Y",
                                     ref_cond_vars = NULL, ref_cond_ops = NULL, ref_cond_vals = NULL,
                                     logic_op = "AND", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  if (is.null(ref_vars)) ref_vars <- "USUBJID"
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

  # 5. Subjects with qualifying reference records ----
  ref_keep <- eval_conds(ref_df, ref_cond_vars, ref_cond_ops, ref_cond_vals)
  ref_subj <- unique(trimws(as.character(ref_df[[ref_vars[1]]][ref_keep])))
  ref_subj <- ref_subj[nzchar(ref_subj)]

  # 6. Core check (vectorized) ----
  cur_subj <- trimws(as.character(main_df$USUBJID))
  cur_flag <- trimws(as.character(main_df[[target_vars[1]]]))
  cur_flag[is.na(cur_flag)] <- ""
  err_idx <- which(!is.na(cur_subj) & nzchar(cur_subj) &
                     cur_subj %in% ref_subj & cur_flag != expected_val)
  if (length(err_idx) == 0) return(NULL)

  # 7. Report (vectorized, one row per error) ----
  ref_desc <- NULL
  if (!is.null(ref_cond_vars)) ref_desc <- paste0(" when ",
    paste(sprintf("%s(%s) is %s '%s'", ref_domain, ref_cond_vars, ref_cond_ops, ref_cond_vals),
          collapse = " and "))
  ref_str <- sprintf("%s(%s)", ref_domain, paste(ref_vars, collapse = ", "))

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = target_vars[1],
    original_value = cur_flag[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s must equal '%s' when a record exists in %s%s",
                             target_vars[1], expected_val, ref_str,
                             if (is.null(ref_desc)) "" else sub(" when ", " with ", ref_desc)),
    domain_name    = domain_name
  )
}