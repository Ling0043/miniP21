#' @name check_cross_subject_has_record
#' @title Check that filtered subjects have at least one record in a reference domain
#' @description For subjects of the current domain (optionally filtered),
#'   checks whether each subject (USUBJID) has at least one record in the
#'   reference domain (optionally filtered by reference-side conditions).
#'   Applicable to: SD0070, SD1240, SD1374, SD1377, SD2288.
#'
#' @param main_df The current domain dataset (subject-level, e.g. DM).
#' @param ref_dfs The reference domain datasets (a named list).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain.
#' @param target_vars The subject identifier variable(s) to check
#'   (e.g. "USUBJID"; "--" wildcard supported).
#' @param ref_vars The corresponding subject identifier variable(s) in the
#'   reference domain (defaults to target_vars).
#' @param rule_id The ID of the rule being checked.
#' @param cond_vars Variables for subject inclusion conditions (on main_df).
#' @param cond_ops Operators for subject inclusion conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains".
#' @param cond_vals Values for subject inclusion conditions.
#' @param ref_cond_vars Variables for reference record conditions (on ref_df).
#' @param ref_cond_ops Operators for reference record conditions. Supported
#'   operators are the same as cond_ops.
#' @param ref_cond_vals Values for reference record conditions.
#' @param logic_op Logical operator to combine conditions ("AND" or "OR"),
#'   applied to both main and reference conditions.
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error (one row per subject without
#'   matching records) containing row_number, variable_name, original_value,
#'   rule_id, error_message and domain_name. Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_subject_has_record <- function(main_df, ref_dfs, domain_name, ref_domain,
                                     target_vars, ref_vars = NULL, rule_id,
                                     cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                     ref_cond_vars = NULL, ref_cond_ops = NULL, ref_cond_vals = NULL,
                                     logic_op = "AND", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  if (is.null(ref_vars)) ref_vars <- target_vars
  ref_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL

  # defensive row number
  if (!"rownumber_new" %in% names(main_df)) {
    main_df <- main_df %>% mutate(rownumber_new = dplyr::row_number())
  }

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

  # 5. Subjects to check (filtered by main-side conditions) ----
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)
  if (!any(rows_to_check)) return(NULL)

  # 6. Reference records (filtered by ref-side conditions) ----
  ref_keep <- eval_conds(ref_df, ref_cond_vars, ref_cond_ops, ref_cond_vals)
  ref_subj <- unique(trimws(as.character(ref_df[[ref_vars[1]]][ref_keep])))
  ref_subj <- ref_subj[nzchar(ref_subj)]

  # 7. Core check (vectorized) ----
  cur_subj <- trimws(as.character(main_df[[target_vars[1]]]))
  err_idx  <- which(rows_to_check & !is.na(cur_subj) & nzchar(cur_subj) & !cur_subj %in% ref_subj)
  if (length(err_idx) == 0) return(NULL)

  # 8. Report (vectorized, one row per subject) ----
  tvar_str  <- paste(target_vars, collapse = ", ")
  ref_str   <- sprintf("%s(%s)", ref_domain, paste(ref_vars, collapse = ", "))
  ref_desc  <- NULL
  if (!is.null(ref_cond_vars)) ref_desc <- paste0(" with ",
    paste(sprintf("%s is %s", ref_cond_vars, ref_cond_ops), collapse = " and "))
  ref_str_full <- paste0(ref_str, if (is.null(ref_desc)) "" else paste0(" (", sub("^ with ", "", ref_desc), ")"))

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = tvar_str,
    original_value = cur_subj[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("No record found in %s for subject %s", ref_str_full, cur_subj[err_idx]),
    domain_name    = domain_name
  )
}
