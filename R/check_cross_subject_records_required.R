#' @name check_cross_subject_records_required
#' @title Check that subjects meeting a condition in the current domain have
#'   supporting records in the target domain
#' @description For subjects having at least one record in the current domain
#'   matching the source condition(s) (e.g. SSSTRESC='DEAD', DSDECOD
#'   in ('PROTOCOL VIOLATION','PROTOCOL DEVIATION')), checks that the same
#'   subject has at least one record in the target domain matching the target
#'   condition(s) (any record if no target condition is given).
#'   Applicable to: SD1316, SD1318.
#'
#' @param main_df The source domain dataset (e.g. SS, DS).
#' @param ref_dfs The reference domain datasets (a named list, must contain
#'   the target domain).
#' @param domain_name The name of the current (source) domain.
#' @param ref_domain The target domain where supporting records are required
#'   (e.g. "DS", "DV").
#' @param target_vars Not used for value lookup (reporting uses USUBJID);
#'   kept for signature consistency.
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param cond_vars Variables for the source condition (evaluated on
#'   main_df, e.g. SSSTRESC, DSDECOD).
#' @param cond_ops Operators for the source condition. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains". Comparison is case-insensitive.
#' @param cond_vals Values for the source condition.
#' @param logic_op Logical operator to combine source conditions
#'   ("AND" or "OR"; typically "OR" when one variable takes several values).
#' @param target_cond_vars Variables for the target condition (evaluated on
#'   the target domain, e.g. DSDECOD). NULL means any record in the target
#'   domain counts.
#' @param target_cond_ops Operators for the target condition (same set as
#'   cond_ops).
#' @param target_cond_vals Values for the target condition.
#' @param target_logic_op Logical operator to combine target conditions
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

check_cross_subject_records_required <- function(main_df, ref_dfs, domain_name, ref_domain,
                                           target_vars = NULL, ref_vars = NULL, rule_id,
                                           cond_vars, cond_ops, cond_vals,
                                           logic_op = "OR",
                                           target_cond_vars, target_cond_ops,
                                           target_cond_vals, target_logic_op = "OR", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Target domain existence ----
  tgt_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL
  if (is.null(tgt_df) || nrow(tgt_df) == 0 || !"USUBJID" %in% names(tgt_df)) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Target domain %s is missing or empty or lacks USUBJID.", ref_domain),
      domain_name    = domain_name
    ))
  }

  # 3. Helpers ----
  get_val <- function(data, var) {
    x <- trimws(as.character(data[[var]]))
    x[is.na(x)] <- ""
    toupper(x)
  }

  eval_conds <- function(data, vars, ops, vals, logic) {
    if (is.null(vars) || length(vars) == 0) return(rep(TRUE, nrow(data)))
    idx_list <- mapply(function(var, op, val) {
      x <- get_val(data, var)
      switch(op,
             missing      = x == "",
             non_missing  = x != "",
             equal        = x == toupper(val),
             not_equal    = x != toupper(val),
             contains     = grepl(toupper(val), x, fixed = TRUE),
             not_contains = !grepl(toupper(val), x, fixed = TRUE),
             stop("Unknown operator: ", op))
    }, var = vars, op = ops, val = vals,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
    if (logic == "AND") Reduce(`&`, idx_list) else Reduce(`|`, idx_list)
  }

  # 4. Subjects meeting the source condition(s) in main_df ----
  if (!"USUBJID" %in% names(main_df)) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("USUBJID not found in %s.", domain_name),
      domain_name    = domain_name
    ))
  }

  src_cond  <- eval_conds(main_df, cond_vars, cond_ops, cond_vals, logic_op)
  subj_src  <- unique(get_val(main_df, "USUBJID")[src_cond])
  subj_src  <- subj_src[nzchar(subj_src)]
  if (length(subj_src) == 0) return(NULL)

  # 5. Subjects having qualifying records in the target domain (vectorized) ----
  tgt_cond  <- eval_conds(tgt_df, target_cond_vars, target_cond_ops, target_cond_vals, target_logic_op)
  subj_tgt  <- unique(get_val(tgt_df, "USUBJID")[tgt_cond])
  subj_tgt  <- subj_tgt[nzchar(subj_tgt)]

  # 6. Core check (vectorized) ----
  err_subj <- setdiff(subj_src, subj_tgt)
  if (length(err_subj) == 0) return(NULL)

  # 7. Report (one row per subject; row number from the subject's source record) ----
  main_subj <- get_val(main_df, "USUBJID")
  row_nums  <- vapply(err_subj, function(s) {
    idx <- which(main_subj == s)
    if (length(idx) > 0) as.character(main_df$rownumber_new[idx[1]]) else "-"
  }, character(1), USE.NAMES = FALSE)


  cond_desc <- if (is.null(cond_vars)) "the specified condition" else
    paste(sprintf("%s %s '%s'", cond_vars,
                  ifelse(cond_ops == "missing", "is missing", cond_ops),
                  ifelse(is.na(cond_vals), "", cond_vals)),
          collapse = paste0(" ", logic_op, " "))

  report_error(
    row_number     = row_nums,
    variable_name  = "USUBJID",
    original_value = err_subj,
    rule_id        = rule_id,
    error_message  = sprintf("Subject has record(s) with %s in %s but no supporting record in %s%s.",
                             cond_desc, domain_name, ref_domain,
                             if (!is.null(target_cond_vars))
                               sprintf(" with %s", paste(sprintf("%s %s '%s'",
                                 target_cond_vars, target_cond_ops, target_cond_vals),
                                 collapse = paste0(" ", target_logic_op, " ")))
                             else ""),
    domain_name    = domain_name
  )
}
