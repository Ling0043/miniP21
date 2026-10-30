#' @name check_cross_no_records_for_subjects
#' @title Check that subjects meeting a condition have no records in the
#'   current domain
#' @description For subjects in the reference domain (DM) meeting the
#'   condition(s) (e.g. not assigned to an arm, screen failure, not treated,
#'   or missing arm), checks that they have no records in the current domain,
#'   excluding records matching the record-level exclusion condition
#'   (e.g. EPOCH = 'RUN-IN').
#'   Applicable to: SD0079, SD1340.
#'
#' @param main_df The current domain dataset (e.g. EX).
#' @param ref_dfs The reference domain datasets (a named list, DM expected).
#' @param domain_name The name of the current domain.
#' @param ref_domain The subject-universe domain (default "DM").
#' @param target_vars Not used for value lookup (reporting uses USUBJID);
#'   kept for signature consistency.
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param ref_cond_vars Variables for subject conditions (evaluated on
#'   ref_domain, e.g. ARMCD, ACTARMCD).
#' @param ref_cond_ops Operators for subject conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains". Comparison is case-insensitive.
#' @param ref_cond_vals Values for subject conditions.
#' @param ref_logic_op Logical operator to combine subject conditions
#'   ("AND" or "OR"; typically "OR").
#' @param epoch_var Record-level exclusion variable (e.g. "EPOCH").
#'   Pass NULL to disable record-level exclusion.
#' @param epoch_exclude_vals Values of epoch_var for which records are
#'   excluded from firing (e.g. "RUN-IN"; case-insensitive).
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_no_records_for_subjects <- function(main_df, ref_dfs, domain_name, ref_domain = "DM",
                                          target_vars = NULL, ref_vars = NULL, rule_id,
                                          ref_cond_vars = NULL, ref_cond_ops = NULL, ref_cond_vals = NULL,
                                          ref_logic_op = "OR",
                                          epoch_var = "EPOCH", epoch_exclude_vals = NULL, ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Reference domain existence ----
  ref_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL
  if (is.null(ref_df) || nrow(ref_df) == 0 || !"USUBJID" %in% names(ref_df)) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Reference domain %s is missing or empty or lacks USUBJID.", ref_domain),
      domain_name    = domain_name
    ))
  }

  # 3. Helpers ----
  get_val <- function(data, var) {
    x <- trimws(as.character(data[[var]]))
    x[is.na(x)] <- ""
    toupper(x)
  }

  eval_conds <- function(data, vars, ops, vals) {
    if (is.null(vars) || length(vars) == 0) return(rep(FALSE, nrow(data)))
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
    if (ref_logic_op == "AND") Reduce(`&`, idx_list) else Reduce(`|`, idx_list)
  }

  # 4. Subjects meeting the condition(s) in DM ----
  subj_cond <- eval_conds(ref_df, ref_cond_vars, ref_cond_ops, ref_cond_vals)
  subj_hit  <- unique(get_val(ref_df, "USUBJID")[subj_cond])
  subj_hit  <- subj_hit[nzchar(subj_hit)]
  if (length(subj_hit) == 0) return(NULL)


  # 5. Records of these subjects in the current domain (vectorized) ----
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
  err_idx <- which(get_val(main_df, "USUBJID") %in% subj_hit)

  # record-level exclusion (e.g. EPOCH = 'RUN-IN')
  if (!is.null(epoch_var) && epoch_var %in% names(main_df) &&
      !is.null(epoch_exclude_vals)) {
    ep   <- get_val(main_df, epoch_var)
    excl <- ep %in% toupper(epoch_exclude_vals)
    err_idx <- err_idx[!excl[err_idx]]
  }

  if (length(err_idx) == 0) return(NULL)

  # 6. Report (vectorized, one row per error) ----
  cond_desc <- paste(sprintf("%s %s '%s'", ref_cond_vars,
                             ifelse(ref_cond_ops == "missing", "is missing", ref_cond_ops),
                             ifelse(is.na(ref_cond_vals), "", ref_cond_vals)),
                     collapse = paste0(" ", ref_logic_op, " "))

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = "USUBJID",
    original_value = get_val(main_df, "USUBJID")[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("Record present for a subject with %s in %s%s.",
                             cond_desc, ref_domain,
                             if (!is.null(epoch_var) && !is.null(epoch_exclude_vals))
                               sprintf(" (records with %s='%s' are excluded)",
                                       epoch_var, paste(epoch_exclude_vals, collapse = "', '"))
                             else ""),
    domain_name    = domain_name
  )
}
