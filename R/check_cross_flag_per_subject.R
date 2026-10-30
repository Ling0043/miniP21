#' @name check_cross_flag_per_subject
#' @title Check that each subject has at least one flagged record in the
#'   specified domain(s)
#' @description For each subject in the reference domain (DM), excluding
#'   subjects meeting the exemption conditions (e.g. screening failure, not
#'   assigned to an arm, not treated), checks that at least one record with
#'   the flag variable equal to flag_val exists in the flag domain(s).
#'   Two modes:
#'   \itemize{
#'     \item flag_domains = NULL: the current domain only (per-domain check,
#'       e.g. SD0006 run for EG, LB, LC, MB, MS, PC, VS).
#'     \item flag_domains = character vector: the listed domains combined
#'       (any-domain check, e.g. SD1478 anchored on DM).
#'   }
#'   Applicable to: SD0006, SD1478.
#'
#' @param main_df The current domain dataset (the flag domain for SD0006,
#'   DM for SD1478).
#' @param ref_dfs The reference domain datasets (a named list). Must contain
#'   the exemption domain (DM); for SD1478 also the flag domains.
#' @param domain_name The name of the current domain.
#' @param ref_domain The exemption/subject-universe domain (default "DM").
#' @param target_vars The flag variable to check (supports "--" wildcard,
#'   e.g. "--BLFL", "--LOBXFL").
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param flag_val The value the flag variable must have (default "Y";
#'   comparison is case-insensitive after trimming).
#' @param flag_domains Domains to search for flagged records. NULL means the
#'   current domain only; otherwise a character vector of domain names
#'   (looked up in ref_dfs). Domains missing from ref_dfs are skipped; if
#'   none is available, one dataset-level error is reported.
#' @param ref_cond_vars Variables for exemption conditions (evaluated on
#'   ref_domain, e.g. ARMCD, ACTARMCD, ARMNRS).
#' @param ref_cond_ops Operators for exemption conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains". Comparison is case-insensitive.
#' @param ref_cond_vals Values for exemption conditions.
#' @param ref_logic_op Logical operator to combine exemption conditions
#'   ("AND" or "OR"; typically "OR").
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_flag_per_subject <- function(main_df, ref_dfs, domain_name, ref_domain = "DM",
                                   target_vars, ref_vars = NULL, rule_id,
                                   flag_val = "Y", flag_domains = NULL,
                                   ref_cond_vars = NULL, ref_cond_ops = NULL, ref_cond_vals = NULL,
                                   ref_logic_op = "OR", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars  <- gsub("--", domain_name, target_vars)
  if (is.null(flag_domains)) flag_domains <- domain_name
  ref_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL

  # 3. Reference (exemption) domain existence ----
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

  # 4. Helpers ----
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

  # 5. Subject universe (DM) minus exempt subjects ----
  subj_all    <- get_val(ref_df, "USUBJID")
  exempt_flag <- eval_conds(ref_df, ref_cond_vars, ref_cond_ops, ref_cond_vals)
  subj_check  <- setdiff(unique(subj_all[nzchar(subj_all) & !exempt_flag]),
                         unique(subj_all[exempt_flag]))
  if (length(subj_check) == 0) return(NULL)

  # 6. Subjects having the flag in any of the flag domains ----
  flagged <- character(0)
  domains_used <- character(0)
  for (d in flag_domains) {
    df <- if (!is.null(ref_dfs)) ref_dfs[[d]] else NULL
    if (is.null(df) || nrow(df) == 0 || !target_vars[1] %in% names(df) ||
        !"USUBJID" %in% names(df)) next
    fv <- get_val(df, target_vars[1])
    flagged <- union(flagged, unique(get_val(df, "USUBJID")[fv == toupper(flag_val)]))
    domains_used <- c(domains_used, d)
  }

  if (length(domains_used) == 0) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("None of the flag domain(s) %s is available in the input data.",
                               paste(flag_domains, collapse = ", ")),
      domain_name    = domain_name
    ))
  }

  # 7. Core check (vectorized) ----
  err_subj <- setdiff(subj_check, flagged)
  if (length(err_subj) == 0) return(NULL)

  # 8. Report (one row per subject) ----
  # row number: the subject's row in main_df when available, else "-"
  main_subj <- get_val(main_df, "USUBJID")
  row_nums  <- vapply(err_subj, function(s) {
    idx <- which(main_subj == s)
    if (length(idx) > 0) as.character(main_df$rownumber_new[idx[1]]) else "-"
  }, character(1), USE.NAMES = FALSE)

  report_error(
    row_number     = row_nums,
    variable_name  = "USUBJID",
    original_value = err_subj,
    rule_id        = rule_id,
    error_message  = sprintf("Subject has no record with %s='%s' in %s.",
                             target_vars[1], flag_val,
                             paste(domains_used, collapse = ", ")),
    domain_name    = domain_name
  )
}