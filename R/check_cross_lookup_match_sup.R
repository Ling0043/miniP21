#' @name check_cross_lookup_match_sup
#' @title Check that a dynamically referenced record exists in its target domain
#' @description For rows of the current domain (optionally filtered), the
#'   referenced record is identified row-wise: target domain by RDOMAIN,
#'   subject by USUBJID or APID, referenced variable name by IDVAR, and the
#'   referenced value by IDVARVAL. Checks that a record with the same subject
#'   and the same value in the variable named by IDVAR exists in the target
#'   domain. The driver (fct_validate_sdtm_multi) resolves the "@RDOMAIN"
#'   marker at dispatch time and passes all candidate target domains via
#'   ref_dfs; this function addresses them row by row.
#'   Applicable to: SD0077, SD1482.
#'
#' @param main_df The current domain dataset (CO, SUPPQUAL, SQAP, APCO...).
#' @param ref_dfs A named list of candidate target domain datasets, resolved
#'   by the driver from the RDOMAIN column (keys are uppercase domain names).
#' @param domain_name The name of the current domain.
#' @param ref_domain Resolved target domain name(s) from the driver
#'   (informational only; the actual target is determined row-wise by
#'   rdomain_var). Kept for signature consistency.
#' @param target_vars The variables describing the reference, e.g.
#'   "RDOMAIN,USUBJID,IDVAR,IDVARVAL" (used for reporting only).
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param rdomain_var Variable holding the target domain name (default "RDOMAIN").
#' @param subj_var Variable holding the subject/person identifier
#'   ("USUBJID" for SD0077, "APID" for SD1482).
#' @param idvar_var Variable holding the referenced variable name (default "IDVAR").
#' @param idvarval_var Variable holding the referenced value (default "IDVARVAL").
#' @param cond_vars Variables for inclusion conditions.
#' @param cond_ops Operators for inclusion conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains".
#' @param cond_vals Values for inclusion conditions.
#' @param logic_op Logical operator to combine conditions ("AND" or "OR").
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_lookup_match_sup <- function(main_df, ref_dfs, domain_name, ref_domain = NULL,
                                    target_vars, ref_vars = NULL, rule_id,
                                    rdomain_var = "RDOMAIN", subj_var = "USUBJID",
                                    idvar_var = "IDVAR", idvarval_var = "IDVARVAL",
                                    cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                    logic_op = "AND", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Helpers ----
  get_val <- function(data, var) {
    x <- trimws(as.character(data[[var]]))
    x[is.na(x)] <- ""
    x
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

  # 3. Values & rows to check ----
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)

  rdom     <- toupper(get_val(main_df, rdomain_var))
  subj     <- get_val(main_df, subj_var)
  idvarv   <- toupper(get_val(main_df, idvar_var))
  idvarval <- get_val(main_df, idvarval_var)

  # rows with incomplete reference info are exempt (no record is referenced)
  rows_to_check <- rows_to_check & nzchar(rdom) & nzchar(idvarv) & nzchar(idvarval)
  if (!any(rows_to_check)) return(NULL)

  # 4. Core check: loop over target domains and referenced variables,
  #    vectorized within each (domain, variable) pair ----
  err_idx <- integer(0)
  err_msg <- character(0)

  for (r in unique(rdom[rows_to_check])) {
    idx_r <- which(rows_to_check & rdom == r)
    ref <- if (!is.null(ref_dfs)) ref_dfs[[r]] else NULL

    if (is.null(ref) || nrow(ref) == 0) {
      # defensive fallback: normally ensured by the driver's requiredDomains check
      err_idx <- c(err_idx, idx_r)
      err_msg <- c(err_msg, sprintf("Referenced record not found: target domain %s is missing or empty.", r))
      next
    }

    ref_subj <- get_val(ref, subj_var)

    for (v in unique(idvarv[idx_r])) {
      idx_v <- idx_r[idvarv[idx_r] == v]
      vals  <- idvarval[idx_v]

      if (v %in% names(ref)) {
        ref_keys <- paste(ref_subj, get_val(ref, v))
        bad <- !paste(subj[idx_v], vals) %in% ref_keys
      } else {
        # referenced variable does not exist in the target domain
        bad <- rep(TRUE, length(idx_v))
      }

      if (any(bad)) {
        err_idx <- c(err_idx, idx_v[bad])
        err_msg <- c(err_msg, sprintf("Referenced record (%s=%s) not found in domain %s for %s.",
                                      v, vals[bad], r, subj_var))
      }
    }
  }

  if (length(err_idx) == 0) return(NULL)

  # keep original row order
  ord     <- order(err_idx)
  err_idx <- err_idx[ord]
  err_msg <- err_msg[ord]

  # 5. Report (vectorized, one row per error) ----
  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = paste(rdomain_var, subj_var, idvar_var, idvarval_var, collapse = ", "),
    original_value = paste(rdom[err_idx], subj[err_idx], idvarv[err_idx], idvarval[err_idx], sep = " | "),
    rule_id        = rule_id,
    error_message  = err_msg,
    domain_name    = domain_name
  )
}
