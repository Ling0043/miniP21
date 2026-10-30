#' @name check_corss_rdomain_valid
#' @title Check that RDOMAIN values are valid domains included in the study data
#' @description For rows of the current domain (optionally filtered), checks
#'   that each non-missing RDOMAIN value is the name of a domain present in
#'   the input data (matched against the names of the datasets provided to
#'   the validation run, resolved by the driver via the "@ALL_DOMAINS"
#'   marker). Values longer than max_chars are also reported as invalid
#'   (SDTMIG: RDOMAIN is 2 characters; RELREC exception allows up to 4).
#'   Applicable to: SD0072, SD0072A.
#'
#' @param main_df The current domain dataset (CO, SUPPQUAL, SQAP, APCO,
#'   RELREC...).
#' @param ref_dfs All candidate domain datasets (a named list), resolved by
#'   the driver from the "@ALL_DOMAINS" marker; names are used as the set of
#'   valid domain values.
#' @param domain_name The name of the current domain.
#' @param ref_domain Not used for value lookup (valid domains are taken from
#'   names(ref_dfs)); kept for signature consistency.
#' @param target_vars The variable holding the domain reference, e.g. "RDOMAIN".
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param rdomain_var Variable holding the target domain name (default "RDOMAIN").
#' @param max_chars Maximum allowed character length for RDOMAIN values
#'   (2 for SD0072; 4 for SD0072A per the SDTMIG RELREC exception).
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
#'
# ==============================================================================
# Modification History
# ==============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-10-27  Zhu Xiuling             Initial version
#
# ==============================================================================

check_corss_rdomain_valid <- function(main_df, ref_dfs, domain_name, ref_domain,
                                target_vars, ref_vars = NULL, rule_id,
                                rdomain_var = "RDOMAIN", max_chars = 2,
                                cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                logic_op = "AND", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Valid domain set (from provided dataset names) ----
  valid_domains <- if (!is.null(ref_dfs)) toupper(names(ref_dfs)) else character(0)

  # 3. Helpers ----
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

  # 4. Values & rows to check ----
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)

  rdom <- toupper(trimws(as.character(main_df[[rdomain_var]])))
  rdom[is.na(rdom)] <- ""

  # rows with missing RDOMAIN are exempt (handled by required-variable rules)
  rows_to_check <- rows_to_check & nzchar(rdom)
  if (!any(rows_to_check) || length(valid_domains) == 0) return(NULL)

  # 5. Core check (vectorized): invalid when value is too long or unknown ----
  err_idx <- which(rows_to_check &
                     (nchar(rdom) > max_chars | !rdom %in% valid_domains))
  if (length(err_idx) == 0) return(NULL)

  # 6. Report (vectorized, one row per error) ----
  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = rdomain_var,
    original_value = rdom[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s value '%s' is not a valid domain included in the study data (max %d characters).",
                             rdomain_var, rdom[err_idx], max_chars),
    domain_name    = domain_name
  )
}
