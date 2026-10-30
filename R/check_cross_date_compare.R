#' @name check_cross_date_compare
#' @title Check date comparison between current domain and reference domain
#' @description For rows of the current domain (optionally filtered), checks
#'   whether the target date variable satisfies the comparison operator
#'   against the reference date variable in the reference domain. Supports
#'   ISO8601 partial dates by comparing at the common precision. When
#'   multiple reference records exist per subject, ref_agg selects the
#'   aggregate reference date (e.g. "max" for the latest date).
#'   Applicable to: SD1144, SD1202, SD1203, SD1204, SD1205, SD1206, SD1207,
#'   SD1261, SD1317, SD1319, SD1347, SD0080, SD0082, SD1446.
#'
#' @param main_df The current domain dataset.
#' @param ref_dfs The reference domain datasets (a named list).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain.
#' @param target_vars The date variable in the current domain to check
#'   (supports "--" wildcard, e.g. "--STDTC").
#' @param ref_vars The corresponding date variable in the reference domain.
#' @param rule_id The ID of the rule being checked.
#' @param compare_op Allowed relation between target and reference date.
#'   Supported: "less", "less_equal", "greater", "greater_equal", "equal".
#'   Rows violating the relation are reported.
#' @param ref_agg Optional aggregate applied to the reference date when a
#'   subject has multiple reference records. Supported: "max", "min".
#'   NULL (default) takes the first record per subject.
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
#' @details
#' Modification History:
#' \itemize{
#'   \item v1.0 2026-10-08 Zhu Xiuling Initial version.
#'   \item v1.1 2026-10-08 Zhu Xiuling Removed logger calls and column
#'     existence check; replaced pmap_dfr with direct vectorized report_error.
#'   \item v1.2 2026-10-08 Zhu Xiuling Added ref_agg parameter to support
#'     aggregating multiple reference dates per subject (max/min).
#' }

check_cross_date_compare <- function(main_df, ref_dfs, domain_name, ref_domain,
                                     target_vars, ref_vars = NULL, rule_id,
                                     compare_op = "less_equal", ref_agg = NULL,
                                     cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                     logic_op = "AND", ...) {

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
  # Compare ISO8601 (possibly partial) dates at their common precision:
  # truncate both to the shorter character length, extract digits, compare numerically.
  compare_dtc <- function(a, b, op) {
    a <- trimws(as.character(a))
    b <- trimws(as.character(b))
    a[is.na(a)] <- ""
    b[is.na(b)] <- ""
    valid <- nzchar(a) & nzchar(b)

    p  <- pmin(nchar(a), nchar(b))
    an <- suppressWarnings(as.numeric(gsub("\\D", "", substr(a, 1, p))))
    bn <- suppressWarnings(as.numeric(gsub("\\D", "", substr(b, 1, p))))

    res <- switch(op,
                  less           = an <  bn,
                  less_equal     = an <= bn,
                  greater        = an >  bn,
                  greater_equal  = an >= bn,
                  equal          = an == bn,
                  stop("Unknown compare operator: ", op))
    valid & !res & !is.na(res)
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

  # 6. Align reference date by subject (vectorized) ----
  if (!"USUBJID" %in% names(main_df) || !"USUBJID" %in% names(ref_df)) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("USUBJID not found in %s or %s.", domain_name, ref_domain),
      domain_name    = domain_name
    ))
  }

  ref_map <- ref_df %>%
    mutate(ref_date = trimws(as.character(.data[[ref_vars[1]]]))) %>%
    filter(ref_date != "")

  # aggregate multiple reference dates per subject when ref_agg is specified
  if (!is.null(ref_agg)) {
    ref_map <- ref_map %>%
      group_by(USUBJID) %>%
      summarise(ref_date = if (ref_agg == "max") max(ref_date) else min(ref_date),
                .groups = "drop")
  } else {
    ref_map <- ref_map %>%
      select(USUBJID, ref_date) %>%
      distinct(USUBJID, .keep_all = TRUE)
  }

  ref_date_aligned <- ref_map$ref_date[match(main_df$USUBJID, ref_map$USUBJID)]

  # 7. Core comparison (vectorized) ----
  err_idx <- which(rows_to_check &
                     compare_dtc(main_df[[target_vars[1]]], ref_date_aligned, compare_op))
  if (length(err_idx) == 0) return(NULL)

  # 8. Report (vectorized, one row per error) ----
  tvar_str <- paste(target_vars, collapse = ", ")
  cmp_desc <- switch(compare_op,
                     less = "before", less_equal = "on or before",
                     greater = "after", greater_equal = "on or after",
                     equal = "the same as")
  agg_desc <- switch(ref_agg, max = "latest ", min = "earliest ", "")
  filter_desc <- NULL
  if (!is.null(cond_vars)) filter_desc <- paste0("only checked when ",
    paste(sprintf("%s is %s", cond_vars, cond_ops), collapse = " and "))
  filter_str <- if (is.null(filter_desc)) "." else paste0(" (", paste(filter_desc, collapse = "; "), ").")

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = tvar_str,
    original_value = as.character(main_df[[target_vars[1]]][err_idx]),
    rule_id        = rule_id,
    error_message  = sprintf("%s (%s) must be %s %s%s(%s)%s",
                             tvar_str, main_df[[target_vars[1]]][err_idx],
                             cmp_desc, agg_desc, ref_domain, ref_vars[1], filter_str),
    domain_name    = domain_name
  )
}