#' @name check_cross_epoch_populated
#' @title Check that EPOCH is populated when the record date is a complete
#'   date not before the informed consent date
#' @description For rows of the current domain, checks that the EPOCH
#'   variable is populated when the date variable (--STDTC or --DTC) is
#'   collected as a complete date and this date is not before the Subject
#'   Informed Consent Date (RFICDTC in DM, aligned by USUBJID).
#'   Applicable to: SD1339, SD1477.
#'
#' @param main_df The current domain dataset.
#' @param ref_dfs The reference domain datasets (a named list, DM expected).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain (e.g. "DM").
#' @param target_vars The variable that should be populated
#'   (e.g. "EPOCH"; supports "--" wildcard).
#' @param ref_vars The informed consent date variable in the reference
#'   domain (e.g. "RFICDTC").
#' @param rule_id The ID of the rule being checked.
#' @param dtc_var The record date variable in the current domain
#'   (supports "--" wildcard, e.g. "--STDTC" or "--DTC").
#' @param complete_len Minimum character length defining a complete date
#'   part (default 10, i.e. YYYY-MM-DD).
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

check_cross_epoch_populated <- function(main_df, ref_dfs, domain_name, ref_domain,
                                  target_vars, ref_vars = "RFICDTC", rule_id,
                                  dtc_var, complete_len = 10,
                                  cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                  logic_op = "AND", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  dtc_var     <- gsub("--", domain_name, dtc_var)
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
  is_complete_date <- function(x) {
    x <- trimws(as.character(x)); x[is.na(x)] <- ""
    !is.na(as.Date(substr(x, 1, 10), format = "%Y-%m-%d")) & nchar(x) >= complete_len
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

  # 5. Align reference date by subject (vectorized) ----
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
    filter(is_complete_date(ref_date)) %>%
    select(USUBJID, ref_date) %>%
    distinct(USUBJID, .keep_all = TRUE)
  ref_date <- ref_map$ref_date[match(main_df$USUBJID, ref_map$USUBJID)]

  # 6. Core check (vectorized) ----
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)

  epoch_val <- trimws(as.character(main_df[[target_vars[1]]]))
  epoch_val[is.na(epoch_val)] <- ""
  dtc_complete <- is_complete_date(main_df[[dtc_var[1]]])

  dtc_date   <- as.Date(substr(trimws(as.character(main_df[[dtc_var[1]]])), 1, 10),
                        format = "%Y-%m-%d")
  ref_date_d <- as.Date(substr(ref_date, 1, 10), format = "%Y-%m-%d")
  not_before_ic <- dtc_complete & !is.na(ref_date_d) & dtc_date >= ref_date_d

  err_idx <- which(rows_to_check & not_before_ic & epoch_val == "")
  if (length(err_idx) == 0) return(NULL)

  # 7. Report (vectorized, one row per error) ----
  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = target_vars[1],
    original_value = epoch_val[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("%s should be populated when %s is collected as a complete date and is not before %s(%s).",
                             target_vars[1], dtc_var[1], ref_domain, ref_vars[1]),
    domain_name    = domain_name
  )
}
