#' @name check_cross_key_combo_in_ref
#' @title Check that key combinations in the current domain exist in the
#'   reference domain
#' @description For rows of the current domain (excluding rows meeting the
#'   exemption conditions), checks that the combination of key variables
#'   (e.g. USUBJID + VISIT + VISITNUM) is present in the reference domain
#'   (e.g. SV), where the reference keys are built from the same
#'   combination. Key comparison is case-insensitive and whitespace-trimmed.
#'   Applicable to: SD0065 (and reusable for other record-level key
#'   existence rules by changing key_vars / ref_domain).
#'
#' @param main_df The current domain dataset.
#' @param ref_dfs The reference domain datasets (a named list, SV expected).
#' @param domain_name The name of the current domain.
#' @param ref_domain The reference domain (e.g. "SV").
#' @param target_vars The key variables in main_df
#'   (e.g. c("USUBJID", "VISIT", "VISITNUM")).
#' @param ref_vars The corresponding key variables in the reference domain
#'   (NULL = same as target_vars).
#' @param rule_id The ID of the rule being checked.
#' @param exc_cond_vars Variables for exemption conditions (evaluated on
#'   main_df; records meeting ANY exemption condition are NOT reported).
#'   Supports "--" wildcard (e.g. "--STAT"). Variables not present in the
#'   current domain make that condition FALSE (silently skipped), so a
#'   shared config can cover domains with and without e.g. ECOCCUR.
#' @param exc_cond_ops Operators for exemption conditions. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains". Comparison is case-insensitive.
#' @param exc_cond_vals Values for exemption conditions.
#' @param exc_logic_op Logical operator to combine exemption conditions
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

check_cross_key_combo_in_ref <- function(main_df, ref_dfs, domain_name, ref_domain,
                                   target_vars, ref_vars = NULL, rule_id,
                                   exc_cond_vars = NULL, exc_cond_ops = NULL,
                                   exc_cond_vals = NULL, exc_logic_op = "OR", ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  exc_cond_vars <- gsub("--", domain_name, exc_cond_vars)
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
  get_val <- function(data, var) {
    x <- trimws(as.character(data[[var]]))
    x[is.na(x)] <- ""
    toupper(x)
  }

  eval_conds <- function(data, vars, ops, vals) {
    if (is.null(vars) || length(vars) == 0) return(rep(FALSE, nrow(data)))
    idx_list <- lapply(seq_along(vars), function(i) {
      var <- vars[i]; op <- ops[i]; val <- vals[i]
      if (!var %in% names(data)) return(rep(FALSE, nrow(data)))  # var absent -> cond FALSE
      x <- get_val(data, var)
      switch(op,
             missing      = x == "",
             non_missing  = x != "",
             equal        = x == toupper(val),
             not_equal    = x != toupper(val),
             contains     = grepl(toupper(val), x, fixed = TRUE),
             not_contains = !grepl(toupper(val), x, fixed = TRUE),
             stop("Unknown operator: ", op))
    })
    if (exc_logic_op == "AND") Reduce(`&`, idx_list) else Reduce(`|`, idx_list)
  }

  # 5. Key variables existence ----
  if (!all(target_vars %in% names(main_df))) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Key variable(s) %s not found in %s.",
                               paste(setdiff(target_vars, names(main_df)),
                                     collapse = ", "), domain_name),
      domain_name    = domain_name
    ))
  }
  if (!all(ref_vars %in% names(ref_df))) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Reference key variable(s) %s not found in %s.",
                               paste(setdiff(ref_vars, names(ref_df)),
                                     collapse = ", "), ref_domain),
      domain_name    = domain_name
    ))
  }

  # 6. Exemptions ----
  exempt <- eval_conds(main_df, exc_cond_vars, exc_cond_ops, exc_cond_vals)

  # 7. Reference key set (vectorized) ----
  ref_keys <- do.call(paste,
                      c(lapply(ref_vars, function(v) get_val(ref_df, v)), sep = " | "))
  # rows with any missing key component in SV are not reliable references
  ref_complete <- Reduce(`&`, lapply(ref_vars, function(v) get_val(ref_df, v) != ""))
  ref_keys <- unique(ref_keys[ref_complete])

  # 8. Core check (vectorized) ----
  main_keys <- do.call(paste,
                       c(lapply(target_vars, function(v) get_val(main_df, v)), sep = " | "))
  main_complete <- Reduce(`&`, lapply(target_vars, function(v) get_val(main_df, v) != ""))

  err_idx <- which(!exempt & main_complete & !main_keys %in% ref_keys)
  if (length(err_idx) == 0) return(NULL)

  # 9. Report (vectorized, one row per error) ----
  key_desc <- paste(target_vars, collapse = " + ")
  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = paste(target_vars, collapse = ", "),
    original_value = main_keys[err_idx],
    rule_id        = rule_id,
    error_message  = sprintf("Combination of %s not found in %s.",
                             key_desc, ref_domain),
    domain_name    = domain_name
  )
}
