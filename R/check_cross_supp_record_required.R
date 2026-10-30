#' @name check_cross_supp_record_required
#' @title Check that records matching a condition have a supplemental
#'   qualifier record with a given QNAM (and populated QVAL) in the SUPP-- domain
#' @description For rows of the current domain matching the source
#'   condition(s) (all rows if no condition is given), checks that a
#'   corresponding record exists in the supplemental domain (e.g. SUPPAE),
#'   linked by USUBJID + IDVARVAL (IDVARVAL holds the value of link_var,
#'   e.g. AESEQ), with QNAM equal to the required qualifier name and
#'   (optionally) a populated QVAL.
#'   Applicable to: SD1143, SD1321.
#'
#' @param main_df The source domain dataset (e.g. AE).
#' @param ref_dfs The reference domain datasets (a named list, must contain
#'   the supplemental domain, e.g. SUPPAE).
#' @param domain_name The name of the current domain.
#' @param ref_domain The supplemental domain (e.g. "SUPPAE").
#' @param target_vars Not used for value lookup (reporting uses link_var);
#'   kept for signature consistency.
#' @param ref_vars Not used, kept for signature consistency.
#' @param rule_id The ID of the rule being checked.
#' @param supp_qnam The required QNAM value in the supplemental domain
#'   (e.g. "AESOSP", "TRTEMFL"; case-insensitive).
#' @param link_var The variable in main_df whose value is stored in
#'   IDVARVAL of the supplemental domain (default "AESEQ").
#' @param check_qval_nonmissing If TRUE (default), the matching supplemental
#'   record must also have a non-missing QVAL; if FALSE, presence of the
#'   QNAM record is sufficient.
#' @param cond_vars Variables for the source condition (evaluated on
#'   main_df, e.g. AESMIE). NULL means all records are checked.
#' @param cond_ops Operators for the source condition. Supported:
#'   "missing", "non_missing", "equal", "not_equal",
#'   "contains", "not_contains". Comparison is case-insensitive.
#' @param cond_vals Values for the source condition.
#' @param logic_op Logical operator to combine source conditions
#'   ("AND" or "OR").
#' @param check_idvar If TRUE, additionally requires SUPP records whose
#'   IDVAR equals paste0(toupper(substr(link_var,1,2)), substr(link_var,3)),
#'   i.e. the standard IDVAR name (e.g. "AESEQ"). Default FALSE (IDVAR not
#'   checked; linkage by IDVARVAL only).
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_supp_record_required <- function(main_df, ref_dfs, domain_name, ref_domain,
                                       target_vars = NULL, ref_vars = NULL, rule_id,
                                       supp_qnam, link_var = "AESEQ",
                                       check_qval_nonmissing = TRUE,
                                       cond_vars = NULL, cond_ops = NULL, cond_vals = NULL,
                                       logic_op = "AND",
                                       check_idvar = FALSE, ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Supplemental domain existence ----
  supp_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL
  if (is.null(supp_df) || nrow(supp_df) == 0 ||
      !all(c("USUBJID", "QNAM", "IDVARVAL") %in% names(supp_df))) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Supplemental domain %s is missing or empty or lacks USUBJID/QNAM/IDVARVAL.", ref_domain),
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
    if (logic_op == "OR") Reduce(`|`, idx_list) else Reduce(`&`, idx_list)
  }

  # 4. Rows to check in main_df ----
  if (!all(c("USUBJID", link_var) %in% names(main_df))) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("USUBJID or %s not found in %s.", link_var, domain_name),
      domain_name    = domain_name
    ))
  }
  rows_to_check <- eval_conds(main_df, cond_vars, cond_ops, cond_vals)
  if (!any(rows_to_check)) return(NULL)

  # 5. Keys of qualifying supplemental records (vectorized) ----
  qnam_match <- get_val(supp_df, "QNAM") == toupper(supp_qnam)
  supp_ok <- qnam_match
  if (check_qval_nonmissing) {
    supp_ok <- supp_ok & (get_val(supp_df, "QVAL") != "")
  }
  if (check_idvar) {
    std_idvar <- paste0(toupper(substr(link_var, 1, 2)), substr(link_var, 3, nchar(link_var)))
    supp_ok <- supp_ok & (get_val(supp_df, "IDVAR") == std_idvar)
  }
  supp_keys <- paste(get_val(supp_df, "USUBJID"), get_val(supp_df, "IDVARVAL"))[supp_ok]

  # 6. Core check (vectorized): row key must exist among supp_keys ----
  idx_chk  <- which(rows_to_check)
  row_keys <- paste(get_val(main_df, "USUBJID")[idx_chk],
                    get_val(main_df, link_var)[idx_chk])
  bad <- !row_keys %in% supp_keys
  err_idx <- idx_chk[bad]
  if (length(err_idx) == 0) return(NULL)

  # 7. Report (vectorized, one row per error) ----
  cond_desc <- if (is.null(cond_vars)) "" else
    paste0(" with ", paste(sprintf("%s %s '%s'", cond_vars,
                                   ifelse(cond_ops == "missing", "is missing", cond_ops),
                                   ifelse(is.na(cond_vals), "", cond_vals)),
                            collapse = paste0(" ", logic_op, " ")))

  qval_desc <- if (check_qval_nonmissing) " and populated QVAL" else ""

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = link_var,
    original_value = paste(get_val(main_df, "USUBJID")[err_idx],
                           get_val(main_df, link_var)[err_idx], sep = " | "),
    rule_id        = rule_id,
    error_message  = sprintf("No %s record with QNAM='%s'%s found for this %s record%s (linked by USUBJID + %s).",
                             ref_domain, toupper(supp_qnam), qval_desc,
                             domain_name, cond_desc, link_var),
    domain_name    = domain_name
  )
}
