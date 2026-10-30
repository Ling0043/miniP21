#' @name check_cross_not_populated_when_ref_missing
#' @title Check that a variable is not populated when a reference date is missing
#' @description For rows of the current domain, checks whether the target variable
#'   is populated for subjects whose reference date variable in the reference
#'   domain is missing (null or empty). Applicable to: SD1030, SD1031.
#' @param main_df The current domain dataset.
#' @param ref_dfs A named list of reference domain datasets.
#' @param domain_name The name of the current domain.
#' @param target_vars The variable in the current domain to check (length 1).
#' @param rule_id The ID of the rule being checked.
#' @param ref_domains A character vector of reference domain names (e.g. "DM").
#' @param ref_vars The reference date variable in the reference domain (length 1).
#' @param ... Additional arguments.
#' @return A data frame with columns Row, Variable, Original_Value, Rule_ID,
#'   Message, Severity, Domain; or NULL if no errors.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_not_populated_when_ref_missing <- function(main_df, ref_dfs, domain_name,
                                                       target_vars, rule_id,
                                                        ref_domains, ref_vars, ...) {
  # 0. Empty check ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 1. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  ref_domains <- as.character(ref_domains)
  rd <- ref_domains[[1]]

  if (length(target_vars) != 1 || length(ref_vars) != 1) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = "target_vars and ref_vars must each contain exactly one variable.",
      domain_name = domain_name
    ))
  }

  # 2. Column existence ----
  miss_t <- setdiff(c("USUBJID", target_vars), names(main_df))
  if (length(miss_t) > 0) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = sprintf("Required variable(s) not found in current domain: %s.", paste(miss_t, collapse = ", ")),
      domain_name = domain_name
    ))
  }

  ref_df <- ref_dfs[[rd]]
  if (is.null(ref_df) || nrow(ref_df) == 0) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = sprintf("Reference domain %s not found or empty.", rd),
      domain_name = domain_name
    ))
  }



  miss_r <- setdiff(c("USUBJID", ref_vars), names(ref_df))
  if (length(miss_r) > 0) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = sprintf("Required variable(s) not found in %s: %s.", rd, paste(miss_r, collapse = ", ")),
      domain_name = domain_name
    ))
  }

  # 3. Match subjects to reference domain ----
  # keep first record per subject in reference domain
  ref_df <- ref_df[!duplicated(as.character(ref_df$USUBJID)), , drop = FALSE]
  ref_idx <- match(as.character(main_df$USUBJID), as.character(ref_df$USUBJID))

  ref_val <- ref_df[[ref_vars]][ref_idx]
  ref_missing <- is.na(ref_val) | trimws(as.character(ref_val)) == ""

  target_val <- trimws(as.character(main_df[[target_vars]]))
  target_populated <- !is.na(target_val) & target_val != ""

  # only check subjects that exist in reference domain and have missing reference date
  err_idx <- which(!is.na(ref_idx) & ref_missing & target_populated)
  if (length(err_idx) == 0) return(NULL)

  # 4. Report ----
  err_msgs <- rep(
    sprintf("%s should not be populated when %s is null in %s.", target_vars, ref_vars, rd),
    length(err_idx)
  )

  report_df <- report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = rep(target_vars, length(err_idx)),
    original_value = as.character(main_df[[target_vars]][err_idx]),
    rule_id        = rep(rule_id, length(err_idx)),
    error_message  = err_msgs,
    domain_name    = rep(domain_name, length(err_idx))
  )

  return(report_df)
}

20+40+30+50+85

225*31
