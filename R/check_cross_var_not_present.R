#' @name check_cross_var_not_present
#' @title Check that a variable is not populated when a reference domain exists
#' @description For rows of the current domain, checks whether the target variable
#'   is populated when the reference domain exists (i.e. is present and non-empty).
#'   Applicable to: SD1468, SD1469.
#' @param main_df The current domain dataset.
#' @param ref_dfs A named list of reference domain datasets.
#' @param domain_name The name of the current domain.
#' @param target_vars The variable in the current domain to check (length 1).
#' @param rule_id The ID of the rule being checked.
#' @param ref_domains A character vector of reference domain names (e.g. "EC").
#' @param ... Additional arguments.
#' @return A data frame with columns Row, Variable, Original_Value, Rule_ID,
#'   Message, Severity, Domain; or NULL if no errors.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
check_cross_var_not_present <- function(main_df, ref_dfs, domain_name,
                                        target_vars, rule_id,
                                        ref_domains, ...) {
  # 0. Empty check ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 1. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  ref_domains <- as.character(ref_domains)

  if (length(target_vars) != 1) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = "target_vars must contain exactly one variable for check_cross_var_not_present.",
      domain_name = domain_name
    ))
  }

  # 2. Column existence ----
  miss_t <- setdiff(target_vars, names(main_df))
  if (length(miss_t) > 0) {
    return(report_error(
      row_number = "-", variable_name = "-", original_value = "-",
      rule_id = rule_id,
      error_message = sprintf("Required variable(s) not found in current domain: %s.", paste(miss_t, collapse = ", ")),
      domain_name = domain_name
    ))
  }

  # 3. Check reference domain existence ----
  ref_exists <- FALSE
  for (rd in ref_domains) {
    ref_df <- ref_dfs[[rd]]
    if (!is.null(ref_df) && nrow(ref_df) > 0) {
      ref_exists <- TRUE
      break
    }
  }
  if (!ref_exists) return(NULL)

  # 4. Identify populated values in target variable ----
  x <- trimws(as.character(main_df[[target_vars]]))
  x[is.na(x)] <- ""
  err_idx <- which(x != "")
  if (length(err_idx) == 0) return(NULL)

  # 5. Report ----
  ref_desc <- paste(ref_domains, collapse = ", ")
  err_msgs <- rep(
    sprintf("%s should not be populated when %s domain exists.", target_vars, ref_desc),
    length(err_idx)
  )

  report_df <- report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = target_vars,
    original_value = as.character(main_df[[target_vars]][err_idx]),
    rule_id        = rule_id,
    error_message  = err_msgs,
    domain_name    = domain_name
  )

  return(report_df)
}