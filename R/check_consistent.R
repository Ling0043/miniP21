#' @name check_consistent
#' @title Check for consistency of composite keys (handles '--' wildcards & aggregates output)
#' @param df Data frame to check
#' @param domain_name The domain of the dataset
#' @param target_vars Character vector of variables that should be unique
#' @param rule_id The rule ID being checked
#' @param group_vars Character vector of grouping variables. Default is NULL.
#' @param ... Absorb extra parameters
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-07-08  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_consistent <- function(df, domain_name, target_vars, rule_id, group_vars, ...) {

  # 1. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_group_vars  <- if (!is.null(group_vars)) gsub("--", domain_name, group_vars) else NULL

  check_cols <- c(actual_group_vars, actual_target_vars)

  # if any of the check_cols are missing in df, return NULL (no error report)
  if (!all(check_cols %in% names(df))) {
    return(NULL)
  }

  # drop rows with missing/blank values in any of the target_vars (only for the purpose of duplication check, not considered as errors here)
  is_valid <- Reduce(`&`, lapply(actual_target_vars, function(v) {
    !is.na(df[[v]]) & trimws(as.character(df[[v]])) != ""
  }))
  
  valid_idx <- which(is_valid)
  if (length(valid_idx) == 0) return(NULL)
  df_check <- df[valid_idx, check_cols, drop = FALSE]
  
  # 2. Core logic ----
  ## check the consistenty of the composite key values
  cont_non_consistent <- df_check %>%
  group_by(across(all_of(actual_group_vars))) %>%
  summarise(distinct_b_count = n_distinct(.data[[actual_target_vars]]), .groups = "drop") %>%
  filter(distinct_b_count > 1)

  if (nrow(cont_non_consistent) > 0) {
    result_consistent <- df %>% semi_join(cont_non_consistent %>% select(all_of(actual_group_vars)), by = actual_group_vars)
    err_idx2 <- result_consistent$rownumber_new
    row_number2 <- paste0(err_idx2, collapse = ", ")
    variable_name2 <- paste(actual_target_vars, collapse = ", ")
    original_value2 <- unique(result_consistent[, actual_target_vars], drop = FALSE) %>% paste0(collapse = " | ")
    error_message2 <- sprintf("Value is not consistent %s.",
                             ifelse(!is.null(actual_group_vars), 
                                    sprintf("within %s", paste(actual_group_vars, collapse = ", ")), 
                                    " in dataset"))
    report_df2 <- report_error(
      row_number    = row_number2,
      variable_name = variable_name2,
      original_value = original_value2,
      rule_id       = rule_id,
      error_message = error_message2
    )
  } else {
    report_df2 <- NULL
  }

  return(report_df2)
}
