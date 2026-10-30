#' @name check_duplicate
#' @title Check for duplicate composite keys (handles '--' wildcards & aggregates output)
#' @param df Data frame to check
#' @param domain_name The domain of the dataset
#' @param target_vars Character vector of variables that should be unique
#' @param rule_id The rule ID being checked
#' @param group_vars Character vector of grouping variables. Default is NULL.
#' @param filter_values Optional value to filter the target variable before checking for duplicates. Default is NULL (no filtering).
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
# 1.0      2026-05-28  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

check_duplicate <- function(df, domain_name, target_vars, rule_id, group_vars = NULL, filter_values = NULL, ...) {
  # 1. Pre-processing ----
  actual_target_vars <- gsub("--", domain_name, target_vars)
  actual_group_vars  <- if (!is.null(group_vars)) gsub("--", domain_name, group_vars) else NULL

  check_cols <- c(actual_group_vars, actual_target_vars)

  # if any of the check_cols are missing in df, return NULL (no error report)
  if (!all(check_cols %in% names(df))) {
    return(NULL)
  }

   if (!is.null(filter_values)) {
    df <- df[df[[target_vars]] == filter_values, ]
  }

  # drop rows with missing/blank values in any of the target_vars (only for the purpose of duplication check, not considered as errors here)
  is_valid <- Reduce(`&`, lapply(actual_target_vars, function(v) {
    !is.na(df[[v]]) & trimws(as.character(df[[v]])) != ""
  }))

  valid_idx <- which(is_valid)
  if (length(valid_idx) == 0) return(NULL)

  df_rownumbered <- df[valid_idx, "rownumber_new", drop = FALSE]
  df_check <- df[valid_idx, check_cols, drop = FALSE]

  # 2. Core logic ----
  ## check duplicates in the composite key (check_cols)
  is_dup <- duplicated(df_check) | duplicated(df_check, fromLast = TRUE)
  if (any(is_dup)) {
    df_dup <- df_check[is_dup, , drop = FALSE]
    df_dup$rownumber_new <- as.character(df_rownumbered[is_dup, "rownumber_new"])

    dup_groups <- df_dup %>%
      group_by(across(all_of(check_cols))) %>%
      summarise(
        row_number = paste(rownumber_new, collapse = " | "),
        times      = n(),
        .groups    = "drop"
      )

    original_value_df <- dup_groups %>% select(all_of(actual_target_vars))
    if (length(actual_target_vars) == 1) {
      original_value <- original_value_df[[actual_target_vars]]
    } else {
      original_value <- do.call(paste, c(original_value_df, sep = ", "))
    }

    error_message <- sprintf(
      "Combination is duplicated %d times %s.",
      dup_groups$times,
      if (!is.null(actual_group_vars))
        sprintf("within %s", paste(actual_group_vars, collapse = ", "))
      else "in dataset"
    )


    report_df1 <- report_error(
      row_number     = dup_groups$row_number,
      variable_name  = actual_target_vars,
      original_value = original_value,
      rule_id        = rule_id,
      error_message  = error_message,
      domain_name    = domain_name
    )
  } else {
    report_df1 <- NULL
  }
  return(report_df1)
}