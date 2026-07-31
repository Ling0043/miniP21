#' @name check_ts_param_required
#'
#' @title Check Required Trial Summary Parameter
#'
#' @description Checks whether a specific parameter code exists and is populated 
#'   in a vertical data structure (e.g., Trial Summary domain). Supports 
#'   conditional checks where the parameter is only required if another 
#'   parameter equals a specific value. Used for rules SD2223 to SD2287.
#'
#' @param df A data frame containing the domain data to check.
#' @param target_vars Character string. Name of the column containing parameter 
#'   codes (e.g., "TSPARMCD").
#' @param param_code Character string. The specific parameter code that must 
#'   be populated (e.g., "FCNTRY").
#' @param cond_param Character string. Optional. Name of the condition parameter 
#'   code (e.g., "STYPE") that triggers the check. Default is NULL.
#' @param cond_val Vector of values. Optional. Values in `cond_param` that 
#'   trigger the check (e.g., "INTERVENTIONAL"). Default is NULL.
#' @param val_var Character string. Name of the column containing parameter 
#'   values (e.g., "TSVAL"). Default is "TSVAL".
#' @param rule_id Character string. Identifier of the validation rule (e.g., 
#'   "SD2224").
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, and `error_message` for each missing 
#'   parameter, or `NULL` if no errors or required columns are missing.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by            Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-06-09  Zhu Xiuling             Initial version
#
# =============================================================================

check_ts_param_required <- function(df, target_vars, param_code, cond_param = NULL, cond_val = NULL, val_var = "TSVAL", rule_id, ...) {
  # 1. Pre-processing ----
  if (!target_vars %in% names(df) || !val_var %in% names(df)) {
    return(NULL)
  }
  
  if (!is.null(cond_param) && !is.null(cond_val)) {
    if (!cond_param %in% df[[target_vars]]) {
      return(NULL)
    }
    actual_cond_vals <- df[[val_var]][df[[target_vars]] == cond_param]
    if (!any(actual_cond_vals %in% cond_val)) {
      return(NULL)
    }
  }
  
  # 2. Core logic ----
  has_param <- param_code %in% df[[target_vars]]
  param_vals <- if (has_param) df[[val_var]][df[[target_vars]] == param_code] else NULL
  
  is_missing_or_empty <- !has_param || all(is.na(param_vals) | trimws(param_vals) == "")
  
  if (is_missing_or_empty) {
    report_df <- report_error(
      row_number = NA_character_,
      variable_name = target_vars,
      original_value = NA_character_,
      rule_id = rule_id,
      error_message = sprintf("Missing required %s parameter '%s' in TS domain", target_vars, param_code)
    )
    return(report_df)
  }
  
  return(NULL)
}

