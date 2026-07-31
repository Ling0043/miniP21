#' @name check_var_presence_cond
#'
#' @title Check Variable Presence with Condition
#'
#' @description Checks whether the presence or absence of target variables
#'   meets the expected logic based on the presence or absence of condition
#'   variables. Used for rules SD1083, SD1087, SD1091, SD1099, SD1101, SD1102,
#'   SD1103, SD1104, SD1129, SD1147, SD1244, SD1245, SD1246, SD1250, SD1280,
#'   SD1282, SD1283, SD1284, SD1285, SD1293, SD1294, SD1299, SD1357, SD1450,
#'   SD1451, SD2270, SD2271, SD2272.
#'
#' @param df A data frame containing the domain data to check.
#' @param domain_name Character string. Name of the domain (e.g., `"DM"`).
#' @param target_vars Character vector (or JSON string array). List of variables
#'   to check for presence/absence. Parsed from config 'targetVariable'.
#' @param cond_vars Character vector. Variables that trigger the condition.
#' @param cond_presence Boolean. TRUE if cond_vars should be present, FALSE if absent.
#' @param cond_type Character string. "all" or "any". Logic for cond_vars.
#' @param target_presence Boolean. TRUE if target variables should be present, FALSE if absent.
#' @param target_type Character string. "all" or "any". Logic for target_vars.
#' @param rule_id Character string. Identifier of the validation rule (e.g.
#'   "SD1083").
#' @param ... Additional arguments (currently unused).
#'
#' @return A data frame with columns `row_number`, `variable_name`,
#'   `original_value`, `rule_id`, and `error_message` if the rule is violated,
#'   or `NULL` if no errors.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#' @importFrom jsonlite fromJSON

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-06-09  Zhu Xiuling             Initial version
#
# =============================================================================

check_var_presence_cond <- function(df, domain_name, target_vars, cond_vars, cond_presence, cond_type, target_presence, target_type, rule_id, ...) {
  # Handle potential JSON string input for target_vars from config table
  # ---- Existence checks ----
  if (any(grepl("^--", target_vars))) {
    target_vars <- gsub("^--", domain_name, target_vars)
    cond_vars <- gsub("^--", domain_name, cond_vars)
  }

  # Get present columns
  present_cols <- names(df)

  # Evaluate condition
  if (length(cond_vars) == 0) {
    cond_met <- TRUE
  } else {
    cond_present_flags <- cond_vars %in% present_cols
    if (cond_presence) {
      cond_met <- if (cond_type == "all") all(cond_present_flags) else any(cond_present_flags)
    } else {
      cond_met <- if (cond_type == "all") all(!cond_present_flags) else any(!cond_present_flags)
    }
  }

  if (!cond_met) {
    return(NULL)
  }

  # Evaluate target
  target_present_flags <- target_vars %in% present_cols
  if (target_presence) {
    target_met <- if (target_type == "all") all(target_present_flags) else any(target_present_flags)
  } else {
    target_met <- if (target_type == "all") all(!target_present_flags) else any(!target_present_flags)
  }
  
  if (!target_met) {
    missing_vars <- target_vars[if (target_presence) !target_present_flags else target_present_flags]
    error_msg <- sprintf("Variable presence logic violation for: %s", paste(missing_vars, collapse=", "))
    
    report_df <- report_error(
      row_number = NA_character_,
      variable_name = paste(target_vars, collapse = ", "),
      original_value = NA_character_,
      rule_id = rule_id,
      error_message = error_msg
    )
    return(report_df)
  }

  return(NULL)
}
