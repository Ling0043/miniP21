#' @name check_variable
#'
#' @title check required/Expected/Unknown variables
#'
#' @description Applicable to rules: SD0056, SD0057, SD0058
#'
#' @param df domain dataset to check
#' @param domain_name the domain of the dataset
#' @param rule_id the rule code to check (SD0056, SD0057, SD0058)
#' @return rule code, check status, error detail, row number of the error
#'
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
# 1.0      2026-05-10  Zhu Xiuling             Initial version
#
# =============================================================================

# 1. Library imports ----
library(dplyr)
library(logger)

# 2. Main function(s) ----
check_variable <- function(df, domain_name, rule_id, ...) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0002] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  df_name <- names(df) |> setdiff("rownumber_new")
  all_errors <- list()
  # 3. required variables ----
  if (rule_id == "SD0056") {
  req_meta <- sdtm_v34 %>% filter(dataset_name == domain_name &
                                     core == "Req")
  req_vars <- req_meta$variable_name
  vars_to_check <- setdiff(req_vars, df_name)
  # if this domain have no "Req" variable, then end function

  for (var in vars_to_check) {
    # a error per line
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = "-",
      variable_name = var,
      original_value = "-",
      rule_id = "SD0056",
      error_message = sprintf("Required variable '%s' is not found.", var),
      domain_name   = domain_name
    )
  }}

  # 4. expected variables ----
  if (rule_id == "SD0057") {
  exp_meta <- sdtm_v34 %>% filter(dataset_name == domain_name &
                                     core == "exp")
  exp_vars <- exp_meta$variable_name
  vars_to_check <- setdiff(exp_vars, df_name)

  for (var in vars_to_check) {
    # a error per line
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = "-",
      variable_name = var,
      original_value = "-",
      rule_id = "SD0057",
      error_message = sprintf("Expected variable '%s' is not found.", var),
      domain_name   = domain_name
    )
  }}

  # 5. unknown variables ----
  if (rule_id == "SD0058") { 
  allow_meta <- sdtm_v34 %>% filter(dataset_name == domain_name)
  allow_vars <- allow_meta$variable_name
  vars_to_check <- setdiff(df_name, allow_vars)

  for (var in vars_to_check) {
    # a error per line
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = "-",
      variable_name = var,
      original_value = "-",
      rule_id = "SD0058",
      error_message = "Only variables listed in SDTM model should appear in a dataset",
      domain_name   = domain_name
    )
  }}

  # 6.Logging end ----
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}