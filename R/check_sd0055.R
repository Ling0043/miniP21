#' @name check_sd0055
#'
#' @title match variable types with SDTM
#'
#' @description SD0055: Variable Data Types in the dataset should
#'     match the variable data types described in SDTM.
#'
#' @param df domain dataset to check
#' @param domain_name the domain of the dataset
#'
#' @return rule code, check status, error detail, row number of the error
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @examples check_sd0055(ta)
#' # minimal reproducible example
#' \dontrun{
#'   result <- check_sd0055(ta)
#' }

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-05-10  Author Name             Initial version
#
# =============================================================================

check_sd0055 <- function(df, domain_name) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0055] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  # 3. Pre-processing ----
  meta <- sdtm_v34 %>% filter(dataset_name == domain_name)

  # if SDTMIG don't have this domain description then skip checking
  if (nrow(meta) == 0) return(NULL)

  vars_to_check <- intersect(names(df), meta$variable_name)
  all_errors <- list()

  # 4. Core logic ----
  for (var in vars_to_check) {
    expected_type <- trimws(meta$type[meta$variable_name == var][1])
    actual_class <- class(df[[var]])
    is_mismatch <- FALSE

    if (expected_type == "numeric") {
      if (!actual_class %in% c("numeric", "integer")) {
        is_mismatch <- TRUE
      }
    } else if (expected_type == "character") {
      if (!actual_class %in% c("character")) {
        is_mismatch <- TRUE
      }
    }

    if (is_mismatch) {
      all_errors[[length(all_errors) + 1]] <- report_error(
        row_number = "-",
        variable_name = var,
        rule_id = "SD0055",
        error_message = sprintf("Type mismatch: Expected '%s', but '%s' in dataframe.",
                                expected_type, actual_class)
      )
    }
  }

  # 5.Logging end ----
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}