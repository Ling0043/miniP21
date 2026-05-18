#' @name check_sd0002
#'
#' @title check required variables
#'
#' @description SD0002: Required variables (where Core attribute is 'Req')
#'     cannot be null for any records.
#'
#' @param df domain dataset to check
#' @param domain_name the domain of the dataset
#' @return rule code, check status, error detail, row number of the error
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @examples check_sd0002(ae)
#' # minimal reproducible example
#' \dontrun{
#'   result <- check_sd0002(ae)
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

# 1. Library imports ----
library(dplyr)
library(logger)

# 2. Main function(s) ----
check_sd0002 <- function(df, domain_name) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0002] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  # 3. Pre-processing ----
  req_meta <- sdtm_v34 %>% filter(dataset_name == domain_name &
                                     core == "Req")
  req_vars <- req_meta$variable_name
  # if this domain have no "Req" variable, then end function
  if (length(req_vars) == 0) {
    return(report_error("-", "Pass", "SD0002", "No Required variables defined for this domain."))
  }
  vars_to_check <- intersect(req_vars, names(df))

  # 4. Core logic ----
  all_errors <- list()
  for (var in vars_to_check) {
    err_idx <- which(is.na(df[[var]]) |
                       trimws(as.character(df[[var]])) == "")

    if (length(err_idx) > 0) {
      for (idx in err_idx) {
        # a error per line
        all_errors[[length(all_errors) + 1]] <- report_error(
          row_number = as.character(idx),
          variable_name = var,
          rule_id = "SD0002",
          error_message = sprintf("Required variable '%s' is null or empty.", var)
        )
      }
    }
  }

  # 5.Logging end ----
  # logger::log_info("[check_sd0002] End.")

  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}