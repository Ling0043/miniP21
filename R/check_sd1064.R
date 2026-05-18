#' @name check_sd1064
#'
#' @title check duplicate ETCD value
#'
#' @description SD1064: The value of Element Code (ETCD) variable
#'     must be unique within Trial Elements (TE) domain.
#'
#' @param df domain dataset to check
#' @param domain_name this is an empty parameter
#'
#' @return rule code, check status, error detail, row number of the error
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @examples check_sd1064(ta)
#' # minimal reproducible example
#' \dontrun{
#'   result <- check_sd1064(ta)
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

check_sd1064 <- function(df, domain_name) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0055] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  # rule SD0002 should report the error
  if (!"ETCD" %in% names(df)) {
    return(NULL)
  }

  # 3. Pre-processing ----
  etcd_values <- as.character(df$ETCD)
  is_blank <- is.na(etcd_values) | trimws(as.character(etcd_values)) == ""
  non_blank_etcds <- etcd_values[!is_blank]

  # 4. Core logic ----
  dup_values <- unique(non_blank_etcds[duplicated(non_blank_etcds)])

  all_errors <- list()
  for (dup_val in dup_values) {
    err_idx <- which(etcd_values == dup_val & !is_blank)
    for (idx in err_idx) {
      all_errors[[length(all_errors) + 1]] <- report_error(
        row_number = as.character(idx),
        variable_name = "ETCD",
        rule_id = "SD1064",
        error_message = sprintf("Duplicate ETCD value: '%s'.", dup_val)
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
