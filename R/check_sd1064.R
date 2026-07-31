#' @name check_sd1064
#'
#' @title check duplicate ETCD value
#'
#' @description SD1064: The value of Element Code (ETCD) variable
#'     must be unique within Trial Elements (TE) domain.
#'
#' @param df domain dataset to check
#' @param ... Additional arguments (currently unused).
#'
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
# 1.0      2026-05-10  Author Name             Initial version
#
# =============================================================================

check_sd1064 <- function(df, ...) {
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
  is_blank <- is.na(etcd_values) | trimws(etcd_values) == ""
  non_blank_etcds <- etcd_values[!is_blank]


  # 4. Core logic ----
  non_blank_idx <- which(!is_blank)
  if (length(non_blank_idx) == 0) return(NULL)

  non_blank_vals <- etcd_values[non_blank_idx]
  is_dup <- duplicated(non_blank_vals) | duplicated(non_blank_vals, fromLast = TRUE)
  dup_idx <- non_blank_idx[is_dup]
  if (length(dup_idx) == 0) return(NULL)

  dup_vals <- etcd_values[dup_idx]

  msg <- sprintf("Duplicate ETCD value: '%s'.", dup_vals)
  report_error(
    row_number    = as.character(dup_idx),
    variable_name = "ETCD",
    original_value = dup_vals,
    rule_id       = "SD1064",
    error_message = msg
  )
}
