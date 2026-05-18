#' @name check_sd1009
#'
#' @title check invalide ETCD value
#'
#' @description SD1009: The value of Element Code (ETCD) should be 
#'     no more than 8 characters in length.
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
#' @examples check_sd1009(ta)
#' # minimal reproducible example
#' \dontrun{
#'   result <- check_sd1009(ta)
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

check_sd1009 <- function(df, domain_name) {

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

  # 4. Core logic ----
  err_idx <- which(nchar(etcd_values) > 8 & !is.na(df$ETCD))

  all_errors <- list()
  for (idx in err_idx) {
    actual_val <- etcd_values[idx]
    all_errors[[length(all_errors) + 1]] <- report_error(
      row_number = as.character(idx),
      variable_name = "ETCD",
      rule_id = "SD1009",
      error_message = sprintf("Invalid value for ETCD: '%s' is %d characters long (max 8).", 
                              actual_val, nchar(actual_val))
    )
  }

  # 5.Logging end ----
  if (length(all_errors) == 0) {
    return(NULL)
  } else {
    return(bind_rows(all_errors))
  }
}
