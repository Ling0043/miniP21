#' @name check_cross_study_day
#' @title Check Study Day variable against its date variable and RFSTDTC
#' @description For rows of the current domain, checks the Study Day variable
#'   (--DY/--STDY/--ENDY) against its date variable (--DTC/--STDTC/--ENDTC)
#'   and the subject's reference start date (RFSTDTC in DM):
#'   \itemize{
#'     \item check_type = "required": should be populated when both the date
#'       variable and RFSTDTC include a complete date part.
#'     \item check_type = "imputed": should only be populated when both the
#'       date variable and RFSTDTC include a complete date part.
#'     \item check_type = "correct": should equal the CDISC formula value
#'       (date difference + 1 when date is on or after RFSTDTC, otherwise
#'       the date difference).
#'   }
#'   Applicable to: SD1084, SD1085, SD1086, SD1088, SD1089, SD1090,
#'   SD1092, SD1093, SD1094.
#'
#' @param main_df The current domain dataset.
#' @param ref_dfs The reference domain datasets (a named list, DM expected).
#' @param domain_name The name of the current domain.
#' @param ref_domain The name of the reference domain (e.g. "DM").
#' @param target_vars The Study Day variable to check
#'   (supports "--" wildcard, e.g. "--STDY").
#' @param ref_vars The reference date variable in the reference domain
#'   (e.g. "RFSTDTC").
#' @param rule_id The ID of the rule being checked.
#' @param dtc_var The corresponding date variable in the current domain
#'   (supports "--" wildcard, e.g. "--STDTC").
#' @param check_type Type of check: "required", "imputed" or "correct".
#' @param complete_len Minimum character length defining a complete date
#'   part (default 10, i.e. YYYY-MM-DD).
#' @param ... Additional arguments.
#'
#' @return A data frame with one row per error containing row_number,
#'   variable_name, original_value, rule_id, error_message and domain_name.
#'   Returns NULL if no error is found.
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

check_cross_study_day <- function(main_df, ref_dfs, domain_name, ref_domain,
                            target_vars, ref_vars = "RFSTDTC", rule_id,
                            dtc_var, check_type = "required", complete_len = 10, ...) {

  # 1. Empty data validation ----
  if (is.null(main_df) || nrow(main_df) == 0) return(NULL)

  # 2. Wildcard & defaults ----
  target_vars <- gsub("--", domain_name, target_vars)
  dtc_var     <- gsub("--", domain_name, dtc_var)
  ref_df <- if (!is.null(ref_dfs)) ref_dfs[[ref_domain]] else NULL

  # 3. Reference domain existence ----
  if (is.null(ref_df) || nrow(ref_df) == 0) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("Reference domain %s is missing or empty.", ref_domain),
      domain_name    = domain_name
    ))
  }

  # 4. Helpers ----
  # a date string has a complete date part when it is long enough and parseable
  is_complete_date <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    !is.na(as.Date(substr(x, 1, 10), format = "%Y-%m-%d")) & nchar(x) >= complete_len
  }
  to_num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

  # 5. Align reference date by subject (vectorized) ----
  if (!"USUBJID" %in% names(main_df) || !"USUBJID" %in% names(ref_df)) {
    return(report_error(
      row_number     = "-",
      variable_name  = "-",
      original_value = "-",
      rule_id        = rule_id,
      error_message  = sprintf("USUBJID not found in %s or %s.", domain_name, ref_domain),
      domain_name    = domain_name
    ))
  }

  ref_map <- ref_df %>%
    mutate(ref_date = trimws(as.character(.data[[ref_vars[1]]]))) %>%
    filter(is_complete_date(ref_date)) %>%
    select(USUBJID, ref_date) %>%
    distinct(USUBJID, .keep_all = TRUE)
  ref_date <- ref_map$ref_date[match(main_df$USUBJID, ref_map$USUBJID)]

  # 6. Core check (vectorized) ----
  dy_val    <- to_num(main_df[[target_vars[1]]])
  dy_filled <- !is.na(dy_val)
  dtc_ok    <- is_complete_date(main_df[[dtc_var[1]]])
  ref_ok    <- !is.na(ref_date) & is_complete_date(ref_date)
  both_ok   <- dtc_ok & ref_ok

  if (check_type == "required") {
    err_idx <- which(!dy_filled & both_ok)
  } else if (check_type == "imputed") {
    err_idx <- which(dy_filled & !both_ok)
  } else if (check_type == "correct") {
    dtc_date <- as.Date(substr(trimws(as.character(main_df[[dtc_var[1]]])), 1, 10), format = "%Y-%m-%d")
    ref_date_d <- as.Date(substr(ref_date, 1, 10), format = "%Y-%m-%d")
    diff_days  <- as.numeric(dtc_date - ref_date_d)
    expected   <- ifelse(dtc_date >= ref_date_d, diff_days + 1, diff_days)
    err_idx    <- which(dy_filled & both_ok & dy_val != expected)
  } else {
    stop("Unknown check_type: ", check_type)
  }

  if (length(err_idx) == 0) return(NULL)

  # 7. Report (vectorized, one row per error) ----
  msg_desc <- switch(check_type,
                     required = "should be populated when both date values are provided with complete date part",
                     imputed  = "should not be imputed; it may only be populated when both date values include complete date part",
                     correct  = sprintf("does not match CDISC definition based on %s and %s(%s)",
                                        dtc_var[1], ref_domain, ref_vars[1]))

  report_error(
    row_number     = main_df$rownumber_new[err_idx],
    variable_name  = target_vars[1],
    original_value = as.character(main_df[[target_vars[1]]][err_idx]),
    rule_id        = rule_id,
    error_message  = sprintf("Incorrect value for %s variable: %s",
                             target_vars[1], msg_desc),
    domain_name    = domain_name
  )
}