#' @name check_sd1353
#' @title SD1353 --STRESC does not equal --ORRES when --STRESU is equal to --ORRESU
#' @description SD1353(FINDINGS/FINDINGS‑ABOUT):
#' When --STRESU equals --ORRESU (or both null), and both --STRESC and --ORRES are numeric values,
#' then --STRESC shall equal --ORRES.
#' Supports "--" domain wildcard resolution. Missing required columns return NULL.
#' @param df Input domain dataframe
#' @param domain_name Domain name used to resolve "--" wildcard prefix
#' @param rule_id Fixed "SD1353"
#' @param ... absorb extra config parameters
#' @author Zhu Xiuling
#' @return error report dataframe or NULL
#' @import dplyr
#' @importFrom logger log_info
# ==============================================================================
# Modification History
# ==============================================================================
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026‑08‑24  Zhu Xiuling             Standalone implementation SD1353
# ==============================================================================

library(dplyr)
library(logger)

check_sd1353 <- function(df,
                         domain_name,
                         rule_id, ...) {

  # resolve -- wildcard
  v_stresc  <- gsub("--", domain_name, "--STRESC")
  v_orres   <- gsub("--", domain_name, "--ORRES")
  v_stresu  <- gsub("--", domain_name, "--STRESU")
  v_orresu  <- gsub("--", domain_name, "--ORRESU")

  req_cols <- c(v_stresc, v_orres, v_stresu, v_orresu)

  # required columns missing → skip rule
  if(!all(req_cols %in% colnames(df))) {
    return(NULL)
  }

  if(nrow(df) == 0L) {
    return(NULL)
  }

  # convert to trim‑char
  c_stresc  <- trimws(as.character(df[[v_stresc]]))
  c_orres   <- trimws(as.character(df[[v_orres]]))
  c_stresu  <- trimws(as.character(df[[v_stresu]]))
  c_orresu  <- trimws(as.character(df[[v_orresu]]))

  # Condition A: STRESU == ORRESU OR both are blank
  both_unit_null <- (is.na(df[[v_stresu]]) | c_stresu == "") & (is.na(df[[v_orresu]]) | c_orresu == "")
  unit_equal     <- !is.na(df[[v_stresu]]) & !is.na(df[[v_orresu]]) & (c_stresu == c_orresu)
  cond_unit_ok   <- both_unit_null | unit_equal

  # Condition B: both STRESC and ORRES can coerce to numeric
  num_stresc <- suppressWarnings(as.numeric(c_stresc))
  num_orres  <- suppressWarnings(as.numeric(c_orres))
  cond_both_numeric <- !is.na(num_stresc) & !is.na(num_orres)

  # candidate rows: satisfy A AND B
  candidate_mask <- cond_unit_ok & cond_both_numeric

  # violation: candidate rows AND character value not equal
  violation_mask <- candidate_mask & (c_stresc != c_orres)

  err_idx <- which(violation_mask)
  if(length(err_idx) == 0L){
    return(NULL)
  }

  err_rows <- df$rownumber_new[err_idx]
  var_name_str <- paste0(v_stresc, ",", v_orres, ",", v_stresu, ",", v_orresu)

  # collect original values for message
  err_slice <- df[err_idx, c(v_stresc, v_orres, v_stresu, v_orresu), drop=FALSE]

  error_message_str <- sprintf(
    "When %s equals %s (or both null) and both values are numeric, %s should equal %s.",
    v_stresu, v_orresu, v_stresc, v_orres
  )

  report_df <- report_error(
    row_number     = as.character(err_rows),
    variable_name  = var_name_str,
    original_value = " ",
    rule_id        = rule_id,
    error_message  = error_message_str,
    domain_name   = domain_name
  )

  return(report_df)
}
