#' @name check_date_logic
#'
#' @title Check date logic comparison
#'
#' @description SD0013、SD0025、SD1002、SD1331、SD1334、SD1335
#'
#' @param df domain dataset to check
#' @param domain_name the domain name, used to replace "--" in target_vars with the actual domain code
#' @param target_vars a character vector of two variable names containing "--" placeholders, e.g., "--STDTC, --ENDTC"
#' @param rule_id the rule id to check
#' @param operator comparison operator: one of "<", "<=", ">", ">="
#' @param ... additional arguments
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
# 1.0      2026-05-27  Zhu Xiuling             Initial version
#
# =============================================================================

check_date_logic <- function(df, domain_name, target_vars, rule_id, operator, ...) {
  # 1. Pre-processing ----
  var_pairs <- list()
  
  # Handle the case when target_vars contain "--" as placeholders for domain code, e.g., "--STDTC" and "--ENDTC"
  if (any(grepl("^--", target_vars))) {

    suf1 <- sub("^--", "", target_vars[1])
    suf2 <- sub("^--", "", target_vars[2])
    
    all_cols <- names(df)

    cols1 <- all_cols[grepl(paste0(suf1, "$"), all_cols)]
    cols2 <- all_cols[grepl(paste0(suf2, "$"), all_cols)]

    pref1 <- sub(paste0(suf1, "$"), "", cols1)
    pref2 <- sub(paste0(suf2, "$"), "", cols2)

    common_prefs <- intersect(pref1, pref2)

    if (length(common_prefs) == 0) return(NULL)
    
    # variable pairs
    for (p in common_prefs) {
      var_pairs[[length(var_pairs) + 1]] <- c(paste0(p, suf1), paste0(p, suf2))
    }
    
  } else {
    # precise variable name (e.g. c("MHSTDTC", "RFSTDTC"))
    var_pairs[[1]] <- target_vars
  }

  # 2. core logic ----
  all_reports <- list()

  for (pair in var_pairs) {
    var1 <- pair[1]
    var2 <- pair[2]

    d1 <- as.character(df[[var1]])
    d2 <- as.character(df[[var2]])

    valid_idx <- which(!is.na(d1) & trimws(d1) != "" & !is.na(d2) & trimws(d2) != "")
    if (length(valid_idx) == 0) next
    
    d1_valid <- d1[valid_idx]
    d2_valid <- d2[valid_idx]

    min_len <- pmin(nchar(d1_valid), nchar(d2_valid))
    d1_sub <- substr(d1_valid, 1, min_len)
    d2_sub <- substr(d2_valid, 1, min_len)

    is_compliant <- switch(operator,
                           "<"  = d1_sub < d2_sub,
                           "<=" = d1_sub <= d2_sub,
                           ">"  = d1_sub > d2_sub,
                           ">=" = d1_sub >= d2_sub,
                           stop(sprintf("Invalid operator '%s'", operator))
    )


    err_pos <- which(!is_compliant)

    if (length(err_pos) > 0) {
      orig_rows <- valid_idx[err_pos]
      pair_report <- report_error(
        row_number = as.character(orig_rows),
        variable_name = paste(var1, "vs", var2),
        original_value = paste(d1_valid[err_pos], "vs", d2_valid[err_pos]),
        rule_id = rule_id,
        error_message = sprintf("Value of %s must be %s %s.", var1, operator, var2)
      )
      all_reports[[length(all_reports) + 1]] <- pair_report
    }
  }

  if (length(all_reports) == 0) return(NULL)
  return(dplyr::bind_rows(all_reports))
}