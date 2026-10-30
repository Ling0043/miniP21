#' @name fct_get_rules
#'
#' @title get the rule from P21
#'
#' @description Extract the relevant rules from Pinnacle 21
#'     based on the provided dataset
#'
#' @param domain_name the name of the domain dataset, should be a character of two uppercase letters
#' @param type the type of rules to retrieve, either "single" or "multi"
#'
#' @return a data frame containing the rules for the specified domain and type
#'
#' @author Zhu Xiuling
#'
#' @import dplyr stringr
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

fct_get_rules <- function(domain_name, type = NULL) {
  if (!is.character(domain_name)) {
    stop("ERROR: 'domain name' must be a character of two uppercase letters.")
  }

  target_class <- NA
  for (cls in names(domain_class_map)) {
    if (domain_name %in% domain_class_map[[cls]]) {
      target_class <- cls
      break
    }
  }

  pattern_exclude <- paste0("-", domain_name, "\\b")
  # <!  -
  pattern_include_domain <- paste0("(?<!-)\\b", domain_name, "\\b")
  pattern_include_class <- if (!is.na(target_class)) paste0("\\b", target_class, "\\b") else "^$"

  if (type == "single") {
    rule_df <- p21_v34_single %>%
    filter(!is.na(domains)) %>%
    filter(
      !str_detect(domains, pattern_exclude) &
        (
          str_detect(domains, "\\bALL\\b") |
            str_detect(domains, pattern_include_class) |
            str_detect(domains, pattern_include_domain)
        )
    )
    return(rule_df)
  }

  if (type == "multi") {
    rule_df <- p21_v34_multi %>%
    filter(!is.na(domains)) %>%
    filter(
      !str_detect(domains, pattern_exclude) &
        (
          str_detect(domains, "\\bALL\\b") |
            str_detect(domains, pattern_include_class) |
            str_detect(domains, pattern_include_domain)
        )
    )
    return(rule_df)
  }

}

# error_report1 <- fct_validate_sdtm(main = ae)
# error_report2 <- fct_validate_sdtm_cross(list(ae, dm))
# 不允许传入少量dataset