#' @name fct_get_rules
#'
#' @title get the rule from P21
#'
#' @description Extract the relevant rules from Pinnacle 21
#'     based on the provided dataset
#'
#' @param df domain dataset to check
#'
#' @return rule code
#'
#' @author Zhu Xiuling
#'
#' @import dplyr stringr
#' @importFrom logger log_info
#'
#' @export
#'
#' @examples get_rules(ta)
#' # minimal reproducible example
#' \dontrun{
#'   result <- get_rules(ta)
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
library(stringr)

fct_get_rules <- function(domain_name) {

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

  rule_df <- p21_v34 %>%
    filter(!is.na(domains)) %>%
    filter(
      !str_detect(domains, pattern_exclude) &
        (
          str_detect(domains, "\\bALL\\b") |
            str_detect(domains, pattern_include_class) |
            str_detect(domains, pattern_include_domain)
        )
    )

  return(rule_df$code)
}
