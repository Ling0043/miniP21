#' @name fct_validate_sdtm
#'
#' @title check the sdtm dataset
#'
#' @description validate the Submission-ready SDTM rds dataset,
#'     including structure, values, and logic and output a report
#'
#' @param df domain dataset to check
#'
#' @return a validation report
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @export
#'
#' @examples fct_validate_sdtm(ta)
#' # minimal reproducible example
#' \dontrun{
#'   fct_validate_sdtm(ta)
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

fct_validate_sdtm <- function(df) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0002] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  if (!"DOMAIN" %in% names(df)) {
    stop("ERROR: Dataset must contain a 'DOMAIN' column.")
  }

  domain_actual <- unique(df[["DOMAIN"]])
  if (length(domain_actual) != 1) {
    stop("ERROR: 'DOMAIN' column must have exactly one unique value per dataset.")
  }

  domain_name <- deparse(substitute(df)) %>% toupper()
  if (domain_actual != domain_name) {
    stop("ERROR: DOMAIN variable should be consistent with the name of the dataset.")
  }

  # 3. Pre-processing ----
  rules_to_check <- fct_get_rules(domain_name = domain_name)

  # if there are no rules for this domain then return null
  if (length(rules_to_check) == 0) {
    message(sprintf("No rules found for domain: %s", domain_name))
    return(NULL)
  }

  # 4. Core logic ----
  all_results <- list()
  for (rule_code in rules_to_check) {
    func_name <- paste0("check_", tolower(rule_code))

    if (!exists(func_name, mode = "function")) {
      message(sprintf("[-] Skip: Function '%s' is not implemented yet.", func_name))
      next
    }
    message(sprintf("[+] Running check: %s...", rule_code))

    check_func <- match.fun(func_name)
    res <- tryCatch({
      check_func(df = df, domain_name = domain_name)
    }, error = function(e) {
      warning(sprintf("Error executing %s: %s", func_name, e$message))
      return(NULL)
    })


    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
      all_results[[rule_code]] <- res
    }
  }

  final_report <- bind_rows(all_results)

  generate_report(report_df = final_report, domain_name = domain_name)
}