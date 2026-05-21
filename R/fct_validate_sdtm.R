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
#' @examples
#' # minimal reproducible example
#' \dontrun{
#'   ta <- data.frame(
#'   Row = 1:6,
#'  STUDYID = c("EX7", "EX7", "EX7", "EX7", "EX7", "EX7"),
#'   DOMAIN = c("TA", "TA", "TA", "TA", "TA", "TA"),
#'   ARMCD = c(1, 1, 1, 1, 1, 1),
#'   ARM = c("CR", "CR", NA, "CR", "", "CR"),
#'   TAETORD = as.character(1:6),
#'   ETCD = c("SCRN", "ICR", "CRNS", "C", "C", "FU"),
#'   ELEMENT = c("Screen", "Initial Chemo + RT",
#'               "Chemo+RT (non-Surgery)", "Chemo",
#'               "Chemo", "Off Treatment Follow-up"),
#'   TABRANCH = c("Randomized to CR", "", "",
#'                "", "", ""),
#'   TATRANS = c("", "", "If progression, skip to Follow-up.",
#'               "", "", ""),
#'   EPOCH = c("SCREENING", "INDUCTION TREATMENT",
#'             "INDUCTION TREATMENT", "CONTINUATION TREATMENT",
#'             "CONTINUATION TREATMENT", "FOLLOW-UP")
#' )
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

# Library imports ----
library(dplyr)
library(logger)

fct_validate_sdtm <- function(df) {
  #  # 1. Logging start ----
  # logger::log_info("[check_sd0002] Start")

  # 2. Input validation ----
  if (!is.data.frame(df)) {
    stop("ERROR: 'data' must be a data frame.")
  }

  if (nrow(df) == 0) {
    stop("ERROR: Dataset must have at least one record.")
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

    pkg_env <- asNamespace("miniP21")
    func_name <- paste0("check_", tolower(rule_code))
   
    if (!exists(func_name,
             mode = "function",
             envir = pkg_env)) {
      message(sprintf("[-] Skip: Function '%s' is not implemented yet.", func_name))
      next
    }
    message(sprintf("[+] Running check: %s...", rule_code))

    check_func <- get(func_name,
                  mode = "function",
                  envir = pkg_env)
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