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
# 1.0      2026-05-10  Zhu Xiuling             Initial version
# 1.1      2026-06-05  Zhu Xiuling             Update the structure
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

  # create a row number column for df
  df <- df %>%
    mutate(rownumber_new = row_number())

  # 3. Pre-processing ----
  rules_df <- fct_get_rules(domain_name = domain_name)

  # if there are no rules for this domain then return null
  if (is.null(rules_df) || nrow(rules_df) == 0) {
    message(sprintf("No rules found for domain: %s", domain_name))
    return(NULL)
  }

  # 4. Core logic ----
  all_results <- list()
  pkg_env <- asNamespace("miniP21")

  for (i in seq_len(nrow(rules_df))) {
    rule_id     <- rules_df$code[i]
    func_name   <- rules_df$coreFunction[i]
    target_vars <- rules_df$targetVariable[i]
    rule_params <- rules_df$ruleParams[i]

    # chech function name is not NA or empty
    if (is.na(func_name) || func_name == "") {
      message(sprintf("[-] Skip: Core function not defined for rule '%s'.", rule_id))
      next
    }

    # check function exists in the package environment
    if (!exists(func_name, mode = "function", envir = pkg_env)) {
      message(sprintf("[-] Skip: Function '%s' for rule '%s' is not implemented yet.", func_name, rule_id))
      next
    }

    if (!is.na(target_vars) && trimws(target_vars) != "") {
          target_vars_vec <- trimws(unlist(strsplit(as.character(target_vars), split = ",")))
        } else {
          target_vars_vec <- NULL
        }
    # basic parameters
    args_list <- list(
          df = df,
          domain_name = domain_name,
          target_vars = target_vars_vec,
          rule_id = rule_id
        )
    
    # If rule_params is not NA or empty, parse it and add to args_list
    if (!is.na(rule_params) && trimws(rule_params) != "") {
      specific_args <- tryCatch({
        jsonlite::fromJSON(rule_params)
      }, error = function(e) {
        warning(sprintf("Failed to parse ruleParams for %s: %s", rule_id, e$message))
      })

      args_list <- c(args_list, specific_args)
    }

    # --- runing code ---    
    check_func <- get(func_name, mode = "function", envir = pkg_env)

    res <- tryCatch({
      do.call(check_func, args_list)
    }, error = function(e) {
      warning(sprintf("Error executing %s for %s: %s", func_name, rule_id, e$message))
      return(NULL)
    })
    
    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
      all_results[[rule_id]] <- res
    }
  }

  # Combine and Report ----
  if (length(all_results) == 0) {
    message(sprintf("All checks passed for domain: %s. No errors found.", domain_name))
    return(NULL)
  }

  final_report <- dplyr::bind_rows(all_results)

  generate_report(report_df = final_report, domain_name = domain_name)
}