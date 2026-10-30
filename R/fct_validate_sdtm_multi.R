#' @name fct_validate_sdtm_multi
#'
#' @title check the sdtm dataset (multiple domains)
#'
#' @description validate the Submission-ready SDTM rds dataset,
#'     including structure, values, and logic and output a report
#'
#' @param data_list named list of domain datasets to check
#'
#' @return a validation report
#'
#' @author Zhu Xiuling
#'
#' @import dplyr

# ==============================================================================
# Modification History
# ==============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-09-21   Zhu Xiuling             Initial version
# 2.0      2026-10-27   Zhu Xiuling             Added dynamic "@RDOMAIN" support for required datasets check
# ==============================================================================

fct_validate_sdtm_multi <- function(data_list) {

  # 1. Input validation ----
  if (!is.list(data_list) || is.data.frame(data_list)) {
    stop("ERROR: 'data_list' must be a named list of data frames.")
  }
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    stop("ERROR: 'data_list' must be a named list (dataset names required).")
  }
  if (!all(sapply(data_list, is.data.frame))) {
    stop("ERROR: All elements of 'data_list' must be data frames.")
  }
  if (any(sapply(data_list, nrow) == 0)) {
    stop("ERROR: Datasets must have at least one record.")
  }

  # 2. Normalize names & add row numbers ----
  names(data_list) <- toupper(trimws(names(data_list)))
  data_list <- lapply(data_list, function(d) {
    d %>% mutate(rownumber_new = dplyr::row_number())
  })

  all_results      <- list()
  execution_issues <- list()
  pkg_env <- asNamespace("miniP21")

  # 3. Loop over datasets, get cross-domain rules for each ----
  for (domain_name in names(data_list)) {

    main_df <- data_list[[domain_name]]   # <-- NEW: available to all steps below

    rules_df <- fct_get_rules(domain_name, type = "multi")
    if (is.null(rules_df) || nrow(rules_df) == 0) next

    for (i in seq_len(nrow(rules_df))) {

      rule_id     <- rules_df$code[i]
      func_name   <- rules_df$coreFunction[i]
      target_vars <- rules_df$targetVariable[i]

      if (!is.na(target_vars) && trimws(target_vars) != "") {
        target_vars <- trimws(unlist(strsplit(as.character(target_vars), split = ",")))
      } else {
        target_vars <- NULL
      }

      # --- 3a. required datasets check: if missing → Suggestion, skip ----
      #     (with dynamic "@RDOMAIN" marker support)                # <-- CHANGED
      req <- if (!is.na(rules_df$requiredDomains[i]) && rules_df$requiredDomains[i] != "") {
        trimws(strsplit(rules_df$requiredDomains[i], "[;,]")[[1]])
      } else character(0)

      if ("@RDOMAIN" %in% req) {                                     # <-- NEW
        req <- c(setdiff(req, "@RDOMAIN"),
                 fct_resolve_dynamic_domains(main_df))
      }

      missing_req <- setdiff(req, names(data_list))
      if (length(missing_req) > 0) {
        execution_issues[[length(execution_issues) + 1]] <- fct_add_execution_issue(
          rule_id,
          sprintf("Required dataset(s) '%s' not found in input list. Rule skipped - please add them to run this check.",
                  paste(missing_req, collapse = ", ")),
          domain_name = domain_name,
          severity = "Suggestion"
        )
        next
      }

      # --- 3b. function existence ----
      if (is.na(func_name) || func_name == "" ||
          !exists(func_name, mode = "function", envir = pkg_env)) {
        execution_issues[[length(execution_issues) + 1]] <- fct_add_execution_issue(
          rule_id,
          if (is.na(func_name) || func_name == "") "Core function not defined"
          else sprintf("Function '%s' not implemented", func_name),
          domain_name = domain_name)
        next
      }

      # --- 3c. parse params: failure → record and skip ----
      params <- tryCatch(
        jsonlite::fromJSON(rules_df$ruleParams[i]),
        error = function(e) {
          execution_issues[[length(execution_issues) + 1]] <<- fct_add_execution_issue(
            rule_id,
            sprintf("Failed to parse ruleParams: %s", e$message),
            domain_name = domain_name)
          NULL
        }
      )
      if (is.null(params)) next

      # --- 3d. ref_domain (with dynamic "@RDOMAIN" support) ----     # <-- CHANGED
      ref_domain_name <- as.vector(params$ref_domain)
      if (!is.null(ref_domain_name) && length(ref_domain_name) == 1 &&
          ref_domain_name == "@RDOMAIN") {
        dyn_domains <- fct_resolve_dynamic_domains(main_df)
        if (length(dyn_domains) == 0) {
          execution_issues[[length(execution_issues) + 1]] <- fct_add_execution_issue(
            rule_id,
            sprintf("No valid domain found in RDOMAIN column of %s. Rule skipped.",
                    domain_name),
            domain_name = domain_name)
          next
        }
        params$ref_domain   <- dyn_domains   # keep resolved value for reporting
        ref_domain_name     <- dyn_domains
      }

      if (!is.null(ref_domain_name) && length(ref_domain_name) == 1 &&    # <-- NEW
          ref_domain_name == "@ALL_DOMAINS") {
        ref_domain_name     <- names(data_list)   # all provided datasets are valid domains
        params$ref_domain   <- ref_domain_name
      }

      args_list <- c(
        list(
          main_df     = main_df,
          ref_dfs     = data_list[ref_domain_name],
          domain_name = domain_name,
          target_vars = target_vars,
          rule_id     = rule_id
        ),
        params
      )

      # --- 3e. execute ----
      check_func <- get(func_name, mode = "function", envir = pkg_env)
      res <- tryCatch(do.call(check_func, args_list), error = function(e) {
        execution_issues[[length(execution_issues) + 1]] <<- fct_add_execution_issue(
          rule_id = rule_id,
          reason = sprintf("Execution error: %s", e$message),
          domain_name = domain_name)
        NULL
      })

      if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) {
        all_results[[length(all_results) + 1]] <- res
      }
    }
  }

  # 4. Combine ----
  final_report <- dplyr::bind_rows(all_results, execution_issues)
  if (nrow(final_report) == 0) {
    message("All cross-domain checks passed. No errors found.")
    return(NULL)
  }

  return(final_report)
}
