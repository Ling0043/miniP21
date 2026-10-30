#' @name fct_validate_sdtm
#'
#' @title check the sdtm dataset
#'
#' @description validate the Submission-ready SDTM rds dataset,
#'     including structure, values, and logic and output a report
#'
#' @param data_list A named list of SDTM domain datasets to check
#'
#' @return a html validation report
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
fct_validate_sdtm <- function(data_list) {

  # 0. Normalize names ----
  if (is.null(names(data_list)) || any(names(data_list) == "")) {
    stop("ERROR: 'data_list' must be a named list of data frames.")
  }
  names(data_list) <- toupper(trimws(names(data_list)))
  data_list <- lapply(data_list, function(d) {
    d <- d %>% mutate(rownumber_new = dplyr::row_number())
    d
  })

  # Validate dataset names and capture non-standard but plausible names
  checked   <- fct_validate_datalist_names(data_list)
  data_list <- checked$valid_list
  entry_issues <- checked$issues

  if (nrow(data_list) == 0 && length(data_list) == 0) {
    stop("ERROR: No dataset passed entry validation. ",
         "Please check the entry issues: dataset names and DOMAIN variables.")
  }

  reports <- list()
  issues  <- list()

  # 1. Single-domain checks ----
  for (nm in names(data_list)) {
    res <- tryCatch(
      fct_validate_sdtm_single(data_list[[nm]], domain_name = nm),
      error = function(e) {
        issues[[length(issues) + 1]] <<- fct_add_execution_issue(
          "-",
          sprintf("Domain '%s' failed validation entirely: %s", nm, e$message),
          domain_name = nm)
        NULL
      }
    )
    if (!is.null(res)) reports[[length(reports)+1]] <- res
  }

  # 2. Cross-domain checks ----
  multi_report <- fct_validate_sdtm_multi(data_list)
  if (!is.null(multi_report)) reports[[length(reports)+1]] <- multi_report

  # 3. Final report ----
  final_report <- dplyr::bind_rows(reports, issues, entry_issues)

  if (nrow(final_report) == 0) {
    message("All checks passed. No errors found.")
    return(invisible(NULL))
  }

  # return(final_report)
 
 generate_report(final_report)
}
