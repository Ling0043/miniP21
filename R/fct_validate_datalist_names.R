#' @name fct_validate_datalist_names
#' @title Validate names and DOMAIN consistency of the input dataset list
#' @description Two-stage entry validation:
#'   (1) names must be valid SDTM domain codes (fail fast on obvious errors);
#'   (2) each dataset's DOMAIN variable must match its list name
#'   (SUPP-- datasets expect DOMAIN = "SUPPQUAL"; RELREC-type datasets are
#'   exempt). Failed datasets are removed from the validated list and reported,
#'   so downstream validation never runs on mislabeled data.
#' @param data_list A named list of data frames representing SDTM datasets.
#' @return A list with two elements:
#'   $valid_list: named list of datasets that passed all entry checks
#'   $issues:     data.frame of entry failures (empty if none), with the
#'                standard report columns for bind_rows into the final report
fct_validate_datalist_names <- function(data_list) {

  nms <- toupper(trimws(names(data_list)))

  # start Stage 1: Structure and naming validation (fail fast)
  if (is.null(names(data_list)) || any(is.na(names(data_list))) || any(nms == "")) {
    stop("ERROR: 'data_list' must be a named list; every dataset requires a ",
         "SDTM domain name, e.g. list(AE = ae_df, DM = dm_df).")
  }

  dup <- unique(nms[duplicated(nms)])
  if (length(dup) > 0) {
    stop("ERROR: Duplicated dataset name(s) in 'data_list': ",
         paste(dup, collapse = ", "),
         ". Later entries silently overwrite earlier ones.")
  }

  standard_codes <- unlist(domain_class_map, use.names = FALSE)
  is_standard    <- nms %in% standard_codes
  is_prefixed    <- grepl("^(SUPP|AP)[A-Z]{2}$", nms)
  looks_like_code <- grepl("^[A-Z]{2}$", nms) | grepl("^[A-Z]{2}[0-9]$", nms)

  bad <- nms[!is_standard & !is_prefixed & !looks_like_code]
  if (length(bad) > 0) {
    stop("ERROR: The following dataset name(s) in 'data_list' are not valid ",
         "SDTM domain names: ", paste(bad, collapse = ", "), ".\n",
         "Please name each element with its CDISC SDTM domain code, e.g.:\n",
         "  datalist <- list(AE = ae_df, DM = dm_df)\n",
         "Cross-domain rules rely on these names to locate reference datasets.")
  }

  non_standard <- nms[!is_standard & (is_prefixed | looks_like_code)]

  # ========== Stage 2: DOMAIN content consistency check (record issues + exclude) ==========
  # Special datasets without a DOMAIN variable (e.g., relationship datasets) are exempt from DOMAIN checks
  domain_exempt <- c("RELREC", "RELSUB", "RELSPEC", "RELSUB")

  entry_issues <- list()
  valid_list   <- list()

  for (i in seq_along(data_list)) {
    nm <- nms[i]
    df <- data_list[[i]]

    issue <- NULL

    # 2a. Empty dataset
    if (!is.data.frame(df) || nrow(df) == 0) {
      issue <- "Dataset is empty or not a data frame."
    }
    # 2b. Relationship datasets: skip DOMAIN check and allow directly
    else if (nm %in% domain_exempt) {
      valid_list[[nm]] <- df
      next
    }
    # 2c. Existence of DOMAIN variable
    else if (!"DOMAIN" %in% names(df)) {
      issue <- "Dataset does not contain a 'DOMAIN' variable."
    }
    else {
      domain_vals <- unique(trimws(as.character(df[["DOMAIN"]])))
      domain_vals <- domain_vals[!is.na(domain_vals)]

      # 2d. Uniqueness of DOMAIN (separately report empty / multiple values, accurate wording)
      if (length(domain_vals) == 0) {
        issue <- "'DOMAIN' variable contains no non-missing values."
      } else if (length(domain_vals) > 1) {
        issue <- sprintf("'DOMAIN' variable must contain exactly one unique value per dataset, found: %s.",
                         paste(domain_vals, collapse = ", "))
      } else {
        # 2e. Expected value: for SUPP-- datasets, DOMAIN must be SUPPQUAL (SDTMIG standard behavior)
        expected <- if (grepl("^SUPP[A-Z]{2}$", nm)) "SUPPQUAL" else nm

        if (domain_vals != expected) {
          issue <- sprintf("Actual 'DOMAIN' value '%s' does not match the dataset name '%s'. ",
                           domain_vals, expected)
        }
      }
    }

    if (!is.null(issue)) {
      stop("ERROR: Dataset '", nm, "' failed entry validation: ", issue, "\n",
           "Please check whether the correct data frame was passed for '",
           nm, "' before running validation.")
    }

    valid_list[[nm]] <- df
  }

  # Non-standard but seemingly valid domain names (e.g., APAE) → Suggestion, report to the upper layer
  for (nm in non_standard) {
    if (nm %in% names(valid_list)) {
      entry_issues[[length(entry_issues) + 1]] <- fct_add_execution_issue(
        rule_id = "-",
        reason = paste0(
          "Dataset '", nm, "' is not a standard SDTMIG v3.4 domain name. ",
          "Domain-specific rules will not apply to it."),
        domain_name = nm,
        severity = "Suggestion"
      )
    }
  }

  list(
    valid_list = valid_list,
    issues     = entry_issues
  )
}