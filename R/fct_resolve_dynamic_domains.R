#' @name fct_resolve_dynamic_domains
#' @title Resolve dynamic reference domains from a marker variable
#' @description Extracts unique, valid domain names from a marker variable
#'   (e.g. RDOMAIN) of the current dataset. Used by the driver to expand the
#'   "@RDOMAIN" marker in rule configuration at dispatch time.
#'
#' @param main_df The current domain dataset.
#' @param rdomain_var Name of the variable holding target domain names
#'   (default "RDOMAIN").
#' @param exclude Domain names to exclude from the result (e.g. the current
#'   domain itself if self-reference is not meaningful).
#'
#' @return A character vector of unique valid domain names (uppercase).
#'   Returns character(0) if nothing valid is found.
#'
#' @author Zhu Xiuling
#'
#' @details
# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-10-27  Zhu Xiuling             Initial version
# =============================================================================

fct_resolve_dynamic_domains <- function(main_df, rdomain_var = "RDOMAIN",
                                        exclude = character(0)) {
  if (is.null(main_df) || !rdomain_var %in% names(main_df)) return(character(0))

  doms <- toupper(trimws(as.character(main_df[[rdomain_var]])))
  doms <- doms[!is.na(doms) & nzchar(doms)]
  # keep only syntactically valid domain names (2-8 uppercase letters)
  doms <- unique(doms[grepl("^[A-Z]{2,8}$", doms)])
  setdiff(doms, exclude)
}
