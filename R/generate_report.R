#' @name generate_report
#'
#' @title generate a html report
#'
#' @description SD0002: Required variables (where Core attribute is 'Req')
#'     cannot be null for any records.
#'
#' @param df a data.frame of all errors detail
#' @param the domain of the dataset
#' @return a html report
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info
#'
#' @export

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-05-10  Author Name             Initial version
#
# =============================================================================


generate_report <- function(report_df, domain_name, output_dir = getwd()) {
  template_path <- system.file("report_template.Rmd", package = "miniP21")
  
  # ⭐⭐⭐just for me!!!!!⭐⭐⭐
  if (template_path == "") {
    template_path <- file.path("inst", "report_template.Rmd")
  }

  if (!file.exists(template_path)) {
    stop("The report template 'report_template.Rmd' cannot be found! Please ensure it is placed in the 'inst/' directory.")
  }

  timestamp <- format(Sys.Date(), "%Y%m%d")
  output_file <- paste0(domain_name, "_P21_Report_", timestamp, ".html")
  output_path <- file.path(output_dir, output_file)

  rmarkdown::render(
    input = template_path,
    output_file = output_path,
    params = list(
      report_data = report_df,
      domain_name = domain_name
    ),
    envir = new.env(),
    quiet = TRUE
  )

  message(sprintf("The report has been generated! Saved to: %s", output_path))

  if (interactive()) {
    browseURL(output_path)
  }
  return(invisible(output_path))
}