#' @name generate_report
#'
#' @title generate a html report
#'
#' @description SD0002: Required variables (where Core attribute is 'Req')
#'     cannot be null for any records.
#'
#' @param report_df a data.frame of all errors detail
#' @param output_dir the directory to save the report, default is current working directory
#' 
#' @return a html report
#'
#' @author Zhu Xiuling
#'
#' @import dplyr
#' @importFrom logger log_info

# =============================================================================
# Modification History
# =============================================================================
#
# Version  Date        Modified by             Modification(s)
# -------  ----------  ----------------------  -------------------------------
# 1.0      2026-05-10  Author Name             Initial version
#
# =============================================================================


generate_report <- function(report_df, output_dir = "F:/intern2/project_R_sdtm/test_report") {
  template_path <- system.file("report_template.Rmd", package = "miniP21")
  
  # ⭐⭐⭐just for me!!!!!⭐⭐⭐
  if (template_path == "") {
    template_path <- file.path("inst", "report_template.Rmd")
  }

  if (!file.exists(template_path)) {
    stop("The report template 'report_template.Rmd' cannot be found! Please ensure it is placed in the 'inst/' directory.")
  }

  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  output_file <- paste0("P21_Report_", timestamp, ".html")
  output_path <- file.path(output_dir, output_file)

  rmarkdown::render(
    input = template_path,
    output_file = output_path,
    params = list(
      report_data = report_df
    ),
    envir = new.env(),
    quiet = TRUE
  )

  message(sprintf("The report has been generated! Saved to: %s", output_path))

  if (interactive()) {
    utils::browseURL(output_path)
  }
  return(invisible(output_path))
}

