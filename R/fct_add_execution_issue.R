
fct_add_execution_issue <- function(rule_id, reason, domain_name, severity = "Execution Issue") {
  report_error(
    row_number     = "-",
    variable_name  = "-",
    original_value = "-",
    rule_id        = rule_id,
    error_message  = reason,
    domain_name   = domain_name
  ) %>%
    mutate(
      Severity = severity
    )
}
