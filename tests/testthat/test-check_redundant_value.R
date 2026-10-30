# ---- Datasets ----
# SD1039: AE domain for --CAT redundancy check
ae_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = c("AE", "AE", "AE", "AE"),
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4"),
  AETERM = c("Headache", "Nausea", "Nausea", "Fatigue"),
  AECAT = c("HEADACHE", "Cardiac", "Nausea", "Serious"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1040: LB domain for --SCAT redundancy check
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = c("LB", "LB", "LB", "LB"),
  LBSEQ = 1:4,
  LBTESTCD = c("HGB", "ALT", "GLUC", "CREAT"),
  LBTRT = c("Hemoglobin", "Alanine Aminotransferase", "Glucose", "Creatinine"),
  LBSCAT = c("HEMOGLOBIN", "CHEMISTRY", "Hematology", "CHEMISTRY"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1039: AE domain with no redundancy
ae_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  USUBJID = c("SUB1", "SUB2"),
  AETERM = c("Headache", "Nausea"),
  AECAT = c("Systemic", "Gastrointestinal"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable
ae_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  USUBJID = "SUB1",
  AETERM = "Headache",
  rownumber_new = 1,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1039: --CAT value is redundant with other variable values
test_that("SD1039: catches AECAT redundant with AETERM (case-insensitive, one row per error)", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("AECAT", "AECAT"),
    Original_Value = c("HEADACHE", "Nausea"),
    Rule_ID = c("SD1039", "SD1039"),
    Message = rep("Value of AECAT should not be identical to that in DOMAIN, AETERM.", 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_redundant_value(
    df = ae_violation,
    domain_name = "AE",
    target_vars = "--CAT",
    rule_id = "SD1039",
    compare_vars = c("DOMAIN", "--TERM", "--TESTCD", "--TRT", "--DECOD", "--BODSYS")
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1039: returns NULL when no redundancy", {
  res <- check_redundant_value(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "--CAT",
    rule_id = "SD1039",
    compare_vars = c("DOMAIN", "--TERM", "--TESTCD", "--TRT", "--DECOD", "--BODSYS")
  )
  expect_null(res)
})

test_that("SD1039: case-sensitive comparison with ignore_case = FALSE", {
  # Row 1: AECAT = "HEADACHE" vs AETERM = "Headache" -> not equal when case-sensitive
  res <- check_redundant_value(
    df = ae_violation,
    domain_name = "AE",
    target_vars = "--CAT",
    rule_id = "SD1039",
    compare_vars = c("DOMAIN", "--TERM"),
    ignore_case = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "3") # Only 'nausea' == 'Nausea'? No: case-sensitive 'nausea' != 'Nausea'
})

# ---- Tests ----
# SD1040: --SCAT value is redundant with other variable values
test_that("SD1040: catches LBSCAT redundant with LBTEST (one row per error)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBSCAT",
    Original_Value = "HEMOGLOBIN",
    Rule_ID = "SD1040",
    Message = "Value of LBSCAT should not be identical to that in DOMAIN, LBTESTCD, LBTRT.",
    stringsAsFactors = FALSE
  )
  
  res <- check_redundant_value(
    df = lb_violation,
    domain_name = "LB",
    target_vars = "--SCAT",
    rule_id = "SD1040",
    compare_vars = c("DOMAIN", "--TERM", "--TESTCD", "--TRT", "--DECOD", "--BODSYS")
    # Note: "--TRT" corresponds to LBTRT
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1040: redundant with DOMAIN variable", {
  dm_scat <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = c("DM", "DM"),
    SCAT = c("DM", "GROUP1"),
    rownumber_new = 1:2,
    stringsAsFactors = FALSE
  )
  
  res <- check_redundant_value(
    df = dm_scat,
    domain_name = "DM",
    target_vars = "SCAT",
    rule_id = "SD1040",
    compare_vars = c("DOMAIN", "--TERM", "--TESTCD")
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "1")
  expect_equal(res$Original_Value, "DM")
})

test_that("SD1040: ignores missing/blank comparison and target values", {
  lb_blank <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    LBSEQ = 1:4,
    LBTESTCD = c("HGB", "", NA, "ALT"),
    LBSCAT = c("", NA, "HGB", "CHEM"),
    rownumber_new = 1:4,
    stringsAsFactors = FALSE
  )
  
  res <- check_redundant_value(
    df = lb_blank,
    domain_name = "LB",
    target_vars = "--SCAT",
    rule_id = "SD1040",
    compare_vars = "--TESTCD"
  )
  expect_null(res)
})

test_that("SD1040: skips comparison variables that do not exist in df", {
  # LBTRT and LBDECOD do not exist; only LBTESTCD and LBTEST checked
  res <- check_redundant_value(
    df = lb_violation,
    domain_name = "LB",
    target_vars = "--SCAT",
    rule_id = "SD1040",
    compare_vars = c("--TESTCD", "--TRT", "--DECOD")
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "1")
  # Message should only reference existing variables
  expect_equal(res$Message, "Value of LBSCAT should not be identical to that in LBTESTCD, LBTRT.")
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_redundant_value(
    df = ae_missing,
    domain_name = "AE",
    target_vars = "--CAT",
    rule_id = "SD1039",
    compare_vars = c("DOMAIN", "--TERM")
  )
  expect_null(res)
})

# Boundary: No comparison variables exist in df
test_that("Boundary: returns NULL when no comparison variables exist in df", {
  res <- check_redundant_value(
    df = ae_violation,
    domain_name = "AE",
    target_vars = "--CAT",
    rule_id = "SD1039",
    compare_vars = c("--TESTCD", "--TRT", "--DECOD", "--BODSYS")
  )
  expect_null(res)
})
