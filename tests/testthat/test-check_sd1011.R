# ---- Datasets ----
# SD1011: DI domain for TDSTOFF
di_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DI",
  DISEQ = 1:5,
  TDSTOFF = c("PT2H", "2H", "PT30M", "P1DT", "P4DT12H"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# SD1011: AE domain for AEDUR and AEELTM (multiple variables, both invalid)
ae_multi_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AEDUR = c("P1Y2M", "PT", "P2D"),
  AEELTM = c("PT10M", "90 MIN", NA),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1011: AE domain with all valid ISO 8601 values
ae_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:5,
  AEDUR = c("P1Y", "P18D", "P1Y2M3W4DT10H30M", "PT0.5S", NA),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# Boundary: DI domain missing target variable
di_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DI",
  DISEQ = 1:2,
  DSTDTC = c("2021-01-01", "2021-02-01"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1011: Invalid ISO 8601 value for variable
test_that("SD1011: catches invalid TDSTOFF values in DI domain", {
  expected_error <- data.frame(
    Row = c("2", "4"),
    Variable = c("TDSTOFF", "TDSTOFF"),
    Original_Value = c("2H", "P1DT"),
    Rule_ID = "SD1011",
    Message = "Values must conform to ISO 8601 format for this variable",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1011(
    df = di_violation,
    domain_name = "DI",
    target_vars = c("--DUR", "--ELTM", "--EVLINT", "--STINT", "--ENINT", "TDSTOFF", "TDTGTPAI", "TDMINPAI", "TDMAXPAI"),
    rule_id = "SD1011"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # Collapsed into a single row error report
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1011: checks multiple variables (AEDUR and AEELTM) via wildcard resolution", {
  res <- check_sd1011(
    df = ae_multi_violation,
    domain_name = "AE",
    target_vars = "--DUR,--ELTM",
    rule_id = "SD1011"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One report per invalid variable
  
  # First report: AEDUR
  expect_equal(res$Row[1], "2")
  expect_equal(res$Variable[1], "AEDUR")
  expect_equal(res$Original_Value[1], "PT")
  
  # Second report: AEELTM
  expect_equal(res$Row[2], "2")
  expect_equal(res$Variable[2], "AEELTM")
  expect_equal(res$Original_Value[2], "90 MIN")
  
  expect_equal(res$Rule_ID, c("SD1011", "SD1011"))
  expect_equal(res$Message, rep("Values must conform to ISO 8601 format for this variable", 2))
})

test_that("SD1011: skips variables that do not exist and reports only existing ones", {
  res <- check_sd1011(
    df = ae_multi_violation,
    domain_name = "AE",
    target_vars = "--DUR,--EVLINT", # AEEVLINT does not exist
    rule_id = "SD1011"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Variable, "AEDUR")
  expect_equal(res$Original_Value, "PT")
})

test_that("SD1011: accepts various valid ISO 8601 duration formats (single variable)", {
  vs_ok <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "VS",
    VSSEQ = 1:4,
    VSEVLINT = c("PT1H30M", "PT15M", "P7D", "PT2.5S"),
    rownumber_new = 1:4,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1011(
    df = vs_ok,
    domain_name = "VS",
    target_vars = "--EVLINT",
    rule_id = "SD1011"
  )
  expect_null(res)
})

test_that("SD1011: returns NULL when all values are valid across multiple variables", {
  res <- check_sd1011(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "--DUR",
    rule_id = "SD1011"
  )
  expect_null(res)
})

test_that("SD1011: ignores missing and blank values", {
  di_blank <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "DI",
    DISEQ = 1:3,
    TDSTOFF = c(NA, "", "PT1H"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1011(
    df = di_blank,
    domain_name = "DI",
    target_vars = "TDSTOFF",
    rule_id = "SD1011"
  )
  expect_null(res)
})

# Boundary: No target variables exist in df
test_that("Boundary: returns NULL when no target variables exist in df", {
  res <- check_sd1011(
    df = di_missing,
    domain_name = "DI",
    target_vars = c("TDSTOFF","TDTGTPAI"),
    rule_id = "SD1011"
  )
  expect_null(res)
})