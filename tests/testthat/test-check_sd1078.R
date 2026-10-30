# ---- Datasets ----
# SD1078: AE domain with all-missing permissible variable
ae_empty_var <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AESPID = c(NA, NA, NA),
  AETERM = c("Headache", "Nausea", "Fever"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1078: AE domain with partially filled permissible variable (No Error)
ae_partial_var <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AESPID = c("SP01", NA, NA),
  AETERM = c("No", "Nausea", "Fever"),
  rownumber_new = 1:3,
  EmptyVar = c(1, NA, NA),
  stringsAsFactors = FALSE
)

# SD1078: AE domain with no empty permissible variables (No Error)
ae_clean <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AETERM = c("Headache", "NAUSEA", "FEVER"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1078: Permissible variable with missing value for all records
test_that("SD1078: catches permissible variable with missing value for all records", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "AESPID",
    Original_Value = "",
    Rule_ID = "SD1078",
    Record_Count = 3,
    Message = "Permissible variable(s) AESPID should not be present in domain, as they have missing value for all records.",
    stringsAsFactors = FALSE
  )
  
  # 1. Test with automatic detection of empty columns
  res <- check_SD1078(
    df = ae_empty_var,
    domain_name = "AE",
    rule_id = "SD1078",
    permissible_vars = NULL
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1078: returns NULL when no violation (all vars populated or mixed)", {
  # Test with partially filled variable
  res <- check_SD1078(
    df = ae_partial_var,
    domain_list = "AE",
    rule_id = "SD1078",
    permissible_vars = NULL
  )
  expect_null(res)
  
  # Test with clean dataset (no empty columns)
  res <- check_SD1078(
    df = ae_clean,
    domain_list = "AE",
    rule_id = "SD1078",
    permissible_vars = NULL
  )
  expect_null(res)
})

test_that("SD1078: respects the permissible_vars whitelist", {
  # Dataset has an empty column 'EmptyVar'
  df <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "AE",
    AESEQ = 1,
    AETERM = "Headache",
    EmptyVar = NA,
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  # Case 1: Check specific list that includes the empty var -> Error
  res <- check_SD1078(
    df = df,
    domain_name = "AE",
    rule_id = "SD1078",
    permissible_vars = c("AETERM", "EmptyVar")
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$Variable, "EmptyVar")
  
  # Case 2: Check specific list that EXCLUDES the empty var -> No Error
  res <- check_SD1078(
    df = df,
    domain_name = "AE",
    rule_id = "SD1078",
    permissible_vars = c("AETERM")
  )
  expect_null(res)
})
