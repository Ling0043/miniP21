# ---- Datasets ----
# Note: The provided function code is named `check_sd1464` and validates '--DOSTXT' 
# (Rule SD1464). The rule text SD1467 provided in the prompt seems to be a 
# copy-paste error regarding '--OCCUR'. These tests correctly target the provided 
# `check_sd1464` function using the "SD1464" rule ID.

# SD1464: EX domain for --DOSTXT numeric check
ex_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:6,
  EXDOSE = c(500, 100.5, NA, NA, NA, NA),
  EXDOSTXT = c("500", "100.5", "Two tablets", "1.2E3", NA, " "),
  rownumber_new = 1:6,
  stringsAsFactors = FALSE
)

# SD1464: EX domain with valid text values (No Error)
ex_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:4,
  EXDOSTXT = c("Three pills", "10 mg", "Unknown", NA),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable EXDOSTXT
ex_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:2,
  EXDOSE = c(500, 100),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1464: Check that --DOSTXT is not a numeric value
test_that("SD1464: catches numeric values in EXDOSTXT", {
  expected_error <- data.frame(
    Row = c("1", "2", "4"),
    Variable = "EXDOSTXT",
    Original_Value = c("500", "100.5", "1.2E3"),
    Rule_ID = "SD1464",
    Message = "EXDOSTXT should not be a numeric value; use --DOSE for numeric representation",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1464(
    df = ex_violation,
    domain_name = "EX",
    target_vars = "--DOSTXT",
    rule_id = "SD1464"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # Collapsed into a single row error report
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1464: returns NULL when no violation", {
  res <- check_sd1464(
    df = ex_ok,
    domain_name = "EX",
    target_vars = "--DOSTXT",
    rule_id = "SD1464"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd1464(
    df = ex_missing,
    domain_name = "EX",
    target_vars = "--DOSTXT",
    rule_id = "SD1464"
  )
  expect_null(res)
})
