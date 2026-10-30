# ---- Datasets ----
# SD2019: DM domain for AGETXT format check
dm_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4", "SUB5", "SUB6", "SUB7"),
  AGETXT = c("18-65", "18-65 ", "Over 65", "18.5-65.5", "65+", NA, ""),
  rownumber_new = 1:7,
  stringsAsFactors = FALSE
)

# SD2019: DM domain with valid formats
dm_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  AGETXT = c("18-65", "18.5-65.5", NA),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable AGETXT
dm_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = "SUB1",
  AGE = 45,
  rownumber_new = 1,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD2019: Invalid value for AGETXT
test_that("SD2019: catches invalid AGETXT values not in 'number-number' format", {
  expected_error <- data.frame(
    Row = c("3", "5"),
    Variable = "AGETXT",
    Original_Value = c("Over 65", "65+"),
    Rule_ID = "SD2019",
    Message = "AGETXT must be in 'number-number' format",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd2019(
    df = dm_violation,
    domain_name = "DM",
    target_vars = "AGETXT",
    rule_id = "SD2019"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # Collapsed into a single row error report
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2019: returns NULL when no violation", {
  res <- check_sd2019(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "AGETXT",
    rule_id = "SD2019"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd2019(
    df = dm_missing,
    domain_name = "DM",
    target_vars = "AGETXT",
    rule_id = "SD2019"
  )
  expect_null(res)
})
