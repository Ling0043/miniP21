# ---- Datasets ----
# SD1349: DM domain with inconsistent STUDYID values
dm_violation <- data.frame(
  STUDYID = c("STUDY01", "STUDY01", "STUDY02", "STUDY01", "STUDY03", "STUDY01"),
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4", "SUB5", "SUB6"),
  SEX = c("M", "F", "M", "F", "M", "F"),
  rownumber_new = c(1, 3, 7, 9, 12, 15),
  stringsAsFactors = FALSE
)

# SD1349: DM domain with consistent STUDYID
dm_ok <- data.frame(
  STUDYID = c("STUDY01", "STUDY01", "STUDY01"),
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  SEX = c("M", "F", "M"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1349: DM domain with missing/blank STUDYID values (ignored by check)
dm_blank <- data.frame(
  STUDYID = c("STUDY01", NA, "", "STUDY01", "STUDY02"),
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4", "SUB5"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable STUDYID
dm_missing <- data.frame(
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2"),
  SEX = c("M", "F"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: All STUDYID values missing
dm_all_missing <- data.frame(
  STUDYID = c(NA, "", NA),
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1349: Inconsistent STUDYID
test_that("SD1349: catches inconsistent STUDYID values (one row per error)", {
  expected_error <- data.frame(
    Row = c("7", "12"),
    Variable = c("STUDYID", "STUDYID"),
    Original_Value = c("STUDY02", "STUDY03"),
    Rule_ID = c("SD1349", "SD1349"),
    Message = rep("STUDYID should be consistent for all subjects in DM domain. Expected 'STUDY01'.", 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1349(
    df = dm_violation,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1349: identifies dominant STUDYID correctly regardless of order", {
  # STUDY02 appears 3 times, STUDY01 appears 1 time -> dominant is STUDY02
  dm_reversed <- data.frame(
    STUDYID = c("STUDY02", "STUDY02", "STUDY02", "STUDY01"),
    DOMAIN = "DM",
    USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4"),
    rownumber_new = 1:4,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1349(
    df = dm_reversed,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "4")
  expect_equal(res$Original_Value, "STUDY01")
  expect_equal(res$Message, "STUDYID should be consistent for all subjects in DM domain. Expected 'STUDY02'.")
})

test_that("SD1349: trims whitespace before comparison", {
  dm_trim <- data.frame(
    STUDYID = c("STUDY01", " STUDY01 ", "STUDY01 "),
    DOMAIN = "DM",
    USUBJID = c("SUB1", "SUB2", "SUB3"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1349(
    df = dm_trim,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  expect_null(res)
})

test_that("SD1349: ignores missing and blank STUDYID values", {
  # STUDY01 x2 (dominant), STUDY02 x1 -> 1 error; NA/blank ignored
  res <- check_sd1349(
    df = dm_blank,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "5")
  expect_equal(res$Original_Value, "STUDY02")
})

test_that("SD1349: works for APDM domain", {
  apdm_violation <- data.frame(
    STUDYID = c("STUDY01", "STUDY01", "STUDY09"),
    DOMAIN = "APDM",
    USUBJID = c("SUB1", "SUB2", "SUB3"),
    rownumber_new = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  expected_error <- data.frame(
    Row = "3",
    Variable = "STUDYID",
    Original_Value = "STUDY09",
    Rule_ID = "SD1349",
    Message = "STUDYID should be consistent for all subjects in APDM domain. Expected 'STUDY01'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1349(
    df = apdm_violation,
    domain_name = "APDM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1349: returns NULL when no violation", {
  res <- check_sd1349(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd1349(
    df = dm_missing,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  expect_null(res)
})

# Boundary: All STUDYID values are missing/blank
test_that("Boundary: returns NULL when all STUDYID values are missing or blank", {
  res <- check_sd1349(
    df = dm_all_missing,
    domain_name = "DM",
    target_vars = "STUDYID",
    rule_id = "SD1349"
  )
  expect_null(res)
})
