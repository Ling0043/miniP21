# ---- Datasets ----
# SD1298: LB domain with non-numeric LBTOXGR values
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:6,
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4", "SUB5", "SUB6"),
  LBTOXGR = c("2", "Grade 2", "1", "Mild", "3.5", "Grade 3"),
  rownumber_new = c(2, 5, 8, 11, 14, 17),
  stringsAsFactors = FALSE
)

# SD1298: LB domain with all valid numeric LBTOXGR values
lb_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4"),
  LBTOXGR = c("1", "2", "3.5", NA),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable LBTOXGR
lb_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  USUBJID = c("SUB1", "SUB2"),
  LBTESTCD = c("HGB", "ALT"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1298: LBTOXGR is not a numeric value when a numeric scale is used
test_that("SD1298: catches non-numeric LBTOXGR values (one row per error)", {
  expected_error <- data.frame(
    Row = c("5", "17"),
    Variable = c("LBTOXGR", "LBTOXGR"),
    Original_Value = c("Grade 2", "Grade 3"),
    Rule_ID = c("SD1298", "SD1298"),
    Message = "LBTOXGR is not a numeric value when a numeric scale is used.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1298(
    df = lb_violation,
    domain_name = "LB",
    target_vars = "LBTOXGR",
    rule_id = "SD1298"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1298: accepts numeric values including integers and decimals", {
  # '2' (row 2), '1' (row 8), '3.5' (row 14) are valid and must not be reported
  res <- check_sd1298(
    df = lb_violation,
    domain_name = "LB",
    target_vars = "LBTOXGR",
    rule_id = "SD1298"
  )
  
  expect_false("2" %in% res$Row)
  expect_false("8" %in% res$Row)
  expect_false("14" %in% res$Row)
})

test_that("SD1298: ignores missing and blank LBTOXGR values", {
  lb_blank <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    LBSEQ = 1:3,
    LBTOXGR = c(NA, "", "2"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1298(
    df = lb_blank,
    domain_name = "LB",
    target_vars = "LBTOXGR",
    rule_id = "SD1298"
  )
  expect_null(res)
})

test_that("SD1298: returns NULL when no violation", {
  res <- check_sd1298(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "LBTOXGR",
    rule_id = "SD1298"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd1298(
    df = lb_missing,
    domain_name = "LB",
    target_vars = "LBTOXGR",
    rule_id = "SD1298"
  )
  expect_null(res)
})
