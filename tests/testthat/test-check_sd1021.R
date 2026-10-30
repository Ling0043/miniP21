# ---- Datasets ----
# SD1021: DM domain with unexpected character values (all variables checked)
dm_violation <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = c(" SUB1", "SUB2", "SUB3", "SUB4", "SUB5"),
  ARM = c("Placebo", "Active\nArm", "Control", "Screen Fail", "Active"),
  SEX = c("M", "F", ".", "M", "F"),
  RACE = c("WHITE", "BLACK", "ASIAN", " white ", "OTHER"),
  COVAL1 = c(" COVAL leading space", ".", "Normal", "Text", "Value"),
  TSVAL1 = c("TS ok", ".", "CR\rin value", "Text", "Value"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# SD1021: DM domain with clean values (no violations)
dm_ok <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = c("SUB1", "SUB2"),
  ARM = c("Placebo", "Active"),
  SEX = c("M", "F"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: No character columns available (all numeric)
dm_numeric_only <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = "SUB1",
  AGE = c(45, 52),
  WEIGHT = c(70.5, 80.2),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1021: Unexpected character value in variable
test_that("SD1021: catches leading space, carriage return, and '.' values across all variables", {
  expected_error <- data.frame(
    Row = c("1", "2", "3", "4"),
    Variable = c("USUBJID", "ARM", "SEX", "RACE"),
    Original_Value = c(" SUB1", "Active\nArm", ".", " white "),
    Rule_ID = "SD1021",
    Message = "Character values should not have leading space, carriage return, or '.' as an entire value.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1021(
    df = dm_violation,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4) # One row per violation
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1021: excludes COVALn and TSVALn variables from the check", {
  # COVAL1 row 1 (leading space) and TSVAL1 rows 2-3 ('.', carriage return) 
  # must NOT be reported - only USUBJID/ARM/SEX violations appear
  res <- check_sd1021(
    df = dm_violation,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  
  expect_false(any(grepl("COVAL|TSVAL", res$Variable)))
})

test_that("SD1021: does not flag trailing space or internal whitespace", {
  # RACE row 4 has ' white ' (leading AND trailing) - leading space should be caught,
  # but a value with only trailing space should pass
  dm_trailing <- data.frame(
    STUDYID = "STUDY01",
    RACE = c("WHITE ", "BLACK", "ASIAN"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1021(
    df = dm_trailing,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  expect_null(res)
})

test_that("SD1021: supports custom exclude_pattern", {
  # Exclude USUBJID as well, so only ARM and SEX violations remain
  res <- check_sd1021(
    df = dm_violation,
    domain_name = "DM",
    exclude_pattern = "^(COVAL|TSVAL)\\d*$|^USUBJID$",
    rule_id = "SD1021"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$Variable, c("ARM", "SEX", "RACE"))
  expect_equal(res$Row, c("2", "3", "4"))
})

test_that("SD1021: handles factors as character-like columns", {
  dm_factor <- data.frame(
    STUDYID = "STUDY01",
    SEX = factor(c("M", ".", "F")),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1021(
    df = dm_factor,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$Variable, "SEX")
  expect_equal(res$Row, "2")
  expect_equal(res$Original_Value, ".")
})

test_that("SD1021: returns NULL when no violation", {
  res <- check_sd1021(
    df = dm_ok,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  expect_null(res)
})

test_that("SD1021: returns NULL when target_vars provided but not found", {
  res <- check_sd1021(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "--NOTEXIST",
    rule_id = "SD1021"
  )
  expect_null(res)
})

test_that("SD1021: checks only specified target_vars when provided", {
  res <- check_sd1021(
    df = dm_violation,
    domain_name = "DM",
    target_vars = "ARM",
    rule_id = "SD1021"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(res$Variable, "ARM")
  expect_equal(res$Row, "2")
})

# Boundary: No character columns in df
test_that("Boundary: returns NULL when no character-like columns exist", {
  res <- check_sd1021(
    df = dm_numeric_only,
    domain_name = "DM",
    rule_id = "SD1021"
  )
  expect_null(res)
})
