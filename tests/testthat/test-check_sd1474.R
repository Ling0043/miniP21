# ---- Datasets ----
# SD1474: Dataset with invalid variable names (lowercase, >8 chars, starts with number)
# Note: check.names = FALSE is required to keep non-syntactic column names
df_violation <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = "SUB1",
  AGE = 45,
  "subjects" = "x",
  "LONGVARIABLENAME" = "y",
  "1SEQ" = "z",
  rownumber_new = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# SD1474: Dataset with all valid variable names
df_ok <- data.frame(
  STUDYID = "STUDY01",
  USUBJID = "SUB1",
  AGE = 45,
  A_SEQ1 = "a",
  WEIGHT = 70.5,
  rownumber_new = 1,
  stringsAsFactors = FALSE
)

# Boundary: Dataset with only invalid names plus rownumber_new
df_all_invalid <- data.frame(
  "lower" = "x",
  "9VAR" = "y",
  "WAYTOOLONGNAME" = "z",
  rownumber_new = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1474: Invalid value for Variable Name
test_that("SD1474: catches invalid variable names (one row per invalid variable)", {
  expected_error <- data.frame(
    Row = rep("-", 3),
    Variable = c("subjects", "LONGVARIABLENAME", "1SEQ"),
    Original_Value = c("subjects", "LONGVARIABLENAME", "1SEQ"),
    Rule_ID = rep("SD1474", 3),
    Message = rep("Variable name does not follow SAS XPORT v5 limitations.", 3),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_violation,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # One row per invalid variable
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1474: accepts valid SAS XPORT v5 variable names", {
  # STUDYID, USUBJID, AGE, A_SEQ1, WEIGHT are all valid
  res <- check_sd1474(
    df = df_ok,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  expect_null(res)
})

test_that("SD1474: flags lowercase letters in variable names", {
  df_lower <- data.frame(
    STUDYID = "STUDY01",
    "subj" = "x",
    rownumber_new = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_lower,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Variable, "subj")
})

test_that("SD1474: flags variable names exceeding 8 characters", {
  df_long <- data.frame(
    STUDYID = "STUDY01",
    "EXACT8CH" = "x",      # exactly 8 chars - valid
    "NINECHARS" = "y",      # 9 chars - invalid
    rownumber_new = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_long,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Variable, "NINECHARS")
})

test_that("SD1474: flags variable names starting with a number", {
  df_digit <- data.frame(
    STUDYID = "STUDY01",
    "2CAT" = "x",
    rownumber_new = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_digit,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Variable, "2CAT")
  expect_equal(res$Row, "-")
})

test_that("SD1474: accepts exactly 8-character names with digits and underscores", {
  df_edge <- data.frame(
    "ABCDEFGH" = "x",    # exactly 8 uppercase chars
    "A_1_B_2" = "y",     # 7 chars with digits and underscores
    "VAR00001" = "z",    # 8 chars starting with letters
    rownumber_new = 1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_edge,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  expect_null(res)
})

test_that("SD1474: excludes rownumber_new from the check", {
  # 'rownumber_new' itself would violate the pattern (lowercase, >8 chars),
  # but must be excluded as an internal helper column
  df_helper <- data.frame(
    STUDYID = "STUDY01",
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1474(
    df = df_helper,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  expect_null(res)
})

# Boundary: All variable names (except rownumber_new) are invalid
test_that("Boundary: reports all invalid variables when none are valid", {
  res <- check_sd1474(
    df = df_all_invalid,
    domain_name = "DM",
    target_vars = NULL,
    rule_id = "SD1474"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Variable, c("lower", "9VAR", "WAYTOOLONGNAME"))
  expect_equal(res$Row, rep("-", 3))
})
