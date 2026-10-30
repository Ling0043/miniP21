# ---- Datasets ----

# SD1267: TA domain with various TAETORD values
ta_invalid <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  ARMCD = c("A", "B", "C", "D", "E", "F"),
  TAETORD = c("1", "2.5", "abc", NA, "", " 3 "),
  rownumber_new = 1:6,
  stringsAsFactors = FALSE
)

# SD1267: TA domain with all valid integers or missing
ta_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  ARMCD = c("A", "B", "C"),
  TAETORD = c("1", "2", NA),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1267: TA domain missing TAETORD column
ta_missing_col <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  ARMCD = c("A", "B"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----

test_that("SD1267: catches non-integer TAETORD values", {
  expected_error <- data.frame(
    Row = c("2", "3"),
    Variable = c("TAETORD", "TAETORD"),
    Original_Value = c("2.5", "abc"),
    Rule_ID = c("SD1267", "SD1267"),
    Message = c("TAETORD variable value must be an integer.",
                "TAETORD variable value must be an integer."),
    stringsAsFactors = FALSE
  )

  res <- check_sd1267(
    df = ta_invalid,
    domain_name = "TA",
    target_vars = "--ETORD",
    rule_id = "SD1267"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1267: returns NULL when all TAETORD values are integers or missing", {
  res <- check_sd1267(
    df = ta_ok,
    domain_name = "TA",
    target_vars = "--ETORD",
    rule_id = "SD1267"
  )

  expect_null(res)
})

test_that("SD1267: returns NULL when TAETORD column is missing", {
  res <- check_sd1267(
    df = ta_missing_col,
    domain_name = "TA",
    target_vars = "--ETORD",
    rule_id = "SD1267"
  )

  expect_null(res)
})

test_that("SD1267: handles direct variable name without wildcard", {
  expected_error <- data.frame(
    Row = c("2", "3"),
    Variable = c("TAETORD", "TAETORD"),
    Original_Value = c("2.5", "abc"),
    Rule_ID = c("SD1267", "SD1267"),
    Message = c("TAETORD variable value must be an integer.",
                "TAETORD variable value must be an integer."),
    stringsAsFactors = FALSE
  )

  res <- check_sd1267(
    df = ta_invalid,
    domain_name = "TA",
    target_vars = "TAETORD",
    rule_id = "SD1267"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1267: trims whitespace before checking integer", {
  # The value " 3 " should be trimmed to "3" and considered valid
  df_trim <- ta_invalid
  df_trim$TAETORD[3] <- " 3 "  # change "abc" to valid with spaces
  df_trim$TAETORD[2] <- "4"    # change "2.5" to integer

  res <- check_sd1267(
    df = df_trim,
    domain_name = "TA",
    target_vars = "--ETORD",
    rule_id = "SD1267"
  )

  expect_null(res)
})