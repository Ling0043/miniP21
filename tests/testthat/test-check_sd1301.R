# ---- Datasets ----
# SD1301: TD domain with invalid TDSTOFF values
td_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TD",
  TDSEQ = 1:6,
  TDSTOFF = c("0", "P80Y", "80", "P1Y2M", "P1DT12H", "P6M"),
  TDTESTCD = c("DURDIS", "DURDIS", "AGEGRP", "DURDIS", "DURDIS", "DURDIS"),
  rownumber_new = c(1, 4, 7, 10, 13, 16),
  stringsAsFactors = FALSE
)

# SD1301: TD domain with all valid TDSTOFF values
td_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TD",
  TDSEQ = 1:5,
  TDSTOFF = c("0", "P80Y", "P1Y2M3W4D", "-P6M", NA),
  TDTESTCD = c("DURDIS", "DURDIS", "AGEGRP", "DURDIS", "DURDIS"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable TDSTOFF
td_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TD",
  TDSEQ = 1:2,
  TDTESTCD = c("DURDIS", "AGEGRP"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1301: Invalid value for TDSTOFF variable
test_that("SD1301: catches invalid TDSTOFF values (one row per error)", {
  expected_error <- data.frame(
    Row = c("7", "13"),
    Variable = c("TDSTOFF", "TDSTOFF"),
    Original_Value = c("80", "P1DT12H"),
    Rule_ID = c("SD1301", "SD1301"),
    Message = rep("TDSTOFF variable value must be either 0, ISO 8601 format for duration (e.g., P80Y), or null.", 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_violation,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1301: accepts valid values '0' and ISO durations (not reported)", {
  # '0' (row 1), 'P80Y' (row 4), 'P1Y2M' (row 10), 'P6M' (row 16) are valid
  res <- check_sd1301(
    df = td_violation,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  
  expect_false("1" %in% res$Row)
  expect_false("4" %in% res$Row)
  expect_false("10" %in% res$Row)
  expect_false("16" %in% res$Row)
})

test_that("SD1301: accepts negative ISO durations and zero variants", {
  td_neg <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TD",
    TDSEQ = 1:4,
    TDSTOFF = c("-P6M", "-0", "00", "P2W"),
    rownumber_new = 1:4,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_neg,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  expect_null(res)
})

test_that("SD1301: rejects plain numbers other than 0", {
  td_num <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TD",
    TDSEQ = 1:3,
    TDSTOFF = c("1", "12", "0"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_num,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, c("1", "2"))
  expect_equal(res$Original_Value, c("1", "12"))
})

test_that("SD1301: rejects incomplete ISO durations (e.g., 'P' alone, missing unit)", {
  td_bad_iso <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TD",
    TDSEQ = 1:3,
    TDSTOFF = c("P", "P1", "1Y"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_bad_iso,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Original_Value, c("P", "P1", "1Y"))
})

test_that("SD1301: trims whitespace before validation", {
  td_trim <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TD",
    TDSEQ = 1:3,
    TDSTOFF = c(" 0 ", " P80Y ", "P80"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_trim,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "3")
})

test_that("SD1301: ignores missing and blank TDSTOFF values", {
  td_blank <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TD",
    TDSEQ = 1:3,
    TDSTOFF = c(NA, "", "0"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1301(
    df = td_blank,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  expect_null(res)
})

test_that("SD1301: returns NULL when no violation", {
  res <- check_sd1301(
    df = td_ok,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd1301(
    df = td_missing,
    domain_name = "TD",
    target_vars = "TDSTOFF",
    rule_id = "SD1301"
  )
  expect_null(res)
})