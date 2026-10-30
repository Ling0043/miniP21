# ---- Datasets ----
# SD1053: TA domain with unexpected ARMCD values (multiple violations)
ta_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  TASEQ = 1:5,
  ARMCD = c("SCRNFAIL", "PLACEBO", "NOTASSGN", "ACTTREAT", "SCRNFAIL"),
  ARM = c("Screen Failure", "Placebo", "Not Assigned", "Active Treatment", "Screen Failure"),
  rownumber_new = c(1, 5, 9, 12, 15),
  stringsAsFactors = FALSE
)

# SD1053: TV domain with no violations
tv_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TV",
  TVSEQ = 1:3,
  ARMCD = c("PLACEBO", "ACTTREAT", "SCREEEN"),
  VISIT = c("SCREENING", "BASELINE", "WEEK 4"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variable ARMCD
ta_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  TASEQ = 1:2,
  ARM = c("Placebo", "Active"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1053: Unexpected value for ARMCD variable
test_that("SD1053: catches TA records with ARMCD 'SCRNFAIL' or 'NOTASSGN' (one row per error)", {
  expected_error <- data.frame(
    row_number = c("1", "9", "15"),
    variable_name = c("ARMCD", "ARMCD", "ARMCD"),
    original_value = c("SCRNFAIL", "NOTASSGN", "SCRNFAIL"),
    rule_id = c("SD1053", "SD1053", "SD1053"),
    error_message = c(
      "TA records with ARMCD='SCRNFAIL' should not be included in the dataset.",
      "TA records with ARMCD='NOTASSGN' should not be included in the dataset.",
      "TA records with ARMCD='SCRNFAIL' should not be included in the dataset."
    ),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1053(
    df = ta_violation,
    domain_name = "TA",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # One row per error
  expect_equal(res$row_number, expected_error$row_number)
  expect_equal(res$variable_name, expected_error$variable_name)
  expect_equal(res$original_value, expected_error$original_value)
  expect_equal(res$rule_id, expected_error$rule_id)
  expect_equal(res$error_message, expected_error$error_message)
})

test_that("SD1053: works for TV domain", {
  tv_violation <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TV",
    TVSEQ = 1:2,
    ARMCD = c("NOTASSGN", "PLACEBO"),
    VISIT = c("SCREENING", "BASELINE"),
    rownumber_new = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  expected_error <- data.frame(
    row_number = "1",
    variable_name = "ARMCD",
    original_value = "NOTASSGN",
    rule_id = "SD1053",
    error_message = "TV records with ARMCD='NOTASSGN' should not be included in the dataset.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1053(
    df = tv_violation,
    domain_name = "TV",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$row_number, expected_error$row_number)
  expect_equal(res$variable_name, expected_error$variable_name)
  expect_equal(res$original_value, expected_error$original_value)
  expect_equal(res$rule_id, expected_error$rule_id)
  expect_equal(res$error_message, expected_error$error_message)
})

test_that("SD1053: supports custom excl_vals", {
  res <- check_sd1053(
    df = ta_violation,
    domain_name = "TA",
    target_vars = "ARMCD",
    rule_id = "SD1053",
    excl_vals = "PLACEBO"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$row_number, "5")
  expect_equal(res$original_value, "PLACEBO")
})

test_that("SD1053: trims whitespace before matching", {
  ta_trim <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TA",
    TASEQ = 1:2,
    ARMCD = c(" SCRNFAIL ", "SCRNFAIL"),
    rownumber_new = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1053(
    df = ta_trim,
    domain_name = "TA",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$row_number, c("1", "2"))
})

test_that("SD1053: ignores missing and blank ARMCD values", {
  ta_blank <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TA",
    TASEQ = 1:3,
    ARMCD = c(NA, "", "SCRNFAIL"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1053(
    df = ta_blank,
    domain_name = "TA",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$row_number, "3")
})

test_that("SD1053: returns NULL when no violation", {
  res <- check_sd1053(
    df = tv_ok,
    domain_name = "TV",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  expect_null(res)
})

# Boundary: Missing target variable in df
test_that("Boundary: returns NULL when target variable is missing in df", {
  res <- check_sd1053(
    df = ta_missing,
    domain_name = "TA",
    target_vars = "ARMCD",
    rule_id = "SD1053"
  )
  expect_null(res)
})
