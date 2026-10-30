# ---- Datasets ----
# SD1029: LB domain (FINDINGS) with non-ASCII / non-printable characters
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:5,
  LBTESTCD = c("HGB", "ALT\u00e9", "GLUC", "CRE\tAT", "WBC"),
  LBTEST = c("Hemoglobin", "Alanine Amino\u00fctransferase", "Glucose", "Creatinine", "Leukocytes"),
  rownumber_new = c(1, 4, 7, 10, 13),
  stringsAsFactors = FALSE
)

# SD1029: SUPPQUAL domain with non-ASCII characters in QNAM/QLABEL
sq_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPQUAL",
  RDOMAIN = "LB",
  IDVARVAL = c("1", "2", "3"),
  QNAM = c("LBTEST1", "LB\u00e9VAL", "A\u00e0B"),
  QLABEL = c("Lab Test 1", "Lab Value", "Lab\u00a0Test 3"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1029: LB domain with all valid printable ASCII values
lb_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBTESTCD = c("HGB", "ALT", "GLUC"),
  LBTEST = c("Hemoglobin", "Alanine Aminotransferase", "Glucose"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: None of the target variables exist in df
dm_irrelevant <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1029: Non-ASCII or non-printable characters in variable
test_that("SD1029: catches non-ASCII and non-printable characters (one row per error)", {
  expected_error <- data.frame(
    Row = c("4", "10", "4"),
    Variable = c("LBTESTCD", "LBTESTCD", "LBTEST"),
    Original_Value = c("ALT\u00e9", "CRE\tAT", "Alanine Amino\u00fctransferase"),
    Rule_ID = rep("SD1029", 3),
    Message = c(
      "LBTESTCD must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range).",
      "LBTESTCD must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range).",
      "LBTEST must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range)."
    ),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1029(
    df = lb_violation,
    domain_name = "LB",
    target_vars = c("--TESTCD", "--TEST"),
    rule_id = "SD1029"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1029: accepts printable ASCII values (not reported)", {
  # Row 1 'HGB', row 7 'GLUC', row 13 'WBC', row 1 'Hemoglobin' etc. are valid
  res <- check_sd1029(
    df = lb_violation,
    domain_name = "LB",
    target_vars = c("--TEST", "--TESTCD"),
    rule_id = "SD1029"
  )
  
  expect_false("1" %in% res$Row)
  expect_false("7" %in% res$Row)
  expect_false("13" %in% res$Row)
})

test_that("SD1029: checks QNAM and QLABEL for SUPPQUAL domain", {
  expected_error <- data.frame(
    Row = c("2", "3", "3"),
    Variable = c("QNAM", "QNAM", "QLABEL"),
    Original_Value = c("LB\u00e9VAL", "A\u00e0B", "Lab\u00a0Test 3"),
    Rule_ID = rep("SD1029", 3),
    Message = c(
      "QNAM must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range).",
      "QNAM must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range).",
      "QLABEL must not include non-ASCII or non-printable characters (outside of 32-126 ASCII code range)."
    ),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1029(
    df = sq_violation,
    domain_name = "SUPPQUAL",
    target_vars = c("--TESTCD", "--TEST", "--PARM", "--PARMCD",  "QNAM", "QLABEL"),
    rule_id = "SD1029"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1029: skips target variables that do not exist in df", {
  # QPARM/QPARMCD don't exist; only LBTESTCD/LBTEST checked
  res <- check_sd1029(
    df = lb_violation,
    domain_name = "LB",
    target_vars = c("--TEST", "--TESTCD", "--PARM", "--PARMCD"),
    rule_id = "SD1029"
  )
  
  expect_s3_class(res, "data.frame")
  expect_true(all(res$Variable %in% c("LBTESTCD", "LBTEST")))
})

test_that("SD1029: ignores missing values in target variables", {
  lb_na <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    LBSEQ = 1:3,
    LBTESTCD = c("HGB", NA, "ALT"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1029(
    df = lb_na,
    domain_name = "LB",
    target_vars = "--TESTCD",
    rule_id = "SD1029"
  )
  expect_null(res)
})

test_that("SD1029: returns NULL when no violation", {
  res <- check_sd1029(
    df = lb_ok,
    domain_name = "LB",
    target_vars = c("--TEST", "--TESTCD"),
    rule_id = "SD1029"
  )
  expect_null(res)
})

test_that("SD1029: catches tab and newline as non-printable characters", {
  lb_ctrl <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    LBSEQ = 1:3,
    LBTESTCD = c("HGB", "AL\nT", "GL\tUC"),
    rownumber_new = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1029(
    df = lb_ctrl,
    domain_name = "LB",
    target_vars = "--TESTCD",
    rule_id = "SD1029"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, c("2", "3"))
  expect_equal(res$Original_Value, c("AL\nT", "GL\tUC"))
})

# Boundary: None of the target variables exist in df
test_that("Boundary: returns NULL when none of the target variables exist in df", {
  res <- check_sd1029(
    df = dm_irrelevant,
    domain_name = "DM",
    target_vars = c("--TEST", "--TESTCD", "--PARM", "--PARMCD", "QLABEL", "QNAM"),
    rule_id = "SD1029"
  )
  expect_null(res)
})
