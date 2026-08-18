# ---- Datasets ----
# SD0015: EX domain for --DUR
ex_dur <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:3,
  EXDUR = c("-1", "5", "0"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1247: EC domain for ECDOSE condition
ec_1247 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EC",
  ECSEQ = 1:3,
  ECOCCUR = c("Y", "Y", "N"),
  ECDOSTXT = c(NA, "two pills", NA),
  ECDOSE = c("0", "10", "0"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1248: EC domain for ECDOSE condition when ECOCCUR = 'N'
ec_1248 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EC",
  ECSEQ = 1:3,
  ECOCCUR = c("N", "N", "Y"),
  ECDOSE = c("-5", "10", "-5"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: EX domain missing target variable
ex_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:3,
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD0015: Negative value for --DUR
test_that("SD0015: catches negative value for EXDUR", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXDUR",
    Original_Value = "-1",
    Rule_ID = "SD0015",
    Message = "Value of EXDUR must be >= 0.",
    stringsAsFactors = FALSE
  )
  
  res <- check_numeric_range(
    df = ex_dur,
    domain_name = "EX",
    target_vars = "--DUR",
    rule_id = "SD0015",
    operator = ">=",
    threshold = 0,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0015: returns NULL when no violation", {
  ex_dur_ok <- ex_dur
  ex_dur_ok$EXDUR[1] <- "1"
  res <- check_numeric_range(
    df = ex_dur_ok,
    domain_name = "EX",
    target_vars = "--DUR",
    rule_id = "SD0015",
    operator = ">=",
    threshold = 0,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND"
  )
  expect_null(res)
})

# SD1247: ECDOSE is not greater than 0 when ECOCCUR does not equal 'N' and ECDOSTXT is null
test_that("SD1247: catches ECDOSE not greater than 0 under conditions", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "ECDOSE",
    Original_Value = "0",
    Rule_ID = "SD1247",
    Message = "Value of ECDOSE must be > 0 when ECOCCUR is not in ('N') and is not missing and ECDOSTXT is missing.",
    stringsAsFactors = FALSE
  )
  
  res <- check_numeric_range(
    df = ec_1247,
    domain_name = "EC",
    target_vars = "ECDOSE",
    rule_id = "SD1247",
    operator = ">",
    threshold = 0,
    cond_vars = c("ECOCCUR", "ECDOSTXT"),
    cond_ops = c("not_in", "missing"),
    cond_vals = c("N,__MISSING__", ""),
    logic_op = "AND"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1247: returns NULL when no violation", {
  ec_1247_ok <- ec_1247
  ec_1247_ok$ECDOSE[1] <- "5"
  res <- check_numeric_range(
    df = ec_1247_ok,
    domain_name = "EC",
    target_vars = "ECDOSE",
    rule_id = "SD1247",
    operator = ">",
    threshold = 0,
    cond_vars = c("ECOCCUR", "ECDOSTXT"),
    cond_ops = c("not_in", "missing"),
    cond_vals = c("N,__MISSING__", ""),
    logic_op = "AND"
  )
  expect_null(res)
})

# SD1248: ECDOSE is not null or greater than 0 when ECOCCUR = 'N'
test_that("SD1248: catches ECDOSE not greater than 0 when ECOCCUR equals 'N'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "ECDOSE",
    Original_Value = "-5",
    Rule_ID = "SD1248",
    Message = "Value of ECDOSE must be > 0 when ECOCCUR='N'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_numeric_range(
    df = ec_1248,
    domain_name = "EC",
    target_vars = "ECDOSE",
    rule_id = "SD1248",
    operator = ">",
    threshold = 0,
    cond_vars = "ECOCCUR",
    cond_ops = "equal",
    cond_vals = "N",
    logic_op = "AND"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1248: returns NULL when no violation", {
  ec_1248_ok <- ec_1248
  ec_1248_ok$ECDOSE[1] <- "10"
  res <- check_numeric_range(
    df = ec_1248_ok,
    domain_name = "EC",
    target_vars = "ECDOSE",
    rule_id = "SD1248",
    operator = ">",
    threshold = 0,
    cond_vars = "ECOCCUR",
    cond_ops = "equal",
    cond_vals = "N",
    logic_op = "AND"
  )
  expect_null(res)
})

# Boundary: Missing target_vars in df
test_that("Boundary: returns NULL when target_vars missing in df", {
  res <- check_numeric_range(
    df = ex_missing,
    domain_name = "EX",
    target_vars = "--DUR",
    rule_id = "SD_BOUND1",
    operator = ">=",
    threshold = 0,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND"
  )
  expect_null(res)
})