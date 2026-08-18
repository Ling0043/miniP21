# ---- Datasets ----
# SD1221: TS domain - TSVAL must be numeric when TSPARMCD='PLANSUB'
ts_sd1221 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("PLANSUB", "PLANSUB", "TITLE"),
  TSVAL = c("100", "Not Numeric", "Study Title"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD2249: TS domain - TSVAL must be numeric when TSPARMCD='ACTSUB'
ts_sd2249 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("ACTSUB", "ACTSUB", "TITLE"),
  TSVAL = c("50", "NonNumeric", "Study Title"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1470: LB domain - --ORRES must be numeric (allow prefix) when --ORNRLO is populated
lb_sd1470 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  LBORNRLO = c("10", "10", "", NA_character_),
  LBORRES = c("20", "High", "<= 15", "30"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1473: LB domain - --ORRES must be numeric (allow prefix) when --STNRHI is populated
lb_sd1473 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  LBSTNRHI = c("50", "50", "", "60"),
  LBORRES = c("45", "Abnormal", "> 55", ""),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: missing required columns
df_missing_cols <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("PLANSUB", "TITLE"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: condition parameter value has no matching records
ts_cond_mismatch <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("TITLE", "DESIGN"),
  TSVAL = c("Study Title", "Parallel Group"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: condition variable all empty/NA (no check triggered)
lb_cond_all_empty <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBORNRLO = c("", NA_character_),
  LBORRES = c("High", "Low"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: condition met but all target values are blank/NA
lb_target_all_blank <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBORNRLO = c("10", "20"),
  LBORRES = c("", NA_character_),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: various valid prefix numeric formats
lb_valid_prefixes <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:5,
  LBORNRLO = rep("10", 5),
  LBORRES = c("< 5", ">= -1.2", "= 100", "  3.14", "> -0.5"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# ---- Tests ----
test_that("SD1221: catches non-numeric TSVAL when TSPARMCD='PLANSUB'", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "TSVAL",
    Original_Value = "",
    Rule_ID = "SD1221",
    Message = "Value for TSVAL is not numeric when TSPARMCD equals 'PLANSUB'.",
    stringsAsFactors = FALSE
  )
  res <- check_cond_numeric(
    df = ts_sd1221,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD1221",
    cond_var = "TSPARMCD",
    cond_val = "PLANSUB"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1221: returns NULL when TSVAL is numeric under matching condition", {
  ts_ok <- ts_sd1221
  ts_ok$TSVAL[2] <- "200"
  res <- check_cond_numeric(
    df = ts_ok,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD1221",
    cond_var = "TSPARMCD",
    cond_val = "PLANSUB"
  )
  expect_null(res)
})

test_that("SD2249: catches non-numeric TSVAL when TSPARMCD='ACTSUB'", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "TSVAL",
    Original_Value = "",
    Rule_ID = "SD2249",
    Message = "Value for TSVAL is not numeric when TSPARMCD equals 'ACTSUB'.",
    stringsAsFactors = FALSE
  )
  res <- check_cond_numeric(
    df = ts_sd2249,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD2249",
    cond_var = "TSPARMCD",
    cond_val = "ACTSUB"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2249: returns NULL when TSVAL is numeric under matching condition", {
  ts_ok <- ts_sd2249
  ts_ok$TSVAL[2] <- "75"
  res <- check_cond_numeric(
    df = ts_ok,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD2249",
    cond_var = "TSPARMCD",
    cond_val = "ACTSUB"
  )
  expect_null(res)
})

test_that("SD1470: catches non-numeric --ORRES when --ORNRLO is populated (allow prefix)", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBORRES",
    Original_Value = "",
    Rule_ID = "SD1470",
    Message = "Value for LBORRES is not numeric when LBORNRLO is populated.",
    stringsAsFactors = FALSE
  )
  res <- check_cond_numeric(
    df = lb_sd1470,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1470",
    cond_var = "--ORNRLO",
    allow_prefix = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1470: returns NULL when --ORRES has valid numeric values with prefixes", {
  lb_ok <- lb_sd1470
  lb_ok$LBORRES[2] <- "< 20"
  res <- check_cond_numeric(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1470",
    cond_var = "--ORNRLO",
    allow_prefix = TRUE
  )
  expect_null(res)
})

test_that("SD1473: catches non-numeric --ORRES when --STNRHI is populated (allow prefix)", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBORRES",
    Original_Value = "",
    Rule_ID = "SD1473",
    Message = "Value for LBORRES is not numeric when LBSTNRHI is populated.",
    stringsAsFactors = FALSE
  )
  res <- check_cond_numeric(
    df = lb_sd1473,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1473",
    cond_var = "--STNRHI",
    allow_prefix = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1473: returns NULL when --ORRES has valid numeric values with prefixes", {
  lb_ok <- lb_sd1473
  lb_ok$LBORRES[2] <- ">= 40"
  res <- check_cond_numeric(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1473",
    cond_var = "--STNRHI",
    allow_prefix = TRUE
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when required columns are missing from dataset", {
  res <- check_cond_numeric(
    df = df_missing_cols,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD1221",
    cond_var = "TSPARMCD",
    cond_val = "PLANSUB"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when cond_val does not match any records", {
  res <- check_cond_numeric(
    df = ts_cond_mismatch,
    domain_name = "TS",
    target_vars = "TSVAL",
    rule_id = "SD1221",
    cond_var = "TSPARMCD",
    cond_val = "PLANSUB"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when condition variable is all empty/NA", {
  res <- check_cond_numeric(
    df = lb_cond_all_empty,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1470",
    cond_var = "--ORNRLO",
    allow_prefix = TRUE
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when condition met but all target values are blank", {
  res <- check_cond_numeric(
    df = lb_target_all_blank,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1470",
    cond_var = "--ORNRLO",
    allow_prefix = TRUE
  )
  expect_null(res)
})

test_that("Boundary: all valid prefix numeric formats pass validation", {
  res <- check_cond_numeric(
    df = lb_valid_prefixes,
    domain_name = "LB",
    target_vars = "--ORRES",
    rule_id = "SD1470",
    cond_var = "--ORNRLO",
    allow_prefix = TRUE
  )
  expect_null(res)
})