# ---- Datasets ----
# SD1041: LB domain for --CAT and --SCAT comparison
lb <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  LBCAT = c("HEMATOLOGY", "CHEMISTRY", "HEMATOLOGY", "CHEMISTRY"),
  LBSCAT = c("HEMATOLOGY", "GLUCOSE", "HEMATOLOGY", NA),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1327 & SD1328: RELSUB domain for RSUBJID, POOLID, and USUBJID comparison
relsub <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELSUB",
  RELID = c("REL1", "REL2"),
  RSUBJID = c("SUB01", "SUB02"),
  POOLID = c("SUB01", "POOL1"),
  USUBJID = c("SUB01", "SUB01"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: Missing target variables
ts_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1041: Values for --CAT and --SCAT are identical
test_that("SD1041: catches identical values for LBCAT and LBSCAT", {
  expected_error <- data.frame(
    Row = "1, 3",
    Variable = "LBCAT, LBSCAT",
    Original_Value = "",
    Rule_ID = "SD1041",
    Message = "Values of LBCAT and LBSCAT are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = lb,
    domain_name = "LB",
    var1 = "--CAT",
    var2 = "--SCAT",
    relation = "not_equal",
    rule_id = "SD1041",
    check_na = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1041: returns NULL when no violation", {
  lb_ok <- lb
  lb_ok$LBSCAT[1] <- "CELL COUNT"
  lb_ok$LBSCAT[3] <- "BLOOD SMEAR"
  res <- check_compare_vars(
    df = lb_ok,
    domain_name = "LB",
    var1 = "--CAT",
    var2 = "--SCAT",
    relation = "not_equal",
    rule_id = "SD1041",
    check_na = FALSE
  )
  expect_null(res)
})

# SD1327: RSUBJID is equal to POOLID
test_that("SD1327: catches RSUBJID equal to POOLID", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RSUBJID, POOLID",
    Original_Value = "",
    Rule_ID = "SD1327",
    Message = "Values of RSUBJID and POOLID are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = relsub,
    domain_name = "RELSUB",
    var1 = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327",
    check_na = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1327: returns NULL when no violation", {
  relsub_ok <- relsub
  relsub_ok$POOLID[1] <- "POOL2"
  res <- check_compare_vars(
    df = relsub_ok,
    domain_name = "RELSUB",
    var1 = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327",
    check_na = FALSE
  )
  expect_null(res)
})

# SD1328: RSUBJID is equal to USUBJID
test_that("SD1328: catches RSUBJID equal to USUBJID", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RSUBJID, USUBJID",
    Original_Value = "",
    Rule_ID = "SD1328",
    Message = "Values of RSUBJID and USUBJID are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = relsub,
    domain_name = "RELSUB",
    var1 = "RSUBJID",
    var2 = "USUBJID",
    relation = "not_equal",
    rule_id = "SD1328",
    check_na = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1328: returns NULL when no violation", {
  relsub_ok <- relsub
  relsub_ok$USUBJID[1] <- "SUB99"
  res <- check_compare_vars(
    df = relsub_ok,
    domain_name = "RELSUB",
    var1 = "RSUBJID",
    var2 = "USUBJID",
    relation = "not_equal",
    rule_id = "SD1328",
    check_na = FALSE
  )
  expect_null(res)
})

# Boundary: Missing target variables in df
test_that("Boundary: returns NULL when target variables are missing in df", {
  res <- check_compare_vars(
    df = ts_missing,
    domain_name = "TS",
    var1 = "TSCAT",
    var2 = "TSSCAT",
    relation = "not_equal",
    rule_id = "SD_BOUND1",
    check_na = FALSE
  )
  expect_null(res)
})
