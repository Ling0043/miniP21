# ---- Datasets ----
# SD1026: RELREC domain, RELTYPE must be null when IDVAR ends with SEQ
relrec_sd1026 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:4,
  IDVAR = c("LBSEQ", "AESQ", "DMSEQ", "USUBJID"),
  RELTYPE = c("PARENT", "", "CHILD", "ASSOCIATED"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1026 compliant dataset: all IDVAR SEQ records have RELTYPE blank
relrec_sd1026_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:3,
  IDVAR = c("LBSEQ", "AESEQ", "USUBJID"),
  RELTYPE = c("", "", "ASSOCIATED"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: multiple violation rows
relrec_multi_err <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:3,
  IDVAR = c("LBSEQ", "AESEQ", "USUBJID"),
  RELTYPE = c("PARENT", "SIBLING", "ASSOCIATED"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: IDVAR all empty / NA (no condition trigger)
relrec_idvar_all_na <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:2,
  IDVAR = c(NA_character_, ""),
  RELTYPE = c("PARENT", "CHILD"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: missing target variable RELTYPE
df_missing_target <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:2,
  IDVAR = c("LBSEQ", "DMSEQ"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: missing condition variable IDVAR
df_missing_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:2,
  RELTYPE = c("PARENT", "CHILD"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: case insensitive SEQ ending test
relrec_case_seq <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELRECSEQ = 1:2,
  IDVAR = c("lbseq", "USUBJID"),
  RELTYPE = c("PARENT", "ASSOCIATED"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
test_that("SD1026: catches RELTYPE populated when IDVAR is *SEQ value, one error per row", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = "RELTYPE",
    Original_Value = c("PARENT", "CHILD"),
    Rule_ID = "SD1026",
    Message = c("RELTYPE must be null when IDVAR is populated with a --SEQ value ('LBSEQ').",
                "RELTYPE must be null when IDVAR is populated with a --SEQ value ('DMSEQ')."),
    stringsAsFactors = FALSE
  )
  res <- check_sd1026(
    df = relrec_sd1026,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1026: returns NULL when RELTYPE is null for IDVAR SEQ records", {
  res <- check_sd1026(
    df = relrec_sd1026_ok,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_null(res)
})

test_that("SD1026: reports multiple violation rows correctly", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("RELTYPE", "RELTYPE"),
    Original_Value = c("PARENT", "SIBLING"),
    Rule_ID = c("SD1026", "SD1026"),
    Message = c("RELTYPE must be null when IDVAR is populated with a --SEQ value ('LBSEQ').",
                 "RELTYPE must be null when IDVAR is populated with a --SEQ value ('AESEQ')."),
    stringsAsFactors = FALSE
  )
  res <- check_sd1026(
    df = relrec_multi_err,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1026: case‑insensitive match for SEQ suffix", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RELTYPE",
    Original_Value = "PARENT",
    Rule_ID = "SD1026",
    Message = "RELTYPE must be null when IDVAR is populated with a --SEQ value ('lbseq').",
    stringsAsFactors = FALSE
  )
  res <- check_sd1026(
    df = relrec_case_seq,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("Boundary: returns NULL when IDVAR all NA/blank, no trigger condition", {
  res <- check_sd1026(
    df = relrec_idvar_all_na,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when target variable RELTYPE missing", {
  res <- check_sd1026(
    df = df_missing_target,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when condition variable IDVAR missing", {
  res <- check_sd1026(
    df = df_missing_cond,
    domain_name = "RELREC",
    target_vars = "RELTYPE",
    cond_vars = "IDVAR",
    rule_id = "SD1026"
  )
  expect_null(res)
})