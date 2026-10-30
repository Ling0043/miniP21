# ---- Datasets ----
# SD1264: RELREC domain for IDVAR check
relrec <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELID = c("REL1", "REL2", "REL3", "REL4"),
  USUBJID = c(NA, "SUB01", NA, NA),
  IDVAR = c("AESEQ", "AESEQ", "AEGRPID", "LBSEQ"),
  IDVARVAL = c(NA, "1", NA, ""),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1264: RELREC domain with no violations
relrec_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELID = c("REL1", "REL2"),
  USUBJID = c(NA, "SUB01"),
  IDVAR = c("AEGRPID", "AESEQ"),
  IDVARVAL = c(NA, "1"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: Missing required columns (USUBJID, IDVARVAL, IDVAR)
relrec_missing_cols <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELREC",
  RELID = "REL1",
  rownumber_new = 1,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1264: IDVAR equals --SEQ when IDVARVAL is null and USUBJID is null
test_that("SD1264: catches IDVAR equals --SEQ when USUBJID and IDVARVAL are null", {
  expected_error <- data.frame(
    Row = c("1", "4"),
    Variable = "IDVAR",
    Original_Value = c("AESEQ", "LBSEQ"),
    Rule_ID = "SD1264",
    Message = "IDVAR cannot be --SEQ when USUBJID and IDVARVAL are null",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1264(
    df = relrec,
    rule_id = "SD1264"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # One row per violation
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1264: returns NULL when no violation", {
  res <- check_sd1264(
    df = relrec_ok,
    rule_id = "SD1264"
  )
  expect_null(res)
})

# Boundary: Missing required columns in df
test_that("Boundary: returns NULL when required columns are missing in df", {
  res <- check_sd1264(
    df = relrec_missing_cols,
    rule_id = "SD1264"
  )
  expect_null(res)
})
