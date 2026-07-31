# ---- Datasets ----

# SD0005: DM domain with duplicate LBSEQ within USUBJID
dm_sd0005 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02"),
  LBSEQ = c(1, 1, 2),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1064: TE domain with duplicate ETCD
te_sd1064 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TE",
  ETCD = c("E1", "E1", "E2"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1216: TS domain with multiple AGEMAX records
ts_sd1216 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSPARMCD = c("AGEMAX", "AGEMAX", "OTHER"),
  TSVAL = c("90", "95", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1216 compliant dataset (only one AGEMAX)
ts_sd1216_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSPARMCD = c("AGEMAX", "OTHER", "OTHER2"),
  TSVAL = c("90", "N", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----

# SD0005: Duplicate SEQ within USUBJID
test_that("SD0005: catches duplicate LBSEQ within USUBJID", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "LBSEQ",
    Original_Value = "1",
    Rule_ID = "SD0005",
    Message = "Combination is duplicated 2 times within USUBJID.",
    stringsAsFactors = FALSE
  )

  res <- check_duplicate(
    df = dm_sd0005,
    domain_name = "DM",
    target_vars = "LBSEQ",
    rule_id = "SD0005",
    group_vars = "USUBJID"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0005: returns NULL when SEQ is unique within USUBJID", {
  dm_ok <- dm_sd0005
  dm_ok$LBSEQ[2] <- 3  # make unique

  res <- check_duplicate(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "LBSEQ",
    rule_id = "SD0005",
    group_vars = "USUBJID"
  )

  expect_null(res)
})

test_that("SD0005: returns NULL if required columns missing", {
  dm_missing <- dm_sd0005
  dm_missing$USUBJID <- NULL

  res <- check_duplicate(
    df = dm_missing,
    domain_name = "DM",
    target_vars = "LBSEQ",
    rule_id = "SD0005",
    group_vars = "USUBJID"
  )

  expect_null(res)
})

# SD1064: Duplicate ETCD in TE
test_that("SD1064: catches duplicate ETCD in TE domain", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "ETCD",
    Original_Value = "E1",
    Rule_ID = "SD1064",
    Message = "Combination is duplicated 2 times in dataset.",
    stringsAsFactors = FALSE
  )

  res <- check_duplicate(
    df = te_sd1064,
    domain_name = "TE",
    target_vars = "ETCD",
    rule_id = "SD1064",
    group_vars = NULL
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1064: returns NULL when ETCD is unique", {
  te_ok <- te_sd1064
  te_ok$ETCD[2] <- "E3"  # make unique

  res <- check_duplicate(
    df = te_ok,
    domain_name = "TE",
    target_vars = "ETCD",
    rule_id = "SD1064",
    group_vars = NULL
  )

  expect_null(res)
})

# SD1216: Only one AGEMAX record in TS
test_that("SD1216: catches duplicate AGEMAX records using filter_values", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "TSPARMCD",
    Original_Value = "AGEMAX",
    Rule_ID = "SD1216",
    Message = "Combination is duplicated 2 times in dataset.",
    stringsAsFactors = FALSE
  )

  res <- check_duplicate(
    df = ts_sd1216,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "SD1216",
    group_vars = NULL,
    filter_values = "AGEMAX"   # only check records where TSPARMCD == "AGEMAX"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1216: returns NULL when only one AGEMAX record exists", {
  res <- check_duplicate(
    df = ts_sd1216_ok,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "SD1216",
    group_vars = NULL,
    filter_values = "AGEMAX"
  )

  expect_null(res)
})

test_that("SD1216: returns NULL when filter_values is NULL (no filtering)", {
  # Without filter, there are duplicate TSPARMCD values? Actually ts_sd1216 has two AGEMAX and one OTHER, so duplicates exist.
  # But we want to test that without filter it will catch duplicates, but we can test that it still works.
  # For this test, we use a dataset with no duplicates overall.
  ts_no_dup <- ts_sd1216_ok
  res <- check_duplicate(
    df = ts_no_dup,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "SD1216",
    group_vars = NULL,
    filter_values = NULL
  )
  expect_null(res)
})

# Boundary: missing target column
test_that("SD1216: returns NULL if target_vars column missing", {
  ts_missing <- ts_sd1216
  ts_missing$TSPARMCD <- NULL

  res <- check_duplicate(
    df = ts_missing,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "SD1216",
    group_vars = NULL,
    filter_values = "AGEMAX"
  )

  expect_null(res)
})

# Boundary: all target values are missing/blank -> return NULL
test_that("check_duplicate ignores rows with missing/blank target values", {
  ts_with_na <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TS",
    TSPARMCD = c("AGEMAX", NA, ""),
    TSVAL = c("90", "95", "N"),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )

  res <- check_duplicate(
    df = ts_with_na,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "BOUNDARY",
    group_vars = NULL,
    filter_values = "AGEMAX"
  )

  expect_null(res)
})

# Boundary: filter_values filters correctly and only checks those records
test_that("filter_values restricts duplicate check to specified value only", {
  # Dataset with duplicate TSPARMCD='AGEMAX' but also other duplicates
  ts_mixed <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "TS",
    TSPARMCD = c("AGEMAX", "AGEMAX", "OTHER", "OTHER"),
    TSVAL = c("90", "95", "N", "N"),
    rownumber_new = 1:4,
    stringsAsFactors = FALSE
  )

  # With filter, only AGEMAX duplicates should be caught (2 records)
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "TSPARMCD",
    Original_Value = "AGEMAX",
    Rule_ID = "BOUNDARY_FILTER",
    Message = "Combination is duplicated 2 times in dataset.",
    stringsAsFactors = FALSE
  )

  res <- check_duplicate(
    df = ts_mixed,
    domain_name = "TS",
    target_vars = "TSPARMCD",
    rule_id = "BOUNDARY_FILTER",
    group_vars = NULL,
    filter_values = "AGEMAX"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})