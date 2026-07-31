# ---- Datasets ----
# SD1043: FINDINGS domain - inconsistent --TESTCD within --TEST
lb_sd1043 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  LBTEST = c("Alanine Aminotransferase", "Alanine Aminotransferase", "Aspartate Aminotransferase", "Aspartate Aminotransferase"),
  LBTESTCD = c("ALT", "ALANINE", "AST", "AST"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD0086: SUPPQUAL domain - inconsistent QVAL within parent key groups (multi-group test)
suppdm_sd0086 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPDM",
  SUPPDMSEQ = 1:4,
  USUBJID = c("SUBJ001", "SUBJ001", "SUBJ001", "SUBJ002"),
  IDVAR = c("DMSEQ", "DMSEQ", "DMSEQ", "DMSEQ"),
  IDVARVAL = c("1", "1", "2", "1"),
  QNAM = c("RACECOMM", "RACECOMM", "ETHNICCOMM", "RACECOMM"),
  QVAL = c("Asian", "Chinese", "Not Hispanic", "Asian"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1236: Device domain - inconsistent --LOC within SPDEVID and USUBJID (-- wildcard test)
de_sd1236 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DE",
  DESEQ = 1:4,
  SPDEVID = c("DEV001", "DEV001", "DEV002", "DEV002"),
  USUBJID = c("SUBJ001", "SUBJ001", "SUBJ002", "SUBJ002"),
  DELOC = c("Left Arm", "Right Arm", "Left Arm", "Left Arm"),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: dataset missing target or group columns
df_missing_cols <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBORRES = c("20", "30"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: all target values are missing or blank
df_all_missing_target <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBTEST = c("ALT", "AST"),
  LBTESTCD = c(NA_character_, ""),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: each group has only one valid record (no inconsistency possible)
df_single_per_group <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBTEST = c("ALT", "AST"),
  LBTESTCD = c("ALT", "AST"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
test_that("SD1043: catches inconsistent --TESTCD values within --TEST", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "LBTESTCD",
    Original_Value = "ALT | ALANINE",
    Rule_ID = "SD1043",
    Message = "Value is not consistent within LBTEST.",
    stringsAsFactors = FALSE
  )
  res <- check_consistent(
    df = lb_sd1043,
    domain_name = "LB",
    target_vars = "--TESTCD",
    group_vars = "--TEST",
    rule_id = "SD1043"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1043: returns NULL when --TESTCD is consistent within --TEST", {
  lb_ok <- lb_sd1043
  lb_ok$LBTESTCD[2] <- "ALT"
  res <- check_consistent(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--TESTCD",
    group_vars = "--TEST",
    rule_id = "SD1043"
  )
  expect_null(res)
})

test_that("SD0086: catches inconsistent values within multi-column parent groups", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "QVAL",
    Original_Value = "Asian | Chinese",
    Rule_ID = "SD0086",
    Message = "Value is not consistent within STUDYID, USUBJID, IDVAR, IDVARVAL, QNAM.",
    stringsAsFactors = FALSE
  )
  res <- check_consistent(
    df = suppdm_sd0086,
    domain_name = "SUPPDM",
    target_vars = "QVAL",
    group_vars = c("STUDYID", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    rule_id = "SD0086"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0086: returns NULL when target is consistent across all parent groups", {
  suppdm_ok <- suppdm_sd0086
  suppdm_ok$QVAL[2] <- "Asian"
  res <- check_consistent(
    df = suppdm_ok,
    domain_name = "SUPPDM",
    target_vars = "QVAL",
    group_vars = c("STUDYID", "USUBJID", "IDVAR", "IDVARVAL", "QNAM"),
    rule_id = "SD0086"
  )
  expect_null(res)
})

test_that("SD1236: catches inconsistent --prefixed variables within specified groups", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "DELOC",
    Original_Value = "Left Arm | Right Arm",
    Rule_ID = "SD1236",
    Message = "Value is not consistent within SPDEVID, USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_consistent(
    df = de_sd1236,
    domain_name = "DE",
    target_vars = "--LOC",
    group_vars = c("SPDEVID", "USUBJID"),
    rule_id = "SD1236"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1236: returns NULL when --prefixed target is consistent within groups", {
  de_ok <- de_sd1236
  de_ok$DELOC[2] <- "Left Arm"
  res <- check_consistent(
    df = de_ok,
    domain_name = "DE",
    target_vars = "--LOC",
    group_vars = c("SPDEVID", "USUBJID"),
    rule_id = "SD1236"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when target or group columns are missing from dataset", {
  res <- check_consistent(
    df = df_missing_cols,
    domain_name = "LB",
    target_vars = "--TESTCD",
    group_vars = "--TEST",
    rule_id = "SD1043"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when all target values are missing or blank", {
  res <- check_consistent(
    df = df_all_missing_target,
    domain_name = "LB",
    target_vars = "--TESTCD",
    group_vars = "--TEST",
    rule_id = "SD1043"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when each group contains only one valid record", {
  res <- check_consistent(
    df = df_single_per_group,
    domain_name = "LB",
    target_vars = "--TESTCD",
    group_vars = "--TEST",
    rule_id = "SD1043"
  )
  expect_null(res)
})