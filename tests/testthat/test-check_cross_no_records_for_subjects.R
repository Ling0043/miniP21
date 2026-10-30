# ---- Datasets ----
# SD0079: DM reference domain with ARMCD conditions
dm_0079 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  ARMCD = c("NOTASSGN", "SCRNFAIL", "ARM1", NA),
  stringsAsFactors = FALSE
)
# SD0079: EX domain (S01 error, S02 RUN-IN excluded, S03 not checked, S04 error)
ex_0079 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:4),
  EXSEQ = 1:4,
  USUBJID = c("S01", "S02", "S03", "S04"),
  EXDOSE = c("10", "5", "20", "15"),
  EPOCH = c("TREATMENT", "RUN-IN", "TREATMENT", "SCREENING"),
  stringsAsFactors = FALSE
)
# SD0079: compliant EX domain (S02 has only RUN-IN record, S04 no record)
ex_0079_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:2),
  EXSEQ = 1:2,
  USUBJID = c("S02", "S03"),
  EXDOSE = c("5", "20"),
  EPOCH = c("RUN-IN", "TREATMENT"),
  stringsAsFactors = FALSE
)
# SD1340: DM reference domain with ACTARMCD conditions
dm_1340 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  ACTARMCD = c("NOTTRT", NA, "ARM1"),
  stringsAsFactors = FALSE
)
# SD1340: EX domain (S01 error, S02 error, S03 not checked, S01 RUN-IN excluded)
ex_1340 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:4),
  EXSEQ = 1:4,
  USUBJID = c("S01", "S02", "S03", "S01"),
  EXDOSE = c("10", "5", "20", "30"),
  EPOCH = c("TREATMENT", "SCREENING", "TREATMENT", "RUN-IN"),
  stringsAsFactors = FALSE
)
# SD1340: EX domain with lowercase EPOCH run-in (case-insensitive exclusion)
ex_1340_lower <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:1),
  EXSEQ = 1L,
  USUBJID = "S01",
  EXDOSE = "10",
  EPOCH = "run-in",
  stringsAsFactors = FALSE
)
# SD1340: compliant EX domain (S02 no record, S03 only record)
ex_1340_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:1),
  EXSEQ = 1L,
  USUBJID = "S03",
  EXDOSE = "20",
  EPOCH = "TREATMENT",
  stringsAsFactors = FALSE
)
# Boundary: DM with lowercase ARMCD values (case-insensitive subject match)
dm_lower <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  ARMCD = c("notassgn", "ARM1"),
  stringsAsFactors = FALSE
)
# Boundary: DM where no subject meets the condition
dm_none <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  ARMCD = c("ARM1", "ARM2"),
  stringsAsFactors = FALSE
)
# Boundary: main domain without USUBJID
ex_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:2),
  EXSEQ = 1:2,
  EXDOSE = c("10", "20"),
  EPOCH = c("TREATMENT", "SCREENING"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD0079: EX record is present when subject is not assigned to an arm
test_that("SD0079: catches EX records for NOTASSGN/SCRNFAIL/missing ARMCD subjects", {
  expected_error <- data.frame(
    Row = c("1", "4"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S01", "S04"),
    Rule_ID = c("SD0079", "SD0079"),
    Message = c(
      "Record present for a subject with ARMCD equal 'NOTASSGN' OR ARMCD equal 'SCRNFAIL' OR ARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
      "Record present for a subject with ARMCD equal 'NOTASSGN' OR ARMCD equal 'SCRNFAIL' OR ARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0079: returns NULL when only RUN-IN records exist for flagged subjects", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079_ok,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
test_that("SD0079: fires for RUN-IN subject records when epoch exclusion disabled", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S02",
    Rule_ID = "SD0079",
    Message = "Record present for a subject with ARMCD equal 'NOTASSGN' OR ARMCD equal 'SCRNFAIL' OR ARMCD is missing '' in DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079_ok,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = NULL,
    epoch_exclude_vals = NULL
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1340: EX record is present when subject is not treated
test_that("SD1340: catches EX records for NOTTRT/missing ACTARMCD subjects", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S01", "S02"),
    Rule_ID = c("SD1340", "SD1340"),
    Message = c(
      "Record present for a subject with ACTARMCD equal 'NOTTRT' OR ACTARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
      "Record present for a subject with ACTARMCD equal 'NOTTRT' OR ACTARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_1340,
    ref_dfs = list(DM = dm_1340),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD1340",
    ref_cond_vars = c("ACTARMCD", "ACTARMCD"),
    ref_cond_ops = c("equal", "missing"),
    ref_cond_vals = c("NOTTRT", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1340: returns NULL when flagged subjects have no EX records", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_1340_ok,
    ref_dfs = list(DM = dm_1340),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD1340",
    ref_cond_vars = c("ACTARMCD", "ACTARMCD"),
    ref_cond_ops = c("equal", "missing"),
    ref_cond_vals = c("NOTTRT", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
test_that("SD1340: excludes lowercase run-in EPOCH records case-insensitively", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_1340_lower,
    ref_dfs = list(DM = dm_1340),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD1340",
    ref_cond_vars = c("ACTARMCD", "ACTARMCD"),
    ref_cond_ops = c("equal", "missing"),
    ref_cond_vals = c("NOTTRT", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
test_that("SD1340: matches subject conditions case-insensitively", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD1340",
    Message = "Record present for a subject with ACTARMCD equal 'NOTTRT' OR ACTARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
    stringsAsFactors = FALSE
  )
  dm_lower_act <- dm_1340
  dm_lower_act$ACTARMCD <- c("nottrt", "ARM1", "ARM1")
  res <- check_cross_no_records_for_subjects(
    main_df = ex_1340_ok,
    ref_dfs = list(DM = dm_lower_act),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD1340",
    ref_cond_vars = c("ACTARMCD", "ACTARMCD"),
    ref_cond_ops = c("equal", "missing"),
    ref_cond_vals = c("NOTTRT", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079[0, ],
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0079",
    Message = "Reference domain DM is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(XX = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: reference dataset empty
test_that("Boundary: reports error when reference dataset is empty", {
  dm_empty <- dm_0079[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0079",
    Message = "Reference domain DM is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_empty),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: reference dataset lacks USUBJID
test_that("Boundary: reports error when reference dataset lacks USUBJID", {
  dm_noUSUBJID <- dm_0079
  dm_noUSUBJID$USUBJID <- NULL
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0079",
    Message = "Reference domain DM is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_noUSUBJID),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: USUBJID not found in main domain
test_that("Boundary: reports error when USUBJID not found in main domain", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0079",
    Message = "USUBJID not found in EX.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_noUSUBJID,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: no subject meets the conditions
test_that("Boundary: returns NULL when no subject meets the conditions", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_none),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
# Boundary: no subject conditions provided (default FALSE, nothing flagged)
test_that("Boundary: returns NULL when no ref conditions are provided", {
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = NULL,
    ref_cond_ops = NULL,
    ref_cond_vals = NULL,
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
# Boundary: case-insensitive subject matching in DM
test_that("Boundary: matches lowercase ARMCD values case-insensitively", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD0079",
    Message = "Record present for a subject with ARMCD equal 'NOTASSGN' OR ARMCD equal 'SCRNFAIL' OR ARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_1340_ok,
    ref_dfs = list(DM = dm_lower),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
# Boundary: epoch_var not in main_df (record-level exclusion skipped)
test_that("Boundary: fires when epoch exclusion variable is absent from main df", {
  ex_no_epoch <- ex_0079
  ex_no_epoch$EPOCH <- NULL
  expected_error <- data.frame(
    Row = c("1", "2", "4"),
    Variable = "USUBJID",
    Original_Value = c("S01", "S02", "S04"),
    Rule_ID = "SD0079",
    Message = "Record present for a subject with ARMCD equal 'NOTASSGN' OR ARMCD equal 'SCRNFAIL' OR ARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_no_epoch,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "equal", "missing"),
    ref_cond_vals = c("NOTASSGN", "SCRNFAIL", NA),
    ref_logic_op = "OR",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})


# Boundary: AND logic combines subject conditions strictly
test_that("Boundary: applies AND logic across subject conditions", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD0079",
    Message = "Record present for a subject with ARMCD equal 'NOTASSGN' AND ARMCD is missing '' in DM (records with EPOCH='RUN-IN' are excluded).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_no_records_for_subjects(
    main_df = ex_0079,
    ref_dfs = list(DM = dm_0079),
    domain_name = "EX",
    ref_domain = "DM",
    rule_id = "SD0079",
    ref_cond_vars = c("ARMCD", "ARMCD"),
    ref_cond_ops = c("equal", "missing"),
    ref_cond_vals = c("NOTASSGN", NA),
    ref_logic_op = "AND",
    epoch_var = "EPOCH",
    epoch_exclude_vals = "RUN-IN"
  )
  expect_null(res)
})
