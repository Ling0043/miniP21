# ---- Datasets ----
# SD1316: SS source domain (S01 DEAD, S02 DEAD, S03 ALIVE)
ss_1316 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:3),
  SSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  SSSTRESC = c("DEAD", "DEAD", "ALIVE"),
  stringsAsFactors = FALSE
)
# SD1316: DS target domain (S02 has DEATH record, S03 has DEATH, S01 not present)
ds_1316 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S04", "S02"),
  DSDECOD = c("DEATH", "DEATH"),
  stringsAsFactors = FALSE
)
# SD1316: compliant DS target domain (S01 also has DEATH record)
ds_1316_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSDECOD = c("DEATH", "DEATH", "DEATH"),
  stringsAsFactors = FALSE
)
# SD1316: DS target where S01 has a record but wrong DSDECOD
ds_1316_wrong <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSDECOD = c("COMPLETED", "DEATH", "DEATH"),
  stringsAsFactors = FALSE
)
# SD1316: SS source with multiple DEAD records for the same subject
ss_1316_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:3),
  SSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  SSSTRESC = c("DEAD", "DEAD", "DEAD"),
  stringsAsFactors = FALSE
)
# SD1316: SS source with lowercase dead value (case-insensitive)
ss_1316_lower <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:2),
  SSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  SSSTRESC = c("dead", "ALIVE"),
  stringsAsFactors = FALSE
)
# SD1318: DS source domain (S01 PROTOCOL VIOLATION, S02 PROTOCOL DEVIATION, S03 COMPLETED)
ds_1318 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSDECOD = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION", "COMPLETED"),
  stringsAsFactors = FALSE
)
# SD1318: DV target domain (S01 has record, S02 not present)
dv_1318 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DV",
  rownumber_new = as.character(1:2),
  DVSEQ = 1:2,
  USUBJID = c("S04", "S03"),
  DVDECOD = c("DEVIATION", "OTHER"),
  stringsAsFactors = FALSE
)
# SD1318: compliant DV target domain (S02 also has a record)
dv_1318_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DV",
  rownumber_new = as.character(1:3),
  DVSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DVDECOD = c("DEVIATION", "DEVIATION", "OTHER"),
  stringsAsFactors = FALSE
)
# SD1318: DV target where S02 has a record but wrong DVDECOD (target condition not used, still OK)
dv_1318_any <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DV",
  rownumber_new = as.character(1:3),
  DVSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DVDECOD = c("DEVIATION", "WHATEVER", "OTHER"),
  stringsAsFactors = FALSE
)
# Boundary: SS domain without USUBJID
ss_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:2),
  SSSEQ = 1:2,
  SSSTRESC = c("DEAD", "ALIVE"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1316: subjects with SSSTRESC='DEAD' in SS should have DSDECOD='DEATH' in DS
test_that("SD1316: catches subject with DEAD record but no DEATH record in DS", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD1316",
    Message = "Subject has record(s) with SSSTRESC equal 'DEAD' in SS but no supporting record in DS with DSDECOD equal 'DEATH'.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1316: returns NULL when all DEAD subjects have DEATH record in DS", {
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(DS = ds_1316_ok),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD1316: reports subject with DS record but wrong DSDECOD", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD1316",
    Message = "Subject has record(s) with SSSTRESC equal 'DEAD' in SS but no supporting record in DS with DSDECOD equal 'DEATH'.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(DS = ds_1316_wrong),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1316: reports each violating subject once when subject has multiple DEAD records", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S01", "S03"),
    Rule_ID = c("SD1316", "SD1316"),
    Message = c(
      "Subject has record(s) with SSSTRESC equal 'DEAD' in SS but no supporting record in DS with DSDECOD equal 'DEATH'.",
      "Subject has record(s) with SSSTRESC equal 'DEAD' in SS but no supporting record in DS with DSDECOD equal 'DEATH'."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316_multi,
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1316: matches source condition case-insensitively", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD1316",
    Message = "Subject has record(s) with SSSTRESC equal 'DEAD' in SS but no supporting record in DS with DSDECOD equal 'DEATH'.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316_lower,
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})


test_that("SD1316: returns NULL when no subject meets the source condition", {
  ss_none <- ss_1316
  ss_none$SSSTRESC <- c("ALIVE", "ALIVE", "ALIVE")
  res <- check_cross_subject_records_required(
    main_df = ss_none,
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_null(res)
})
# SD1318: subjects with PROTOCOL VIOLATION/DEVIATION in DS should have DV records
test_that("SD1318: catches subject with PROTOCOL VIOLATION record but no DV record", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S01", "S02"),
    Rule_ID = c("SD1318", "SD1318"),
    Message = c(
      "Subject has record(s) with DSDECOD equal 'PROTOCOL VIOLATION' OR DSDECOD equal 'PROTOCOL DEVIATION' in DS but no supporting record in DV.",
      "Subject has record(s) with DSDECOD equal 'PROTOCOL VIOLATION' OR DSDECOD equal 'PROTOCOL DEVIATION' in DS but no supporting record in DV."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ds_1318,
    ref_dfs = list(DV = dv_1318),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1318: returns NULL when all flagged subjects have DV records", {
  res <- check_cross_subject_records_required(
    main_df = ds_1318,
    ref_dfs = list(DV = dv_1318_ok),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD1318: any DV record counts as supporting record regardless of DVDECOD", {
  res <- check_cross_subject_records_required(
    main_df = ds_1318,
    ref_dfs = list(DV = dv_1318_any),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD1318: only one error row reported per subject with multiple flagged records", {
  ds_multi <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "DS",
    rownumber_new = as.character(1:3),
    DSSEQ = 1:3,
    USUBJID = c("S01", "S01", "S02"),
    DSDECOD = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION", "PROTOCOL VIOLATION"),
    stringsAsFactors = FALSE
  )
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S01", "S02"),
    Rule_ID = c("SD1318", "SD1318"),
    Message = c(
      "Subject has record(s) with DSDECOD equal 'PROTOCOL VIOLATION' OR DSDECOD equal 'PROTOCOL DEVIATION' in DS but no supporting record in DV.",
      "Subject has record(s) with DSDECOD equal 'PROTOCOL VIOLATION' OR DSDECOD equal 'PROTOCOL DEVIATION' in DS but no supporting record in DV."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ds_multi,
    ref_dfs = list(DV = dv_1318),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1318: returns NULL when no subject meets the source conditions", {
  ds_none <- ds_1318
  ds_none$DSDECOD <- c("COMPLETED", "COMPLETED", "ADVERSE EVENT")
  res <- check_cross_subject_records_required(
    main_df = ds_none,
    ref_dfs = list(DV = dv_1318),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD1318: matches source conditions case-insensitively", {
  ds_lower <- ds_1318
  ds_lower$DSDECOD <- c("protocol violation", "COMPLETED", "COMPLETED")
  expected_error <- data.frame(
    Row = "1",
    Variable = "USUBJID",
    Original_Value = "S01",
    Rule_ID = "SD1318",
    Message = "Subject has record(s) with DSDECOD equal 'PROTOCOL VIOLATION' OR DSDECOD equal 'PROTOCOL DEVIATION' in DS but no supporting record in DV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ds_lower,
    ref_dfs = list(DV = dv_1318),
    domain_name = "DS",
    ref_domain = "DV",
    rule_id = "SD1318",
    cond_vars = c("DSDECOD", "DSDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("PROTOCOL VIOLATION", "PROTOCOL DEVIATION"),
    logic_op = "OR",
    target_cond_vars = NULL,
    target_cond_ops = NULL,
    target_cond_vals = NULL,
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_subject_records_required(
    main_df = ss_1316[0, ],
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_null(res)
})
# Boundary: target domain missing from ref_dfs list
test_that("Boundary: reports error when target domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1316",
    Message = "Target domain DS is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(XX = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: target domain empty
test_that("Boundary: reports error when target domain is empty", {
  ds_empty <- ds_1316[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1316",
    Message = "Target domain DS is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(DS = ds_empty),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: target domain lacks USUBJID
test_that("Boundary: reports error when target domain lacks USUBJID", {
  ds_noUSUBJID <- ds_1316
  ds_noUSUBJID$USUBJID <- NULL
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1316",
    Message = "Target domain DS is missing or empty or lacks USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_1316,
    ref_dfs = list(DS = ds_noUSUBJID),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
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
    Rule_ID = "SD1316",
    Message = "USUBJID not found in SS.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_records_required(
    main_df = ss_noUSUBJID,
    ref_dfs = list(DS = ds_1316),
    domain_name = "SS",
    ref_domain = "DS",
    rule_id = "SD1316",
    cond_vars = "SSSTRESC",
    cond_ops = "equal",
    cond_vals = "DEAD",
    logic_op = "OR",
    target_cond_vars = "DSDECOD",
    target_cond_ops = "equal",
    target_cond_vals = "DEATH",
    target_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})