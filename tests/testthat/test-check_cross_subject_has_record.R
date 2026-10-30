# ---- Datasets ----
# SD0070: DM domain with ARMCD / ACTARMCD (including missing ARMCD)
dm_ex <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:5),
  USUBJID = c("S01", "S02", "S03", "S04", "S05"),
  ARMCD = c("ARM1", "SCRNFAIL", "", "ARM2", "ARM3"),
  ACTARMCD = c("ARM1", "", "", "NOTTRT", "ARM3"),
  stringsAsFactors = FALSE
)
# SD0070: EX reference domain (S05 has EX record but EXDOSE is empty)
ex_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S05"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXDOSE = c("10", "5", ""),
  stringsAsFactors = FALSE
)
# SD0070: compliant EX reference domain (S05 EXDOSE populated)
ex_ref_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S05"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXDOSE = c("10", "5", "20"),
  stringsAsFactors = FALSE
)
# SD0070: EX reference for multiple AND ref-side conditions
ex_ref_and <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:2),
  EXSEQ = 1:2,
  USUBJID = c("S01", "S05"),
  EXTRT = c("DRUG", "PLACEBO"),
  EXDOSE = c("10", "15"),
  stringsAsFactors = FALSE
)
# SD1240: DM domain
dm_ds <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  ARMCD = c("ARM1", "ARM2", "ARM3"),
  stringsAsFactors = FALSE
)
# SD1240: DS reference domain (S02 wrong category, S03 no record)
ds_consent <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  DSCAT = c("INFORMED CONSENT", "DISPOSITION EVENT"),
  stringsAsFactors = FALSE
)
# SD1240: compliant DS reference domain
ds_consent_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSCAT = c("INFORMED CONSENT", "INFORMED CONSENT", "INFORMED CONSENT"),
  stringsAsFactors = FALSE
)
# SD1374: DM domain (S03 has null ACTARMCD, excluded)
dm_disp <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  ACTARMCD = c("ARM1", "ARM2", NA),
  stringsAsFactors = FALSE
)
# SD1374: DS reference domain (S02 has no record)
ds_disp <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:1),
  DSSEQ = 1L,
  USUBJID = c("S01"),
  DSCAT = c("DISPOSITION EVENT"),
  stringsAsFactors = FALSE
)
# SD1374: compliant DS reference domain
ds_disp_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  DSCAT = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
  stringsAsFactors = FALSE
)
# SD1377: DM domain with ARMNRS
dm_scrn <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  ARMNRS = c("SCREEN FAILURE", "SCREEN FAILURE", NA),
  stringsAsFactors = FALSE
)
# SD1377: IE reference domain (S02 has no record)
ie_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "IE",
  rownumber_new = as.character(1),
  IESEQ = 1,
  USUBJID = c("S01"),
  IECAT = c("INCLUSION"),
  stringsAsFactors = FALSE
)

# SD2288: DM domain with ARMCD exceptions
dm_dspe <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  ARMCD = c("ARM1", "ARM2", "SCRNFAIL", "NOTASSGN"),
  stringsAsFactors = FALSE
)
# SD2288: DS reference domain (S02 lacks DISPOSITION EVENT record)
ds_dspe <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSCAT = c("DISPOSITION EVENT", "COMPLETION", "DISPOSITION EVENT"),
  stringsAsFactors = FALSE
)
# SD2288: compliant DS reference domain
ds_dspe_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  DSCAT = c("DISPOSITION EVENT", "DISPOSITION EVENT"),
  stringsAsFactors = FALSE
)
# Boundary: DM domain with NA USUBJID
dm_nasubj <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1),
  USUBJID = c(NA),
  ARMNRS = c("SCREEN FAILURE"),
  stringsAsFactors = FALSE
)
# Boundary: DM domain where no row satisfies the conditions
dm_none <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  ARMCD = c("SCRNFAIL", "SCRNFAIL"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD0070: All DM subjects should have an EX record with non-missing EXDOSE, with exceptions
test_that("SD0070: catches subject with no EX record with non-missing EXDOSE", {
  expected_error <- data.frame(
    Row = "5",
    Variable = "USUBJID",
    Original_Value = "S05",
    Rule_ID = "SD0070",
    Message = "No record found in EX(USUBJID) (EXDOSE is non_missing) for subject S05",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_ex,
    ref_dfs = list(EX = ex_ref),
    domain_name = "DM",
    ref_domain = "EX",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD0070",
    cond_vars = c("ARMCD", "ARMCD", "ARMCD", "ACTARMCD"),
    cond_ops = c("non_missing", "not_equal", "not_equal", "not_equal"),
    cond_vals = c("", "SCRNFAIL", "NOTASSGN", "NOTTRT"),
    ref_cond_vars = "EXDOSE",
    ref_cond_ops = "non_missing",
    ref_cond_vals = "",
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
test_that("SD0070: returns NULL when all checked subjects have EX record with EXDOSE", {
  res <- check_cross_subject_has_record(
    main_df = dm_ex,
    ref_dfs = list(EX = ex_ref_ok),
    domain_name = "DM",
    ref_domain = "EX",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD0070",
    cond_vars = c("ARMCD", "ARMCD", "ARMCD", "ACTARMCD"),
    cond_ops = c("non_missing", "not_equal", "not_equal", "not_equal"),
    cond_vals = c("", "SCRNFAIL", "NOTASSGN", "NOTTRT"),
    ref_cond_vars = "EXDOSE",
    ref_cond_ops = "non_missing",
    ref_cond_vals = "",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD0070: combines multiple ref-side conditions with AND logic", {
  expected_error <- data.frame(
    Row = "5",
    Variable = "USUBJID",
    Original_Value = "S05",
    Rule_ID = "SD0070",
    Message = "No record found in EX(USUBJID) (EXDOSE is non_missing and EXTRT is equal) for subject S05",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_ex,
    ref_dfs = list(EX = ex_ref_and),
    domain_name = "DM",
    ref_domain = "EX",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD0070",
    cond_vars = c("ARMCD", "ARMCD", "ARMCD", "ACTARMCD"),
    cond_ops = c("non_missing", "not_equal", "not_equal", "not_equal"),
    cond_vals = c("", "SCRNFAIL", "NOTASSGN", "NOTTRT"),
    ref_cond_vars = c("EXDOSE", "EXTRT"),
    ref_cond_ops = c("non_missing", "equal"),
    ref_cond_vals = c("", "DRUG"),
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
# SD1240: All subjects should have an Informed Consent record in DS
test_that("SD1240: reports one row per subject without Informed Consent record", {
  expected_error <- data.frame(
    Row = c("2", "3"),
    Variable = c("USUBJID", "USUBJID"),
    Original_Value = c("S02", "S03"),
    Rule_ID = c("SD1240", "SD1240"),
    Message = c(
      "No record found in DS(USUBJID) (DSCAT is equal) for subject S02",
      "No record found in DS(USUBJID) (DSCAT is equal) for subject S03"
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_ds,
    ref_dfs = list(DS = ds_consent),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1240",
    ref_cond_vars = "DSCAT",
    ref_cond_ops = "equal",
    ref_cond_vals = "INFORMED CONSENT"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1240: returns NULL when all subjects have Informed Consent record", {
  res <- check_cross_subject_has_record(
    main_df = dm_ds,
    ref_dfs = list(DS = ds_consent_ok),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1240",
    ref_cond_vars = "DSCAT",
    ref_cond_ops = "equal",
    ref_cond_vals = "INFORMED CONSENT"
  )
  expect_null(res)
})
# SD1374: All DM subjects should have a DS record except null ACTARMCD
test_that("SD1374: catches subject without DS record and excludes null ACTARMCD", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID",
    Original_Value = "S02",
    Rule_ID = "SD1374",
    Message = "No record found in DS(USUBJID) for subject S02",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_disp,
    ref_dfs = list(DS = ds_disp),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    rule_id = "SD1374",
    cond_vars = "ACTARMCD",
    cond_ops = "non_missing",
    cond_vals = ""
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1374: returns NULL when all non-excluded subjects have DS record", {
  res <- check_cross_subject_has_record(
    main_df = dm_disp,
    ref_dfs = list(DS = ds_disp_ok),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    rule_id = "SD1374",
    cond_vars = "ACTARMCD",
    cond_ops = "non_missing",
    cond_vals = ""
  )
  expect_null(res)
})
# SD1377: Subjects with ARMNRS='SCREEN FAILURE' should have IE records
test_that("SD1377: catches SCREEN FAILURE subject without IE record", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID",
    Original_Value = "S02",
    Rule_ID = "SD1377",
    Message = "No record found in IE(USUBJID) for subject S02",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_scrn,
    ref_dfs = list(IE = ie_ref),
    domain_name = "DM",
    ref_domain = "IE",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1377",
    cond_vars = "ARMNRS",
    cond_ops = "equal",
    cond_vals = "SCREEN FAILURE"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1377: returns NULL when all SCREEN FAILURE subjects have IE record", {
  ie_ok <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "IE",
    rownumber_new = as.character(1:2),
    IESEQ = 1:2,
    USUBJID = c("S01", "S02"),
    IECAT = c("INCLUSION", "EXCLUSION"),
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_scrn,
    ref_dfs = list(IE = ie_ok),
    domain_name = "DM",
    ref_domain = "IE",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1377",
    cond_vars = "ARMNRS",
    cond_ops = "equal",
    cond_vals = "SCREEN FAILURE"
  )
  expect_null(res)
})

# SD2288: Subjects should have a DS DISPOSITION EVENT record, with ARMCD exceptions
test_that("SD2288: catches subject without DISPOSITION EVENT record", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID",
    Original_Value = "S02",
    Rule_ID = "SD2288",
    Message = "No record found in DS(USUBJID) (DSCAT is equal) for subject S02",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_dspe,
    ref_dfs = list(DS = ds_dspe),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD2288",
    cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    cond_ops = c("non_missing", "not_equal", "not_equal"),
    cond_vals = c("", "SCRNFAIL", "NOTASSGN"),
    ref_cond_vars = "DSCAT",
    ref_cond_ops = "equal",
    ref_cond_vals = "DISPOSITION EVENT",
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
test_that("SD2288: returns NULL when all subjects have DISPOSITION EVENT record", {
  res <- check_cross_subject_has_record(
    main_df = dm_dspe,
    ref_dfs = list(DS = ds_dspe_ok),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD2288",
    cond_vars = c("ARMCD", "ARMCD", "ARMCD"),
    cond_ops = c("non_missing", "not_equal", "not_equal"),
    cond_vals = c("", "SCRNFAIL", "NOTASSGN"),
    ref_cond_vars = "DSCAT",
    ref_cond_ops = "equal",
    ref_cond_vals = "DISPOSITION EVENT",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD2288: applies OR logic across main-side conditions", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID",
    Original_Value = "S02",
    Rule_ID = "SD2288",
    Message = "No record found in DS(USUBJID) (DSCAT is equal) for subject S02",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_dspe,
    ref_dfs = list(DS = ds_dspe),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD2288",
    cond_vars = "ARMCD",
    cond_ops = c("equal", "equal"),
    cond_vals = c("ARM1", "ARM2"),
    ref_cond_vars = "DSCAT",
    ref_cond_ops = "equal",
    ref_cond_vals = "DISPOSITION EVENT",
    logic_op = "OR"
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
  res <- check_cross_subject_has_record(
    main_df = dm_disp[0, ],
    ref_dfs = list(DS = ds_disp),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    rule_id = "SD1374",
    cond_vars = "ACTARMCD",
    cond_ops = "non_missing",
    cond_vals = ""
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1374",
    Message = "Reference domain XX is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_disp,
    ref_dfs = list(DS = ds_disp),
    domain_name = "DM",
    ref_domain = "XX",
    target_vars = "USUBJID",
    rule_id = "SD1374",
    cond_vars = "ACTARMCD",
    cond_ops = "non_missing",
    cond_vals = ""
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
  ds_empty <- ds_disp[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1374",
    Message = "Reference domain DS is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_subject_has_record(
    main_df = dm_disp,
    ref_dfs = list(DS = ds_empty),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "USUBJID",
    rule_id = "SD1374",
    cond_vars = "ACTARMCD",
    cond_ops = "non_missing",
    cond_vals = ""
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: no row satisfies the main-side conditions
test_that("Boundary: returns NULL when no row satisfies the conditions", {
  res <- check_cross_subject_has_record(
    main_df = dm_none,
    ref_dfs = list(EX = ex_ref),
    domain_name = "DM",
    ref_domain = "EX",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD0070",
    cond_vars = c("ARMCD", "ARMCD"),
    cond_ops = c("not_equal", "not_equal"),
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "AND"
  )
  expect_null(res)
})
# Boundary: subjects with NA USUBJID are not reported
test_that("Boundary: returns NULL when subject USUBJID is NA", {
  res <- check_cross_subject_has_record(
    main_df = dm_nasubj,
    ref_dfs = list(IE = ie_ref),
    domain_name = "DM",
    ref_domain = "IE",
    target_vars = "USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1377",
    cond_vars = "ARMNRS",
    cond_ops = "equal",
    cond_vals = "SCREEN FAILURE"
  )
  expect_null(res)
})
# Boundary: -- wildcard resolves against domain_name
test_that("Boundary: returns NULL when -- wildcard resolves to non-existent column", {
  res <- check_cross_subject_has_record(
    main_df = dm_scrn,
    ref_dfs = list(IE = ie_ref),
    domain_name = "DM",
    ref_domain = "IE",
    target_vars = "--USUBJID",
    ref_vars = "USUBJID",
    rule_id = "SD1377",
    cond_vars = "ARMNRS",
    cond_ops = "equal",
    cond_vals = "SCREEN FAILURE"
  )
  expect_null(res)
})
