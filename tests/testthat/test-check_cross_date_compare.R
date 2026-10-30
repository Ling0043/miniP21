# ---- Datasets ----
# DM reference dataset containing RFSTDTC/RFPENDTC/RFICDTC/RFXENDTC/DTHDTC
dm_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  RFSTDTC = c("2021-01-01", "2021-02-01", "2021-03-01"),
  RFPENDTC = c("2021-06-30", "2021-07-15", "2021-08-31"),
  RFICDTC = c("2020-12-15", "2021-01-20", "2021-02-10"),
  RFXENDTC = c("2021-06-15", "2021-07-01", "2021-08-01"),
  DTHDTC = c("2021-06-30", "2021-07-10", "2021-08-31"),
  stringsAsFactors = FALSE
)
# SD1144: MH domain with MHSTDTC
mh <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "MH",
  rownumber_new = as.character(1:3),
  MHSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  MHTERM = c("DIABETES", "HYPERTENSION", "HEADACHE"),
  MHSTDTC = c("2020-06-01", "2021-05-01", "2021-03-01"),
  stringsAsFactors = FALSE
)
# SD1144: dataset with partial dates (common precision comparison)
mh_partial <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "MH",
  rownumber_new = as.character(1:2),
  MHSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  MHSTDTC = c("2021-05", "2021-02"),
  stringsAsFactors = FALSE
)
# SD1144: dataset with multiple violations
mh_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "MH",
  rownumber_new = as.character(1:3),
  MHSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  MHSTDTC = c("2021-05-01", "2022-01-01", "2021-03-01"),
  stringsAsFactors = FALSE
)
# SD1144: dataset with contains condition
mh_contains <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "MH",
  rownumber_new = as.character(1:2),
  MHSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  MHTERM = c("DIABETES TYPE 2", "HEADACHE"),
  MHSTDTC = c("2021-05-01", "2022-01-01"),
  stringsAsFactors = FALSE
)
# SD1202: LB domain with LBSTDTC vs RFPENDTC
lb_std <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBSTDTC = c("2021-06-30", "2021-07-20", "2021-05-01"),
  stringsAsFactors = FALSE
)
# SD1202: dataset for OR logic conditions
lb_or <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBSTDTC = c("2021-07-20", "2021-08-01", "2021-08-01"),
  stringsAsFactors = FALSE
)
# SD1203: LB domain with LBDTC vs RFPENDTC
lb_dtc <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBDTC = c("2021-06-01", "2021-08-01", ""),
  stringsAsFactors = FALSE
)
# SD1206: EX domain with EXSTDTC vs RFXENDTC
ex <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  EXSTDTC = c("2021-06-01", "2021-07-10", "2021-06-20"),
  stringsAsFactors = FALSE
)
# SD1317: DS domain with DEATH records
ds_death <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSDECOD = c("DEATH", "DEATH", "COMPLETED"),
  DSSTDTC = c("2021-06-30", "2021-07-11", "2021-09-01"),
  stringsAsFactors = FALSE
)
# SD1317: dataset where no row satisfies the condition
ds_no_death <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  DSDECOD = c("COMPLETED", "ADVERSE EVENT"),
  DSSTDTC = c("2021-06-30", "2021-07-11"),
  stringsAsFactors = FALSE
)
# SD1319: DS domain with DSSTDTC before RFICDTC
ds_consent <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:3),
  DSSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  DSDECOD = c("COMPLETED", "DISCRETIONARY", "ADVERSE EVENT"),
  DSSTDTC = c("2021-01-05", "2021-01-10", "2021-02-10"),
  stringsAsFactors = FALSE
)
# Boundary: main domain without USUBJID
mh_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "MH",
  rownumber_new = as.character(1:2),
  MHSEQ = 1:2,
  MHSTDTC = c("2021-05-01", "2021-01-01"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1144: MHSTDTC must be on or before RFSTDTC
test_that("SD1144: catches MHSTDTC after RFSTDTC", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "MHSTDTC",
    Original_Value = "2021-05-01",
    Rule_ID = "SD1144",
    Message = "MHSTDTC (2021-05-01) must be on or before DM(RFSTDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1144: returns NULL when all MHSTDTC on or before RFSTDTC", {
  mh_ok <- mh
  mh_ok$MHSTDTC[2] <- "2021-01-15"
  res <- check_cross_date_compare(
    main_df = mh_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_null(res)
})
test_that("SD1144: compares partial dates at common precision", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "MHSTDTC",
    Original_Value = "2021-05",
    Rule_ID = "SD1144",
    Message = "MHSTDTC (2021-05) must be on or before DM(RFSTDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh_partial,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1144: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("MHSTDTC", "MHSTDTC"),
    Original_Value = c("2021-05-01", "2022-01-01"),
    Rule_ID = c("SD1144", "SD1144"),
    Message = c(
      "MHSTDTC (2021-05-01) must be on or before DM(RFSTDTC).",
      "MHSTDTC (2022-01-01) must be on or before DM(RFSTDTC)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh_multi,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1144: only checks rows satisfying contains condition", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "MHSTDTC",
    Original_Value = "2021-05-01",
    Rule_ID = "SD1144",
    Message = "MHSTDTC (2021-05-01) must be on or before DM(RFSTDTC) (only checked when MHTERM is contains).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh_contains,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal",
    cond_vars = "MHTERM",
    cond_ops = "contains",
    cond_vals = "DIABETES"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1202: --STDTC must be on or before RFPENDTC
test_that("SD1202: catches LBSTDTC after RFPENDTC via --STDTC wildcard", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBSTDTC",
    Original_Value = "2021-07-20",
    Rule_ID = "SD1202",
    Message = "LBSTDTC (2021-07-20) must be on or before DM(RFPENDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = lb_std,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--STDTC",
    ref_vars = "RFPENDTC",
    rule_id = "SD1202",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1202: returns NULL when all --STDTC on or before RFPENDTC", {
  lb_ok <- lb_std
  lb_ok$LBSTDTC[2] <- "2021-07-15"
  res <- check_cross_date_compare(
    main_df = lb_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--STDTC",
    ref_vars = "RFPENDTC",
    rule_id = "SD1202",
    compare_op = "less_equal"
  )
  expect_null(res)
})
test_that("SD1202: applies OR logic across conditions", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("LBSTDTC", "LBSTDTC"),
    Original_Value = c("2021-07-20", "2021-08-01"),
    Rule_ID = c("SD1202", "SD1202"),
    Message = c(
      "LBSTDTC (2021-07-20) must be on or before DM(RFPENDTC) (only checked when LBTESTCD is equal and LBTESTCD is equal).",
      "LBSTDTC (2021-08-01) must be on or before DM(RFPENDTC) (only checked when LBTESTCD is equal and LBTESTCD is equal)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = lb_or,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--STDTC",
    ref_vars = "RFPENDTC",
    rule_id = "SD1202",
    compare_op = "less_equal",
    cond_vars = "LBTESTCD",
    cond_ops = c("equal", "equal"),
    cond_vals = c("GLUC", "ALB"),
    logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1203: --DTC must be on or before RFPENDTC
test_that("SD1203: catches LBDTC after RFPENDTC and ignores empty LBDTC", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBDTC",
    Original_Value = "2021-08-01",
    Rule_ID = "SD1203",
    Message = "LBDTC (2021-08-01) must be on or before DM(RFPENDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = lb_dtc,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DTC",
    ref_vars = "RFPENDTC",
    rule_id = "SD1203",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1203: returns NULL when all --DTC on or before RFPENDTC", {
  lb_ok <- lb_dtc
  lb_ok$LBDTC[2] <- "2021-07-01"
  res <- check_cross_date_compare(
    main_df = lb_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DTC",
    ref_vars = "RFPENDTC",
    rule_id = "SD1203",
    compare_op = "less_equal"
  )
  expect_null(res)
})
# SD1206: EXSTDTC must be on or before RFXENDTC
test_that("SD1206: catches EXSTDTC after RFXENDTC", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "EXSTDTC",
    Original_Value = "2021-07-10",
    Rule_ID = "SD1206",
    Message = "EXSTDTC (2021-07-10) must be on or before DM(RFXENDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = ex,
    ref_dfs = list(DM = dm_ref),
    domain_name = "EX",
    ref_domain = "DM",
    target_vars = "EXSTDTC",
    ref_vars = "RFXENDTC",
    rule_id = "SD1206",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1206: returns NULL when all EXSTDTC on or before RFXENDTC", {
  ex_ok <- ex
  ex_ok$EXSTDTC[2] <- "2021-07-01"
  res <- check_cross_date_compare(
    main_df = ex_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "EX",
    ref_domain = "DM",
    target_vars = "EXSTDTC",
    ref_vars = "RFXENDTC",
    rule_id = "SD1206",
    compare_op = "less_equal"
  )
  expect_null(res)
})
# SD1317: DSSTDTC for DEATH records must be the same as DM.DTHDTC
test_that("SD1317: catches DSSTDTC not equal to DTHDTC for DEATH records", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "DSSTDTC",
    Original_Value = "2021-07-11",
    Rule_ID = "SD1317",
    Message = "DSSTDTC (2021-07-11) must be the same as DM(DTHDTC) (only checked when DSDECOD is equal).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = ds_death,
    ref_dfs = list(DM = dm_ref),
    domain_name = "DS",
    ref_domain = "DM",
    target_vars = "DSSTDTC",
    ref_vars = "DTHDTC",
    rule_id = "SD1317",
    compare_op = "equal",
    cond_vars = "DSDECOD",
    cond_ops = "equal",
    cond_vals = "DEATH"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1317: returns NULL when all DEATH records match DTHDTC", {
  ds_ok <- ds_death
  ds_ok$DSSTDTC[2] <- "2021-07-10"
  res <- check_cross_date_compare(
    main_df = ds_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "DS",
    ref_domain = "DM",
    target_vars = "DSSTDTC",
    ref_vars = "DTHDTC",
    rule_id = "SD1317",
    compare_op = "equal",
    cond_vars = "DSDECOD",
    cond_ops = "equal",
    cond_vals = "DEATH"
  )
  expect_null(res)
})
test_that("SD1317: returns NULL when no row satisfies the DEATH condition", {
  res <- check_cross_date_compare(
    main_df = ds_no_death,
    ref_dfs = list(DM = dm_ref),
    domain_name = "DS",
    ref_domain = "DM",
    target_vars = "DSSTDTC",
    ref_vars = "DTHDTC",
    rule_id = "SD1317",
    compare_op = "equal",
    cond_vars = "DSDECOD",
    cond_ops = "equal",
    cond_vals = "DEATH"
  )
  expect_null(res)
})
# SD1319: --STDTC must be on or after RFICDTC
test_that("SD1319: catches DSSTDTC before RFICDTC", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "DSSTDTC",
    Original_Value = "2021-01-10",
    Rule_ID = "SD1319",
    Message = "DSSTDTC (2021-01-10) must be on or after DM(RFICDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = ds_consent,
    ref_dfs = list(DM = dm_ref),
    domain_name = "DS",
    ref_domain = "DM",
    target_vars = "--STDTC",
    ref_vars = "RFICDTC",
    rule_id = "SD1319",
    compare_op = "greater_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1319: returns NULL when all --STDTC on or after RFICDTC", {
  ds_ok <- ds_consent
  ds_ok$DSSTDTC[2] <- "2021-01-20"
  res <- check_cross_date_compare(
    main_df = ds_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "DS",
    ref_domain = "DM",
    target_vars = "--STDTC",
    ref_vars = "RFICDTC",
    rule_id = "SD1319",
    compare_op = "greater_equal"
  )
  expect_null(res)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_date_compare(
    main_df = mh[0, ],
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1144",
    Message = "Reference domain XX is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "XX",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
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
  dm_empty <- dm_ref[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1144",
    Message = "Reference domain DM is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh,
    ref_dfs = list(DM = dm_empty),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
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
    Rule_ID = "SD1144",
    Message = "USUBJID not found in MH or DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_date_compare(
    main_df = mh_noUSUBJID,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: non-numeric date strings do not produce errors
test_that("Boundary: returns NULL when date strings contain no digits (NA comparison)", {
  mh_bad <- mh
  mh_bad$MHSTDTC <- c("UNKNOWN", "2021-02-01", "2021-03-01")
  mh_bad$USUBJID <- c("S01", "S02", "S03")
  res <- check_cross_date_compare(
    main_df = mh_bad,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_null(res)
})
# Boundary: NA dates in target are not reported
test_that("Boundary: returns NULL when target dates are NA", {
  mh_na <- mh
  mh_na$MHSTDTC <- c(NA, NA, NA)
  res <- check_cross_date_compare(
    main_df = mh_na,
    ref_dfs = list(DM = dm_ref),
    domain_name = "MH",
    ref_domain = "DM",
    target_vars = "MHSTDTC",
    ref_vars = "RFSTDTC",
    rule_id = "SD1144",
    compare_op = "less_equal"
  )
  expect_null(res)
})
