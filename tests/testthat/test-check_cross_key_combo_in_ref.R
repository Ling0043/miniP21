# ---- Datasets ----
# SV reference dataset
sv_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  rownumber_new = as.character(1:3),
  SVSEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  VISIT = c("SCREENING", "TREATMENT", "SCREENING"),
  VISITNUM = c("1", "2", "1"),
  stringsAsFactors = FALSE
)
# SD0065: LB domain (row 2 key not in SV, row 3 exempt via LBSTAT populated)
lb_keys <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:4),
  LBSEQ = 1:4,
  USUBJID = c("S01", "S01", "S02", "S01"),
  VISIT = c("SCREENING", "WRONGVISIT", "SCREENING", "TREATMENT"),
  VISITNUM = c("1", "1", "1", "2"),
  LBSTAT = c("", "", "NOT DONE", ""),
  stringsAsFactors = FALSE
)
# SD0065: compliant LB domain (all keys found in SV)
lb_keys_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  VISIT = c("SCREENING", "TREATMENT", "SCREENING"),
  VISITNUM = c("1", "2", "1"),
  stringsAsFactors = FALSE
)
# SD0065: LB domain with multiple violations
lb_keys_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  VISIT = c("WRONGVISIT", "BADVISIT", "OTHERVISIT"),
  VISITNUM = c("1", "2", "1"),
  stringsAsFactors = FALSE
)
# SD0065: LB domain with lowercase VISIT values (case-insensitive match)
lb_keys_lower <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  USUBJID = c("s01", "S01"),
  VISIT = c("screening", "treatment"),
  VISITNUM = c("1", "2"),
  stringsAsFactors = FALSE
)
# SD0065: LB domain with blank VISIT component (row skipped)
lb_keys_blank <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  USUBJID = c("S01", "S01"),
  VISIT = c("", "SCREENING"),
  VISITNUM = c("1", "1"),
  stringsAsFactors = FALSE
)
# SD0065: EC domain with ECOCCUR exemption (row 2 exempt, row 3 error)
ec_keys <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EC",
  rownumber_new = as.character(1:3),
  ECSEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  ECTERM = c("ASPIRIN", "IBUPROFEN", "PARACETAMOL"),
  VISIT = c("SCREENING", "BADVISIT", "BADVISIT"),
  VISITNUM = c("1", "1", "1"),
  ECOCCUR = c("N", "N", "Y"),
  stringsAsFactors = FALSE
)
# Boundary: LB domain without rownumber_new column
lb_no_rownr <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  USUBJID = c("S01", "S01"),
  VISIT = c("WRONGVISIT", "SCREENING"),
  VISITNUM = c("1", "1"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD0065: USUBJID/VISIT/VISITNUM combinations must exist in SV,
# except records with --STAT populated or ECOCCUR='N'
# Note: the JSON specifies "–STAT" (en dash); this is corrected to "--STAT"
# per the rule description and the function's "--" wildcard convention.
test_that("SD0065: catches LB key combination not found in SV", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID, VISIT, VISITNUM",
    Original_Value = "S01 | WRONGVISIT | 1",
    Rule_ID = "SD0065",
    Message = "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0065: returns NULL when all key combinations found in SV", {
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys_ok,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD0065: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2", "3"),
    Variable = c("USUBJID, VISIT, VISITNUM", "USUBJID, VISIT, VISITNUM", "USUBJID, VISIT, VISITNUM"),
    Original_Value = c("S01 | WRONGVISIT | 1", "S01 | BADVISIT | 2", "S02 | OTHERVISIT | 1"),
    Rule_ID = c("SD0065", "SD0065", "SD0065"),
    Message = c(
      "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
      "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
      "Combination of USUBJID + VISIT + VISITNUM not found in SV."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys_multi,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0065: exempts records with LBSTAT populated", {
  lb_exempt <- lb_keys
  lb_exempt$LBSTAT <- c("", "NOT DONE", "", "")
  res <- check_cross_key_combo_in_ref(
    main_df = lb_exempt,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD0065: exempts EC records with ECOCCUR='N' but catches ECOCCUR='Y'", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "USUBJID, VISIT, VISITNUM",
    Original_Value = "S02 | BADVISIT | 1",
    Rule_ID = "SD0065",
    Message = "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = ec_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "EC",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0065: matches keys case-insensitively", {
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys_lower,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD0065: skips rows with missing key components", {
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys_blank,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
test_that("SD0065: applies AND logic across exemption conditions", {
  # with AND logic, LBSTAT populated alone no longer exempts row 3
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID, VISIT, VISITNUM",
    Original_Value = "S01 | WRONGVISIT | 1",
    Rule_ID = "SD0065",
    Message = "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys[0, ],
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0065",
    Message = "Reference domain SV is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(XX = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
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
  sv_empty <- sv_ref[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0065",
    Message = "Reference domain SV is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_empty),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: key variable not found in main domain
test_that("Boundary: reports error when key variable not found in main df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0065",
    Message = "Key variable(s) NOTAVAR not found in LB.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "NOTAVAR"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: reference key variable not found in reference domain
test_that("Boundary: reports error when reference key variable not found in ref df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0065",
    Message = "Reference key variable(s) NOTAVAR not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "NOTAVAR"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: ref_vars defaults to target_vars when NULL
test_that("Boundary: ref_vars defaults to target_vars when NULL", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID, VISIT, VISITNUM",
    Original_Value = "S01 | WRONGVISIT | 1",
    Rule_ID = "SD0065",
    Message = "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: -- wildcard resolved with domain_name for exemption variables
test_that("Boundary: resolves -- prefix with domain_name for exemption variable", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "USUBJID, VISIT, VISITNUM",
    Original_Value = "S01 | WRONGVISIT | 1",
    Rule_ID = "SD0065",
    Message = "Combination of USUBJID + VISIT + VISITNUM not found in SV.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_keys,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = "--STAT",
    exc_cond_ops = "non_missing",
    exc_cond_vals = NA,
    exc_logic_op = "OR"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# Boundary: whitespace-trimmed key comparison
test_that("Boundary: trims whitespace before key comparison", {
  lb_ws <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    rownumber_new = as.character(1:1),
    LBSEQ = 1L,
    USUBJID = " S01 ",
    VISIT = " SCREENING ",
    VISITNUM = "1",
    stringsAsFactors = FALSE
  )
  res <- check_cross_key_combo_in_ref(
    main_df = lb_ws,
    ref_dfs = list(SV = sv_ref),
    domain_name = "LB",
    ref_domain = "SV",
    target_vars = c("USUBJID", "VISIT", "VISITNUM"),
    ref_vars = c("USUBJID", "VISIT", "VISITNUM"),
    rule_id = "SD0065",
    exc_cond_vars = c("--STAT", "ECOCCUR"),
    exc_cond_ops = c("non_missing", "equal"),
    exc_cond_vals = c(NA, "N"),
    exc_logic_op = "OR"
  )
  expect_null(res)
})
