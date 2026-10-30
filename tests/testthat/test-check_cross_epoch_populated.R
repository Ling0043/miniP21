# ---- Datasets ----
# DM reference dataset with RFICDTC (S03 partial, S04 empty)
dm_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  RFICDTC = c("2020-12-15", "2021-01-20", "2021-02", ""),
  stringsAsFactors = FALSE
)
# SD1339: AE domain with missing EPOCH where AESTDTC complete and on/after consent
ae_epoch <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER"),
  AESTDTC = c("2021-01-10", "2020-12-01", "2021-02-01"),
  EPOCH = c("", "", ""),
  stringsAsFactors = FALSE
)
# SD1339: compliant AE domain (EPOCH populated when required)
ae_epoch_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER"),
  AESTDTC = c("2021-01-10", "2020-12-01", "2021-02-01"),
  EPOCH = c("TREATMENT", "SCREENING", "SCREENING"),
  stringsAsFactors = FALSE
)
# SD1339: AE domain with multiple violations
ae_epoch_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S02", "S01"),
  AETERM = c("HEADACHE", "FEVER", "NAUSEA"),
  AESTDTC = c("2021-01-10", "2021-03-01", "2021-01-15"),
  EPOCH = c("", "", ""),
  stringsAsFactors = FALSE
)
# SD1477: LB domain with missing EPOCH where LBDTC complete and on/after consent
lb_epoch <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:4),
  LBSEQ = 1:4,
  USUBJID = c("S01", "S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB", "CREAT"),
  LBDTC = c("2021-01-10", "2020-12-01", "2021-02-01", "2021-03-01"),
  EPOCH = c("", "", "", ""),
  stringsAsFactors = FALSE
)
# SD1477: compliant LB domain (EPOCH populated when required)
lb_epoch_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  USUBJID = c("S01", "S01"),
  LBTESTCD = c("GLUC", "ALB"),
  LBDTC = c("2021-01-10", "2020-12-01"),
  EPOCH = c("TREATMENT", "SCREENING"),
  stringsAsFactors = FALSE
)
# SD1477: LB domain with LBTESTCD condition filter
lb_epoch_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S01", "S02"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-01-12", "2021-02-01"),
  EPOCH = c("", "", ""),
  stringsAsFactors = FALSE
)
# Boundary: main domain without USUBJID
ae_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:2),
  AESEQ = 1:2,
  AETERM = c("HEADACHE", "NAUSEA"),
  AESTDTC = c("2021-01-10", "2021-01-15"),
  EPOCH = c("", ""),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1339: EPOCH should be populated when --STDTC complete and not before RFICDTC
test_that("SD1339: catches missing EPOCH when AESTDTC complete and on/after RFICDTC", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = "EPOCH",
    Original_Value = "",
    Rule_ID = "SD1339",
    Message = "EPOCH should be populated when AESTDTC is collected as a complete date and is not before DM(RFICDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = ae_epoch,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1339",
    dtc_var = "--STDTC",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1339: returns NULL when EPOCH populated or date before consent", {
  res <- check_cross_epoch_populated(
    main_df = ae_epoch_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1339",
    dtc_var = "--STDTC",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1339: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2", "3"),
    Variable = c("EPOCH", "EPOCH", "EPOCH"),
    Original_Value = c("", "", ""),
    Rule_ID = c("SD1339", "SD1339", "SD1339"),
    Message = c(
      "EPOCH should be populated when AESTDTC is collected as a complete date and is not before DM(RFICDTC).",
      "EPOCH should be populated when AESTDTC is collected as a complete date and is not before DM(RFICDTC).",
      "EPOCH should be populated when AESTDTC is collected as a complete date and is not before DM(RFICDTC)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = ae_epoch_multi,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1339",
    dtc_var = "--STDTC",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1339: returns NULL when RFICDTC is partial even if EPOCH missing", {
  ae_s03 <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "AE",
    rownumber_new = as.character(1:1),
    AESEQ = 1L,
    USUBJID = "S03",
    AETERM = "HEADACHE",
    AESTDTC = "2021-03-01",
    EPOCH = "",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = ae_s03,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1339",
    dtc_var = "--STDTC",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1339: returns NULL when AESTDTC is partial even if EPOCH missing", {
  ae_partial <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "AE",
    rownumber_new = as.character(1:1),
    AESEQ = 1L,
    USUBJID = "S01",
    AETERM = "HEADACHE",
    AESTDTC = "2021-01",
    EPOCH = "",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = ae_partial,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1339",
    dtc_var = "--STDTC",
    complete_len = 10
  )
  expect_null(res)
})
# SD1477: EPOCH should be populated when --DTC complete and not before RFICDTC
test_that("SD1477: catches missing EPOCH when LBDTC complete and on/after RFICDTC", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("EPOCH", "EPOCH"),
    Original_Value = c("", ""),
    Rule_ID = c("SD1477", "SD1477"),
    Message = c(
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC).",
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_epoch,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1477: returns NULL when all required EPOCH are populated", {
  res <- check_cross_epoch_populated(
    main_df = lb_epoch_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1477: only checks rows satisfying equal condition", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "EPOCH",
    Original_Value = "",
    Rule_ID = "SD1477",
    Message = "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_epoch_cond,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10,
    cond_vars = "LBTESTCD",
    cond_ops = "equal",
    cond_vals = "GLUC",
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
test_that("SD1477: applies OR logic across conditions", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("EPOCH", "EPOCH"),
    Original_Value = c("", ""),
    Rule_ID = c("SD1477", "SD1477"),
    Message = c(
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC).",
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_epoch_cond,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10,
    cond_vars = "LBTESTCD",
    cond_ops = c("equal", "equal"),
    cond_vals = c("GLUC", "HGB"),
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
test_that("SD1477: treats NA EPOCH as empty and reports it", {
  lb_na <- lb_epoch
  lb_na$EPOCH <- c(NA, "", "", NA)
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("EPOCH", "EPOCH"),
    Original_Value = c("", ""),
    Rule_ID = c("SD1477", "SD1477"),
    Message = c(
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC).",
      "EPOCH should be populated when LBDTC is collected as a complete date and is not before DM(RFICDTC)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_na,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_epoch_populated(
    main_df = lb_epoch[0, ],
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1477",
    Message = "Reference domain DM is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_epoch,
    ref_dfs = list(XX = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
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
    Rule_ID = "SD1477",
    Message = "Reference domain DM is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_epoch,
    ref_dfs = list(DM = dm_empty),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
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
    Rule_ID = "SD1477",
    Message = "USUBJID not found in LB or DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = ae_noUSUBJID,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: subject without matching DM record is not checked
test_that("Boundary: returns NULL when subject has no RFICDTC match", {
  lb_nomatch <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    rownumber_new = as.character(1:1),
    LBSEQ = 1L,
    USUBJID = "S99",
    LBDTC = "2021-03-01",
    EPOCH = "",
    stringsAsFactors = FALSE
  )
  res <- check_cross_epoch_populated(
    main_df = lb_nomatch,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10
  )
  expect_null(res)
})
# Boundary: no row satisfies the conditions
test_that("Boundary: returns NULL when no row satisfies the conditions", {
  res <- check_cross_epoch_populated(
    main_df = lb_epoch_cond,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "EPOCH",
    ref_vars = "RFICDTC",
    rule_id = "SD1477",
    dtc_var = "--DTC",
    complete_len = 10,
    cond_vars = "LBTESTCD",
    cond_ops = "equal",
    cond_vals = "NOTEXIST",
    logic_op = "AND"
  )
  expect_null(res)
})
