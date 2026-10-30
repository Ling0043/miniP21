# ---- Datasets ----
# DM reference dataset with RFSTDTC (S02 missing) and RFENDTC (S03 missing)
dm_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  RFSTDTC = c("2021-01-01", "", "2021-03-01", "2021-04-01"),
  RFENDTC = c("2021-06-30", "2021-07-15", "", "2021-08-31"),
  stringsAsFactors = FALSE
)
# SD1030: LB domain with LBSTRF populated for subject with missing RFSTDTC
lb_strf <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:4),
  LBSEQ = 1:4,
  USUBJID = c("S01", "S02", "S03", "S99"),
  LBTESTCD = c("GLUC", "ALB", "HGB", "CREAT"),
  LBSTRF = c("BEFORE", "DURING", "AFTER", "DURING"),
  stringsAsFactors = FALSE
)
# SD1030: compliant LB domain (LBSTRF empty for subject with missing RFSTDTC)
lb_strf_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBSTRF = c("BEFORE", "", "AFTER"),
  stringsAsFactors = FALSE
)
# SD1030: LB domain with multiple violations for the same subject
lb_strf_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S02", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBSTRF = c("DURING", "AFTER", "DURING"),
  stringsAsFactors = FALSE
)
# SD1030: LB domain with NA LBSTRF values (not reported)
lb_strf_na <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  USUBJID = c("S02", "S03"),
  LBSTRF = c(NA, "DURING"),
  stringsAsFactors = FALSE
)
# SD1031: AE domain with AEENRF populated for subject with missing RFENDTC
ae_enrf <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S03", "S02"),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER"),
  AEENRF = c("AFTER", "BEFORE", "DURING"),
  stringsAsFactors = FALSE
)
# SD1031: compliant AE domain (AEENRF empty for subject with missing RFENDTC)
ae_enrf_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S03", "S02"),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER"),
  AEENRF = c("AFTER", "", "DURING"),
  stringsAsFactors = FALSE
)
# SD1031: AE domain with blank string AEENRF (not reported)
ae_enrf_blank <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:1),
  AESEQ = 1L,
  USUBJID = "S03",
  AETERM = "NAUSEA",
  AEENRF = "   ",
  stringsAsFactors = FALSE
)
# Boundary: LB domain without USUBJID
lb_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  LBSTRF = c("DURING", "BEFORE"),
  stringsAsFactors = FALSE
)
# Boundary: LB domain without rownumber_new column
lb_no_rownr <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  USUBJID = c("S02", "S03"),
  LBSTRF = c("DURING", "BEFORE"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1030: --STRF should not be populated when RFSTDTC is null
test_that("SD1030: catches LBSTRF populated when RFSTDTC is null", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBSTRF",
    Original_Value = "DURING",
    Rule_ID = "SD1030",
    Message = "LBSTRF should not be populated when RFSTDTC is null in DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1030: returns NULL when LBSTRF empty for subject with missing RFSTDTC", {
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_null(res)
})
test_that("SD1030: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("LBSTRF", "LBSTRF"),
    Original_Value = c("DURING", "AFTER"),
    Rule_ID = c("SD1030", "SD1030"),
    Message = c(
      "LBSTRF should not be populated when RFSTDTC is null in DM.",
      "LBSTRF should not be populated when RFSTDTC is null in DM."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf_multi,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1030: returns NULL when LBSTRF is NA for subject with missing RFSTDTC", {
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf_na,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_null(res)
})
test_that("SD1030: skips subjects not found in the reference domain", {
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref[1, , drop = FALSE]),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_null(res)
})
# SD1031: --ENRF should not be populated when RFENDTC is null
test_that("SD1031: catches AEENRF populated when RFENDTC is null", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "AEENRF",
    Original_Value = "BEFORE",
    Rule_ID = "SD1031",
    Message = "AEENRF should not be populated when RFENDTC is null in DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = ae_enrf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    target_vars = "--ENRF",
    rule_id = "SD1031",
    ref_domains = "DM",
    ref_vars = "RFENDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1031: returns NULL when AEENRF empty for subject with missing RFENDTC", {
  res <- check_cross_not_populated_when_ref_missing(
    main_df = ae_enrf_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    target_vars = "--ENRF",
    rule_id = "SD1031",
    ref_domains = "DM",
    ref_vars = "RFENDTC"
  )
  expect_null(res)
})
test_that("SD1031: returns NULL when AEENRF is blank string (whitespace only)", {
  res <- check_cross_not_populated_when_ref_missing(
    main_df = ae_enrf_blank,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    target_vars = "--ENRF",
    rule_id = "SD1031",
    ref_domains = "DM",
    ref_vars = "RFENDTC"
  )
  expect_null(res)
})
test_that("SD1031: only first reference domain is used", {
  dm_alt <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "DM",
    rownumber_new = as.character(1:2),
    USUBJID = c("S01", "S02"),
    RFENDTC = c("2021-06-30", ""),
    stringsAsFactors = FALSE
  )
  expected_error <- data.frame(
    Row = "3",
    Variable = "AEENRF",
    Original_Value = "DURING",
    Rule_ID = "SD1031",
    Message = "AEENRF should not be populated when RFENDTC is null in DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = ae_enrf,
    ref_dfs = list(DM = dm_alt),
    domain_name = "AE",
    target_vars = "--ENRF",
    rule_id = "SD1031",
    ref_domains = c("DM", "XX"),
    ref_vars = "RFENDTC"
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
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf[0, ],
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "Reference domain DM not found or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(XX = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
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
    Rule_ID = "SD1030",
    Message = "Reference domain DM not found or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_empty),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: required variables missing in current domain
test_that("Boundary: reports error when target variable missing in main df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "Required variable(s) not found in current domain: NOTAVAR.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "NOTAVAR",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("Boundary: reports error when USUBJID missing in main df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "Required variable(s) not found in current domain: USUBJID.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_noUSUBJID,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: required variables missing in reference domain
test_that("Boundary: reports error when ref variable missing in ref df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "Required variable(s) not found in DM: NOTEXIST.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "NOTEXIST"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: more than one target/ref variable
test_that("Boundary: reports error when target_vars has more than one variable", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "target_vars and ref_vars must each contain exactly one variable.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = c("LBSTRF", "LBENRF"),
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("Boundary: reports error when ref_vars has more than one variable", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1030",
    Message = "target_vars and ref_vars must each contain exactly one variable.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_strf,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    target_vars = "LBSTRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = c("RFSTDTC", "RFENDTC")
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: -- wildcard resolved with domain_name
test_that("Boundary: resolves -- prefix with domain_name", {
  ae_wildcard <- ae_enrf
  ae_wildcard$AEENRF <- c("AFTER", "BEFORE", "DURING")
  expected_error <- data.frame(
    Row = "2",
    Variable = "AEENRF",
    Original_Value = "BEFORE",
    Rule_ID = "SD1031",
    Message = "AEENRF should not be populated when RFENDTC is null in DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = ae_wildcard,
    ref_dfs = list(DM = dm_ref),
    domain_name = "AE",
    target_vars = "--ENRF",
    rule_id = "SD1031",
    ref_domains = "DM",
    ref_vars = "RFENDTC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: only first record per subject in reference domain is used
test_that("Boundary: uses only the first record per subject in the reference domain", {
  dm_dup <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "DM",
    rownumber_new = as.character(1:2),
    USUBJID = c("S01", "S01"),
    RFSTDTC = c("2021-01-01", ""),
    stringsAsFactors = FALSE
  )
  lb_one <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    rownumber_new = as.character(1:1),
    LBSEQ = 1L,
    USUBJID = "S01",
    LBSTRF = "DURING",
    stringsAsFactors = FALSE
  )
  res <- check_cross_not_populated_when_ref_missing(
    main_df = lb_one,
    ref_dfs = list(DM = dm_dup),
    domain_name = "LB",
    target_vars = "--STRF",
    rule_id = "SD1030",
    ref_domains = "DM",
    ref_vars = "RFSTDTC"
  )
  expect_null(res)
})
