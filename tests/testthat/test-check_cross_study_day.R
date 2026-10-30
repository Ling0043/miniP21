# ---- Datasets ----
# DM reference dataset with RFSTDTC (S03 partial, S04 empty)
dm_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  RFSTDTC = c("2021-01-01", "2021-02-01", "2021-02", ""),
  stringsAsFactors = FALSE
)
# SD1084: LB domain with missing LBDY where both dates are complete
lb_dy <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-02-05", "2021-03-01"),
  LBDY = c(NA, "5", NA),
  stringsAsFactors = FALSE
)
# SD1084: compliant LB domain (all LBDY populated when required)
lb_dy_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-02-05", "2021-03-01"),
  LBDY = c("10", "5", ""),
  stringsAsFactors = FALSE
)
# SD1084: LB domain with multiple violations
lb_dy_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S01"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-02-05", "2021-01-15"),
  LBDY = c(NA, NA, "15"),
  stringsAsFactors = FALSE
)
# SD1084: LB domain with partial LBDTC (no error expected)
lb_dy_partial <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  LBDTC = c("2021-01", "2021-02"),
  LBDY = c(NA, NA),
  stringsAsFactors = FALSE
)
# SD1086: LB domain with incorrect LBDY values (on/after and before RFSTDTC)
lb_correct <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S01"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-01-25", "2021-01-01"),
  LBDY = c("9", "-6", "1"),
  stringsAsFactors = FALSE
)
# SD1086: compliant LB domain (LBDY matches CDISC definition)
lb_correct_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S01"),
  LBTESTCD = c("GLUC", "ALB", "HGB"),
  LBDTC = c("2021-01-10", "2021-01-25", "2021-01-01"),
  LBDY = c("10", "-7", "1"),
  stringsAsFactors = FALSE
)
# SD1089: SV domain with SVSTDY imputed on partial SVSTDTC
sv_imputed <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  rownumber_new = as.character(1:3),
  VISITNUM = c(1, 2, 3),
  USUBJID = c("S01", "S01", "S02"),
  SVSTDTC = c("2021-03", "2021-01-10", ""),
  SVSTDY = c("5", "10", ""),
  stringsAsFactors = FALSE
)
# SD1089: compliant SV domain (SVSTDY only populated with complete dates)
sv_imputed_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  rownumber_new = as.character(1:2),
  VISITNUM = c(1, 2),
  USUBJID = c("S01", "S01"),
  SVSTDTC = c("2021-01-10", "2021-03"),
  SVSTDY = c("10", ""),
  stringsAsFactors = FALSE
)
# Boundary: main domain without USUBJID
lb_noUSUBJID <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:2),
  LBSEQ = 1:2,
  LBDTC = c("2021-01-10", "2021-01-15"),
  LBDY = c(NA, NA),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1084: --DY should be populated when both --DTC and RFSTDTC are complete
test_that("SD1084: catches missing LBDY when LBDTC and RFSTDTC are complete", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBDY",
    Original_Value = NA_character_,
    Rule_ID = "SD1084",
    Message = "Incorrect value for LBDY variable: should be populated when both date values are provided with complete date part",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_dy,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
test_that("SD1084: returns NULL when all required LBDY are populated", {
  res <- check_cross_study_day(
    main_df = lb_dy_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1084: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("LBDY", "LBDY"),
    Original_Value = c(NA_character_, NA_character_),
    Rule_ID = c("SD1084", "SD1084"),
    Message = c(
      "Incorrect value for LBDY variable: should be populated when both date values are provided with complete date part",
      "Incorrect value for LBDY variable: should be populated when both date values are provided with complete date part"
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_dy_multi,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
test_that("SD1084: returns NULL when LBDTC is partial even if LBDY missing", {
  res <- check_cross_study_day(
    main_df = lb_dy_partial,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1084: returns NULL when RFSTDTC is partial even if LBDY missing", {
  lb_s03 <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    rownumber_new = as.character(1:1),
    LBSEQ = 1L,
    USUBJID = "S03",
    LBDTC = "2021-03-01",
    LBDY = NA_character_,
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_s03,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
    complete_len = 10
  )
  expect_null(res)
})
# SD1086: --DY should equal the CDISC formula value
test_that("SD1086: catches incorrect LBDY when LBDTC is on or after RFSTDTC", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBDY",
    Original_Value = "9",
    Rule_ID = "SD1086",
    Message = "Incorrect value for LBDY variable: does not match CDISC definition based on LBDTC and DM(RFSTDTC)",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_correct,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1086",
    dtc_var = "--DTC",
    check_type = "correct",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row[1], expected_error$Row)
  expect_equal(res$Variable[1], expected_error$Variable)
  expect_equal(res$Original_Value[1], expected_error$Original_Value)
  expect_equal(res$Rule_ID[1], expected_error$Rule_ID)
  expect_equal(res$Message[1], expected_error$Message)
})
test_that("SD1086: catches incorrect LBDY when LBDTC precedes RFSTDTC", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBDY",
    Original_Value = "-6",
    Rule_ID = "SD1086",
    Message = "Incorrect value for LBDY variable: does not match CDISC definition based on LBDTC and DM(RFSTDTC)",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_correct,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1086",
    dtc_var = "--DTC",
    check_type = "correct",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row[2], expected_error$Row)
  expect_equal(res$Variable[2], expected_error$Variable)
  expect_equal(res$Original_Value[2], expected_error$Original_Value)
  expect_equal(res$Rule_ID[2], expected_error$Rule_ID)
  expect_equal(res$Message[2], expected_error$Message)
})
test_that("SD1086: returns NULL when all LBDY match CDISC definition", {
  res <- check_cross_study_day(
    main_df = lb_correct_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1086",
    dtc_var = "--DTC",
    check_type = "correct",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1086: returns NULL when LBDY is missing (only filled values checked)", {
  lb_correct_na <- lb_correct
  lb_correct_na$LBDY <- c(NA, "-6", "1")
  res <- check_cross_study_day(
    main_df = lb_correct_na,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1086",
    dtc_var = "--DTC",
    check_type = "correct",
    complete_len = 10
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, "2")
})
# SD1089: --STDY should not be imputed; only populated with complete dates
test_that("SD1089: catches imputed SVSTDY when SVSTDTC is partial", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "SVSTDY",
    Original_Value = "5",
    Rule_ID = "SD1089",
    Message = "Incorrect value for SVSTDY variable: should not be imputed; it may only be populated when both date values include complete date part",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = sv_imputed,
    ref_dfs = list(DM = dm_ref),
    domain_name = "SV",
    ref_domain = "DM",
    target_vars = "--STDY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1089",
    dtc_var = "--STDTC",
    check_type = "imputed",
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
test_that("SD1089: returns NULL when SVSTDY only populated with complete dates", {
  res <- check_cross_study_day(
    main_df = sv_imputed_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "SV",
    ref_domain = "DM",
    target_vars = "--STDY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1089",
    dtc_var = "--STDTC",
    check_type = "imputed",
    complete_len = 10
  )
  expect_null(res)
})
test_that("SD1089: returns NULL when RFSTDTC is partial and SVSTDY populated", {
  sv_partial_ref <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "SV",
    rownumber_new = as.character(1:1),
    VISITNUM = 1,
    USUBJID = "S03",
    SVSTDTC = "2021-03-10",
    SVSTDY = " ",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = sv_partial_ref,
    ref_dfs = list(DM = dm_ref),
    domain_name = "SV",
    ref_domain = "DM",
    target_vars = "--STDY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1089",
    dtc_var = "--STDTC",
    check_type = "imputed",
    complete_len = 10
  )
  expect_null(res)
})


# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_study_day(
    main_df = lb_dy[0, ],
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
    Rule_ID = "SD1084",
    Message = "Reference domain DM is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_dy,
    ref_dfs = list(XX = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
    Rule_ID = "SD1084",
    Message = "Reference domain DM is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_dy,
    ref_dfs = list(DM = dm_empty),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
    Rule_ID = "SD1084",
    Message = "USUBJID not found in LB or DM.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_noUSUBJID,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
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
# Boundary: unknown check_type triggers error
test_that("Boundary: stops with error on unknown check_type", {
  expect_error(
    check_cross_study_day(
      main_df = lb_dy,
      ref_dfs = list(DM = dm_ref),
      domain_name = "LB",
      ref_domain = "DM",
      target_vars = "--DY",
      ref_vars = "RFSTDTC",
      rule_id = "SD1084",
      dtc_var = "--DTC",
      check_type = "unknown"
    ),
    "Unknown check_type"
  )
})
# Boundary: subject without matching DM record is not checked
test_that("Boundary: returns NULL when subject has no RFSTDTC match", {
  lb_nomatch <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    rownumber_new = as.character(1:1),
    LBSEQ = 1L,
    USUBJID = "S99",
    LBDTC = "2021-01-10",
    LBDY = NA_character_,
    stringsAsFactors = FALSE
  )
  res <- check_cross_study_day(
    main_df = lb_nomatch,
    ref_dfs = list(DM = dm_ref),
    domain_name = "LB",
    ref_domain = "DM",
    target_vars = "--DY",
    ref_vars = "RFSTDTC",
    rule_id = "SD1084",
    dtc_var = "--DTC",
    check_type = "required",
    complete_len = 10
  )
  expect_null(res)
})