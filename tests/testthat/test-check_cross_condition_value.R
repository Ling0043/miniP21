# ---- Datasets ----
# SD1252: DM domain with DTHFL
dm_dthfl <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  DTHFL = c("", "Y", ""),
  stringsAsFactors = FALSE
)
# SD1252: SS reference domain (S01 DEAD, S02 not DEAD)
ss_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:2),
  SSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  SSSTRESC = c("DEAD", "ALIVE"),
  stringsAsFactors = FALSE
)
# SD1252: compliant SS reference domain (no DEAD records)
ss_ref_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:2),
  SSSEQ = 1:2,
  USUBJID = c("S02", "S03"),
  SSSTRESC = c("ALIVE", "ALIVE"),
  stringsAsFactors = FALSE
)
# SD1252: SS reference with multiple violations
ss_ref_multi <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SS",
  rownumber_new = as.character(1:2),
  SSSEQ = 1:2,
  USUBJID = c("S01", "S03"),
  SSSTRESC = c("DEAD", "DEAD"),
  stringsAsFactors = FALSE
)
# SD1253: DM domain with DTHFL
dm_dd <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  DTHFL = c("", "Y", "N"),
  stringsAsFactors = FALSE
)
# SD1253: DD reference domain (no ref-side conditions)
dd_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DD",
  rownumber_new = as.character(1:2),
  DDSEQ = 1:2,
  USUBJID = c("S01", "S03"),
  DSTERM = c("DEATH DETAILS", "DEATH DETAILS"),
  stringsAsFactors = FALSE
)
# SD1253: compliant DD reference domain
dd_ref_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DD",
  rownumber_new = as.character(1:1),
  DDSEQ = 1L,
  USUBJID = c("S02"),
  DSTERM = c("DEATH DETAILS"),
  stringsAsFactors = FALSE
)
# SD1254/SD1255: DM domain with DTHFL
dm_ae <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  DTHFL = c("", "Y", ""),
  stringsAsFactors = FALSE
)
# SD1254: AE reference domain with AEOUT (S01 FATAL, S02 not fatal)
ae_ref_out <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  AEOUT = c("FATAL", "RECOVERED", "FATAL"),
  stringsAsFactors = FALSE
)
# SD1254: compliant AE reference domain (no FATAL records)
ae_ref_out_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  AEOUT = c("RECOVERED", "RECOVERED", "RECOVERED"),
  stringsAsFactors = FALSE
)
# SD1255: AE reference domain with AESDTH (S01 'Y', S02/S03 'N')
ae_ref_sdth <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  AESDTH = c("Y", "N", "N"),
  stringsAsFactors = FALSE
)
# SD1255: compliant AE reference domain (no AESDTH='Y' records)
ae_ref_sdth_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  AESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  AESDTH = c("N", "N", "N"),
  stringsAsFactors = FALSE
)
# SD1256: DM domain with DTHFL
dm_ds <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  DTHFL = c("", "Y", "N"),
  stringsAsFactors = FALSE
)
# SD1256: DS reference domain (S01 DEATH, S02 COMPLETED)
ds_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  DSDECOD = c("DEATH", "COMPLETED"),
  stringsAsFactors = FALSE
)
# SD1256: DS reference domain where S03 has DEATH record
ds_ref_s03 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:2),
  DSSEQ = 1:2,
  USUBJID = c("S02", "S03"),
  DSDECOD = c("COMPLETED", "DEATH"),
  stringsAsFactors = FALSE
)
# SD1256: compliant DS reference domain (no DEATH records)
ds_ref_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  rownumber_new = as.character(1:1),
  DSSEQ = 1L,
  USUBJID = c("S02"),
  DSDECOD = c("COMPLETED"),
  stringsAsFactors = FALSE
)
# Boundary: DM domain with NA DTHFL values
dm_na_flag <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  DTHFL = c(NA, NA),
  stringsAsFactors = FALSE
)
# Boundary: DM domain with empty USUBJID
dm_empty_subj <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:2),
  USUBJID = c("", "S02"),
  DTHFL = c("", ""),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1252: DTHFL must equal 'Y' when SS.SSSTRESC = 'DEAD'
test_that("SD1252: catches DTHFL not 'Y' when SSSTRESC='DEAD' record exists", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DTHFL",
    Original_Value = "",
    Rule_ID = "SD1252",
    Message = "DTHFL must equal 'Y' when a record exists in SS(USUBJID) with SS(SSSTRESC) is equal 'DEAD'",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_dthfl,
    ref_dfs = list(SS = ss_ref),
    domain_name = "DM",
    ref_domain = "SS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1252",
    expected_val = "Y",
    ref_cond_vars = "SSSTRESC",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEAD",
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
test_that("SD1252: returns NULL when no DEAD record qualifies", {
  res <- check_cross_condition_value(
    main_df = dm_dthfl,
    ref_dfs = list(SS = ss_ref_ok),
    domain_name = "DM",
    ref_domain = "SS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1252",
    expected_val = "Y",
    ref_cond_vars = "SSSTRESC",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEAD",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD1252: reports one row per violation when multiple subjects fail", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("DTHFL", "DTHFL"),
    Original_Value = c("", ""),
    Rule_ID = c("SD1252", "SD1252"),
    Message = "DTHFL must equal 'Y' when a record exists in SS(USUBJID) with SS(SSSTRESC) is equal 'DEAD'"
  )
  res <- check_cross_condition_value(
    main_df = dm_dthfl,
    ref_dfs = list(SS = ss_ref_multi),
    domain_name = "DM",
    ref_domain = "SS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1252",
    expected_val = "Y",
    ref_cond_vars = "SSSTRESC",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEAD",
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1253: DTHFL must equal 'Y' when any DD record is present
test_that("SD1253: catches DTHFL not 'Y' when DD record is present", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("DTHFL", "DTHFL"),
    Original_Value = c("", "N"),
    Rule_ID = c("SD1253", "SD1253"),
    Message = c(
      "DTHFL must equal 'Y' when a record exists in DD(USUBJID)",
      "DTHFL must equal 'Y' when a record exists in DD(USUBJID)"
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_dd,
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1253: returns NULL when DD record exists only for DTHFL='Y' subject", {
  res <- check_cross_condition_value(
    main_df = dm_dd,
    ref_dfs = list(DD = dd_ref_ok),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
    logic_op = "AND"
  )
  expect_null(res)
})
# SD1254: DTHFL must equal 'Y' when AE.AEOUT = 'FATAL'
test_that("SD1254: catches DTHFL not 'Y' when AEOUT='FATAL' record exists", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = "DTHFL",
    Original_Value = "",
    Rule_ID = "SD1254",
    Message = "DTHFL must equal 'Y' when a record exists in AE(USUBJID) with AE(AEOUT) is equal 'FATAL'",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_ae,
    ref_dfs = list(AE = ae_ref_out),
    domain_name = "DM",
    ref_domain = "AE",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1254",
    expected_val = "Y",
    ref_cond_vars = "AEOUT",
    ref_cond_ops = "equal",
    ref_cond_vals = "FATAL",
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1254: returns NULL when no FATAL record qualifies", {
  res <- check_cross_condition_value(
    main_df = dm_ae,
    ref_dfs = list(AE = ae_ref_out_ok),
    domain_name = "DM",
    ref_domain = "AE",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1254",
    expected_val = "Y",
    ref_cond_vars = "AEOUT",
    ref_cond_ops = "equal",
    ref_cond_vals = "FATAL",
    logic_op = "AND"
  )
  expect_null(res)
})
# SD1255: DTHFL must equal 'Y' when AE.AESDTH = 'Y'
test_that("SD1255: catches DTHFL not 'Y' when AESDTH='Y' record exists", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DTHFL",
    Original_Value = "",
    Rule_ID = "SD1255",
    Message = "DTHFL must equal 'Y' when a record exists in AE(USUBJID) with AE(AESDTH) is equal 'Y'",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_ae,
    ref_dfs = list(AE = ae_ref_sdth),
    domain_name = "DM",
    ref_domain = "AE",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1255",
    expected_val = "Y",
    ref_cond_vars = "AESDTH",
    ref_cond_ops = "equal",
    ref_cond_vals = "Y",
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
test_that("SD1255: returns NULL when no AESDTH='Y' record qualifies", {
  res <- check_cross_condition_value(
    main_df = dm_ae,
    ref_dfs = list(AE = ae_ref_sdth_ok),
    domain_name = "DM",
    ref_domain = "AE",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1255",
    expected_val = "Y",
    ref_cond_vars = "AESDTH",
    ref_cond_ops = "equal",
    ref_cond_vals = "Y",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD1255: returns NULL when AESDTH='Y' subject has DTHFL='Y'", {
  dm_ok <- dm_ae
  dm_ok$DTHFL[1] <- "Y"
  res <- check_cross_condition_value(
    main_df = dm_ok,
    ref_dfs = list(AE = ae_ref_sdth),
    domain_name = "DM",
    ref_domain = "AE",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1255",
    expected_val = "Y",
    ref_cond_vars = "AESDTH",
    ref_cond_ops = "equal",
    ref_cond_vals = "Y",
    logic_op = "AND"
  )
  expect_null(res)
})
# SD1256: DTHFL must equal 'Y' when DS.DSDECOD = 'DEATH'
test_that("SD1256: catches DTHFL not 'Y' when DSDECOD='DEATH' record exists", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DTHFL",
    Original_Value = "",
    Rule_ID = "SD1256",
    Message = "DTHFL must equal 'Y' when a record exists in DS(USUBJID) with DS(DSDECOD) is equal 'DEATH'",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_ds,
    ref_dfs = list(DS = ds_ref),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1256",
    expected_val = "Y",
    ref_cond_vars = "DSDECOD",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEATH",
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
test_that("SD1256: returns NULL when no DEATH record qualifies", {
  res <- check_cross_condition_value(
    main_df = dm_ds,
    ref_dfs = list(DS = ds_ref_ok),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1256",
    expected_val = "Y",
    ref_cond_vars = "DSDECOD",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEATH",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD1256: reports 'N' as original value when DTHFL is populated but wrong", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "DTHFL",
    Original_Value = "N",
    Rule_ID = "SD1256",
    Message = "DTHFL must equal 'Y' when a record exists in DS(USUBJID) with DS(DSDECOD) is equal 'DEATH'",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_ds,
    ref_dfs = list(DS = ds_ref_s03),
    domain_name = "DM",
    ref_domain = "DS",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1256",
    expected_val = "Y",
    ref_cond_vars = "DSDECOD",
    ref_cond_ops = "equal",
    ref_cond_vals = "DEATH",
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
# Boundary: NA DTHFL is treated as empty and reported
test_that("Boundary: reports NA DTHFL as empty original value", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DTHFL",
    Original_Value = "",
    Rule_ID = "SD1253",
    Message = "DTHFL must equal 'Y' when a record exists in DD(USUBJID)",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_na_flag,
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
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
# Boundary: empty USUBJID rows are not reported
test_that("Boundary: skips rows with empty USUBJID", {
  res <- check_cross_condition_value(
    main_df = dm_empty_subj,
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
    logic_op = "AND"
  )
  expect_null(res)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_condition_value(
    main_df = dm_dd[0, ],
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
    logic_op = "AND"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1253",
    Message = "Reference domain XX is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_dd,
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "XX",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
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
# Boundary: reference dataset empty
test_that("Boundary: reports error when reference dataset is empty", {
  dd_empty <- dd_ref[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1253",
    Message = "Reference domain DD is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_dd,
    ref_dfs = list(DD = dd_empty),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    ref_vars = "USUBJID",
    rule_id = "SD1253",
    expected_val = "Y",
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
# Boundary: ref_vars defaults to USUBJID when NULL
test_that("Boundary: ref_vars defaults to USUBJID when NULL", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("DTHFL", "DTHFL"),
    Original_Value = c("", "N"),
    Rule_ID = c("SD1253", "SD1253"),
    Message = c(
      "DTHFL must equal 'Y' when a record exists in DD(USUBJID)",
      "DTHFL must equal 'Y' when a record exists in DD(USUBJID)"
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_condition_value(
    main_df = dm_dd,
    ref_dfs = list(DD = dd_ref),
    domain_name = "DM",
    ref_domain = "DD",
    target_vars = "DTHFL",
    rule_id = "SD1253",
    expected_val = "Y",
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
