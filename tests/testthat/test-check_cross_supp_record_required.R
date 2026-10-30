# ---- Datasets ----
# SD1143: AE source domain with AESMIE condition
ae_1143 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S01", "S02", "S03"),
  AESEQ = c(1, 2, 1, 1),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER", "RASH"),
  AESMIE = c("Y", "N", "Y", "Y"),
  stringsAsFactors = FALSE
)
# SD1143: SUPPAE with AESOSP record for S02/seq1 only
suppae_1143 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:2),
  USUBJID = c("S02", "S01"),
  IDVAR = c("AESEQ", "AESEQ"),
  IDVARVAL = c("1", "2"),
  QNAM = c("AESOSP", "AETRTEM"),
  QVAL = c("MYELODYSPLASIA", "Y"),
  stringsAsFactors = FALSE
)
# SD1143: compliant SUPPAE with AESOSP records for all AESMIE='Y' rows
suppae_1143_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S01"),
  IDVAR = "AESEQ",
  IDVARVAL = c("1", "1", "1", "2"),
  QNAM = c("AESOSP", "AESOSP", "AESOSP", "AETRTEM"),
  QVAL = c("MEDICALLY IMPORTANT", "MYELODYSPLASIA", "OTHER", "Y"),
  stringsAsFactors = FALSE
)
# SD1143: SUPPAE with AESOSP but missing QVAL for S03/seq1
suppae_1143_qval <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:3),
  USUBJID = c("S02", "S01", "S03"),
  IDVAR = "AESEQ",
  IDVARVAL = c("1", "2", "1"),
  QNAM = c("AESOSP", "AETRTEM", "AESOSP"),
  QVAL = c("MYELODYSPLASIA", "Y", ""),
  stringsAsFactors = FALSE
)
# SD1143: SUPPAE with lowercase qnam/qval (case-insensitive matching)
suppae_1143_lower <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  IDVAR = "AESEQ",
  IDVARVAL = c("1", "1"),
  QNAM = c("aesosp", "AETRTEM"),
  QVAL = c("some description", "Y"),
  stringsAsFactors = FALSE
)
# SD1143: SUPPAE with wrong IDVAR for S01/seq1 (only matters when check_idvar=TRUE)
suppae_1143_idvar <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:2),
  USUBJID = c("S02", "S01"),
  IDVAR = c("AESEQ", "AETERM"),
  IDVARVAL = c("1", "1"),
  QNAM = c("AESOSP", "AESOSP"),
  QVAL = c("MYELODYSPLASIA", "DESCRIPTION"),
  stringsAsFactors = FALSE
)
# SD1321: AE source domain (all rows checked, no condition)
ae_1321 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S01", "S02"),
  AESEQ = c(1, 2, 1),
  AETERM = c("HEADACHE", "NAUSEA", "FEVER"),
  stringsAsFactors = FALSE
)
# SD1321: SUPPAE with TRTEMFL record for S01/seq1 and S02/seq1 only
suppae_1321 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  IDVAR = "AESEQ",
  IDVARVAL = c("1", "1"),
  QNAM = c("TRTEMFL", "TRTEMFL"),
  QVAL = c("Y", "Y"),
  stringsAsFactors = FALSE
)
# SD1321: compliant SUPPAE with TRTEMFL for all AE records
suppae_1321_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SUPPAE",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S01", "S02"),
  IDVAR = "AESEQ",
  IDVARVAL = c("1", "2", "1"),
  QNAM = c("TRTEMFL", "TRTEMFL", "TRTEMFL"),
  QVAL = c("Y", "N", "Y"),
  stringsAsFactors = FALSE
)
# Boundary: AE domain without AESEQ
ae_no_seq <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  rownumber_new = as.character(1:2),
  USUBJID = c("S01", "S02"),
  AETERM = c("HEADACHE", "NAUSEA"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1143: AESMIE='Y' records require an AESOSP record in SUPPAE
test_that("SD1143: catches AESMIE='Y' record without AESOSP record in SUPPAE", {
  expected_error <- data.frame(
    Row = c("1", "4"),
    Variable = c("AESEQ", "AESEQ"),
    Original_Value = c("S01 | 1", "S03 | 1"),
    Rule_ID = c("SD1143", "SD1143"),
    Message = c(
      "No SUPPAE record with QNAM='AESOSP' and populated QVAL found for this AE record with AESMIE equal 'Y' (linked by USUBJID + AESEQ).",
      "No SUPPAE record with QNAM='AESOSP' and populated QVAL found for this AE record with AESMIE equal 'Y' (linked by USUBJID + AESEQ)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1143: returns NULL when all AESMIE='Y' records have AESOSP in SUPPAE", {
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143_ok),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})
test_that("SD1143: reports AESOSP record with empty QVAL when check_qval_nonmissing is TRUE", {
  expected_error <- data.frame(
    Row = c("1", "4"),
    Variable = "AESEQ",
    Original_Value = c("S01 | 1", "S03 | 1"),
    Rule_ID = "SD1143",
    Message = "No SUPPAE record with QNAM='AESOSP' and populated QVAL found for this AE record with AESMIE equal 'Y' (linked by USUBJID + AESEQ).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143_qval),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1143: matches QNAM and values case-insensitively", {
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143_lower),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Original_Value, c("S02 | 1", "S03 | 1"))
})
test_that("SD1143: ignores IDVAR mismatch when check_idvar is FALSE", {
  expected_error <- data.frame(
    Row = c("4"),
    Variable = "AESEQ",
    Original_Value = c("S03 | 1"),
    Rule_ID = "SD1143",
    Message = "No SUPPAE record with QNAM='AESOSP' and populated QVAL found for this AE record with AESMIE equal 'Y' (linked by USUBJID + AESEQ).",
    stringsAsFactors = FALSE
  )

  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143_idvar),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)

})
test_that("SD1143: enforces standard IDVAR when check_idvar is TRUE", {
  expected_error <- data.frame(
    Row = c("1", "4"),
    Variable = "AESEQ",
    Original_Value = c("S01 | 1", "S03 | 1"),
    Rule_ID = "SD1143",
    Message = "No SUPPAE record with QNAM='AESOSP' and populated QVAL found for this AE record with AESMIE equal 'Y' (linked by USUBJID + AESEQ).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143_idvar),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    check_idvar = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1321: all AE records require a TRTEMFL record in SUPPAE
test_that("SD1321: catches AE records without TRTEMFL record in SUPPAE", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "AESEQ",
    Original_Value = "S01 | 2",
    Rule_ID = "SD1321",
    Message = "No SUPPAE record with QNAM='TRTEMFL' and populated QVAL found for this AE record (linked by USUBJID + AESEQ).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = suppae_1321),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1321: returns NULL when all AE records have TRTEMFL in SUPPAE", {
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = suppae_1321_ok),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})
test_that("SD1321: reports all missing records when SUPPAE is missing required QNAM entirely", {
  suppae_other <- suppae_1321
  suppae_other$QNAM <- c("AETRTEM", "AESOSP")
  expected_error <- data.frame(
    Row = c("1", "2", "3"),
    Variable = c("AESEQ", "AESEQ", "AESEQ"),
    Original_Value = c("S01 | 1", "S01 | 2", "S02 | 1"),
    Rule_ID = c("SD1321", "SD1321", "SD1321"),
    Message = c(
      "No SUPPAE record with QNAM='TRTEMFL' and populated QVAL found for this AE record (linked by USUBJID + AESEQ).",
      "No SUPPAE record with QNAM='TRTEMFL' and populated QVAL found for this AE record (linked by USUBJID + AESEQ).",
      "No SUPPAE record with QNAM='TRTEMFL' and populated QVAL found for this AE record (linked by USUBJID + AESEQ)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = suppae_other),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1321: matches subject keys case-insensitively", {
  ae_lower <- ae_1321
  ae_lower$USUBJID <- c("s01", "S01", "S02")
  res <- check_cross_supp_record_required(
    main_df = ae_lower,
    ref_dfs = list(SUPPAE = suppae_1321_ok),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})
test_that("SD1321: matches IDVARVAL by character comparison regardless of type", {
  supp_char <- suppae_1321_ok
  supp_char$IDVARVAL <- c(1, 2, 1)
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = supp_char),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_supp_record_required(
    main_df = ae_1321[0, ],
    ref_dfs = list(SUPPAE = suppae_1321),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})
# Boundary: supplemental domain missing from ref_dfs list
test_that("Boundary: reports error when supplemental domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1321",
    Message = "Supplemental domain SUPPAE is missing or empty or lacks USUBJID/QNAM/IDVARVAL.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE2 = suppae_1321),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: supplemental domain empty
test_that("Boundary: reports error when supplemental domain is empty", {
  supp_empty <- suppae_1321[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1321",
    Message = "Supplemental domain SUPPAE is missing or empty or lacks USUBJID/QNAM/IDVARVAL.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = supp_empty),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: supplemental domain lacks required columns
test_that("Boundary: reports error when supplemental domain lacks required columns", {
  supp_nocols <- suppae_1321
  supp_nocols$IDVARVAL <- NULL
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1321",
    Message = "Supplemental domain SUPPAE is missing or empty or lacks USUBJID/QNAM/IDVARVAL.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_1321,
    ref_dfs = list(SUPPAE = supp_nocols),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: USUBJID or link_var not found in main domain
test_that("Boundary: reports error when link_var not found in main domain", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1321",
    Message = "USUBJID or AESEQ not found in AE.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_supp_record_required(
    main_df = ae_no_seq,
    ref_dfs = list(SUPPAE = suppae_1321),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1321",
    supp_qnam = "TRTEMFL",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: no row satisfies the source conditions
test_that("Boundary: returns NULL when no row satisfies the source conditions", {
  res <- check_cross_supp_record_required(
    main_df = ae_1143,
    ref_dfs = list(SUPPAE = suppae_1143),
    domain_name = "AE",
    ref_domain = "SUPPAE",
    rule_id = "SD1143",
    supp_qnam = "AESOSP",
    link_var = "AESEQ",
    check_qval_nonmissing = TRUE,
    cond_vars = "AESMIE",
    cond_ops = "equal",
    cond_vals = "NOTEXIST",
    logic_op = "AND",
    check_idvar = FALSE
  )
  expect_null(res)
})