# ---- Datasets ----
# SD1041: AE domain for --CAT vs --SCAT not_equal
ae_cat_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  AECAT = c("Cardiac", "Serious", "Gastrointestinal"),
  AESCAT = c("Cardiac", "Non-Serious", "Mild"),
  rownumber_new = c(1, 4, 7),
  stringsAsFactors = FALSE
)

# SD1327/SD1328: RELSUB domain for RSUBJID vs POOLID / USUBJID
relsub_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "RELSUB",
  RSUBJID = c("POOL1", "SUB1", "POOL2", "SUB3"),
  POOLID = c("POOL1", "POOL1", NA, ""),
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB3"),
  rownumber_new = c(1, 3, 6, 9),
  stringsAsFactors = FALSE
)

# SD0085: IE domain for IEORRES vs IESTRESC equal
ie_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "IE",
  IESEQ = 1:4,
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4"),
  IEORRES = c("Y", "N", "120", "NA"),
  IESTRESC = c("Y", "Y", "120", "NA"),
  rownumber_new = c(1, 4, 8, 11),
  stringsAsFactors = FALSE
)

# SD1353: LB domain for LBSTRESC vs LBORRES equal with require_numeric and unit_vars
lb_stres_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:6,
  LBORRES = c("10", "5.5", "BLQ", "7", "3", "12"),
  LBSTRESC = c("10", "5.5", "BLQ", "8", "3", "20"),
  LBORRESU = c("mg/dL", "mg/dL", "mg/dL", "mg/dL", NA, "mg/dL"),
  LBSTRESU = c("mg/dL", "mg/dL", "mg/dL", "mg/dL", NA, "g/L"),
  rownumber_new = c(1, 3, 5, 8, 10, 12),
  stringsAsFactors = FALSE
)

# SD1462/SD1463: AE domain for SOC vs BODSYS equal
ae_soc_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:4,
  AESOC = c("CARDIAC DISORDERS", "GASTROINTESTINAL DISORDERS", "SKIN DISORDERS", "NERVOUS SYSTEM DISORDERS"),
  AEBODSYS = c("CARDIAC DISORDERS", "SKIN DISORDERS", "SKIN DISORDERS", "INFECTIONS"),
  AESOCCD = c("10008495", "10017947", "20012345", "10029205"),
  AEBDSYCD = c("10008495", "20012345", "20012345", "10029205"),
  rownumber_new = c(1, 5, 9, 12),
  stringsAsFactors = FALSE
)

# SD2236/SD2237: DM domain for ACTARMCD vs ARMCD / ACTARM vs ARM
dm_arm_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3", "SUB4"),
  ARMCD = c("TRT1", "TRT2", "CTRL", "SCRNFAIL"),
  ACTARMCD = c("TRT1", "CTRL", "CTRL", "SCRNFAIL"),
  ARM = c("Treatment 1", "Treatment 2", "Control", "Screen Failure"),
  ACTARM = c("Treatment 1", "Control", "Control", "Screen Failure"),
  rownumber_new = c(1, 3, 6, 9),
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1041: Values for --CAT and --SCAT are identical
test_that("SD1041: catches identical AECAT and AESCAT values (one row per error)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AECAT, AESCAT",
    Original_Value = "Cardiac, Cardiac",
    Rule_ID = "SD1041",
    Message = "Values of AECAT and AESCAT are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = ae_cat_violation,
    domain_name = "AE",
    target_vars = "--CAT",
    var2 = "--SCAT",
    relation = "not_equal",
    rule_id = "SD1041"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD1327: RSUBJID is equal to POOLID
test_that("SD1327: catches RSUBJID equal to POOLID (one row per error)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RSUBJID, POOLID",
    Original_Value = "POOL1, POOL1",
    Rule_ID = "SD1327",
    Message = "Values of RSUBJID and POOLID are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = relsub_violation,
    domain_name = "RELSUB",
    target_vars = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD1328: RSUBJID is equal to USUBJID
test_that("SD1328: catches RSUBJID equal to USUBJID (one row per error)", {
  # Row 9: RSUBJID='SUB3' == USUBJID='SUB3'
  # Row 1: RSUBJID='POOL1' == USUBJID='SUB1'? No. Row 3: 'SUB1' vs 'SUB2'? No.
  expected_error <- data.frame(
    Row = "9",
    Variable = "RSUBJID, USUBJID",
    Original_Value = "SUB3, SUB3",
    Rule_ID = "SD1328",
    Message = "Values of RSUBJID and USUBJID are identical, but they should not be.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = relsub_violation,
    domain_name = "RELSUB",
    target_vars = "RSUBJID",
    var2 = "USUBJID",
    relation = "not_equal",
    rule_id = "SD1328"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD0085: Mismatch between IEORRES and IESTRESC values
test_that("SD0085: catches IEORRES not equal to IESTRESC (one row per error)", {
  # Row 4: IEORRES='N' vs IESTRESC='Y' -> mismatch
  # Rows 1, 8, 11: equal -> no error
  expected_error <- data.frame(
    Row = "4",
    Variable = "IEORRES, IESTRESC",
    Original_Value = "N, Y",
    Rule_ID = "SD0085",
    Message = "Values of IEORRES and IESTRESC should be identical.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = ie_violation,
    domain_name = "IE",
    target_vars = "IEORRES",
    var2 = "IESTRESC",
    relation = "equal",
    rule_id = "SD0085"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD1353: --STRESC does not equal --ORRES when --STRESU is equal to --ORRESU
test_that("SD1353: catches LBSTRESC != LBORRES only when numeric and units match (one row per error)", {
  # Row 1: '10' vs '10', units equal -> no error
  # Row 3: '5.5' vs '5.5', units equal -> no error
  # Row 5: 'BLQ' vs 'BLQ', non-numeric -> no error (require_numeric)
  # Row 8: '7' vs '8', numeric, units equal -> ERROR
  # Row 10: '3' vs '3', units both null -> valid_cond TRUE, values equal -> no error
  # Row 12: '12' vs '20', numeric but units differ -> no error (unit precondition fails)
  expected_error <- data.frame(
    Row = "8",
    Variable = "LBSTRESC, LBORRES",
    Original_Value = "8, 7",
    Rule_ID = "SD1353",
    Message = "Values of LBSTRESC and LBORRES should be identical.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = lb_stres_violation,
    domain_name = "LB",
    target_vars = "--STRESC",
    var2 = "--ORRES",
    relation = "equal",
    rule_id = "SD1353",
    require_numeric = TRUE,
    unit_vars = c("--STRESU", "--ORRESU")
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD1462: Not the same values for --SOC and --BODSYS
test_that("SD1462: catches AESOC not equal to AEBODSYS (one row per error)", {
  # Rows 5, 12 mismatch; rows 1, 9 equal
  expected_error <- data.frame(
    Row = c("5", "12"),
    Variable = c("AESOC, AEBODSYS", "AESOC, AEBODSYS"),
    Original_Value = c(
      "GASTROINTESTINAL DISORDERS, SKIN DISORDERS",
      "NERVOUS SYSTEM DISORDERS, INFECTIONS"
    ),
    Rule_ID = c("SD1462", "SD1462"),
    Message = rep("Values of AESOC and AEBODSYS should be identical.", 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = ae_soc_violation,
    domain_name = "AE",
    target_vars = "--SOC",
    var2 = "--BODSYS",
    relation = "equal",
    rule_id = "SD1462"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# SD1463: Not the same values for --SOCCD and --BDSYCD
test_that("SD1463: catches AESOCCD not equal to AEBDSYCD (one row per error)", {
  # Rows 5, 12 mismatch; rows 1, 9 equal
  expected_error <- data.frame(
    Row = c("5", "12"),
    Variable = c("AESOCCD, AEBDSYCD", "AESOCCD, AEBDSYCD"),
    Original_Value = c("10017947, 20012345", "10029205, 10029205")[c(1, 2)][c(1, 2)],
    Rule_ID = c("SD1463", "SD1463"),
    Message = rep("Values of AESOCCD and AEBDSYCD should be identical.", 2),
    stringsAsFactors = FALSE
  )
  # Fix: row 12 has AESOCCCD=10029205 vs AEBDSYCD=10029205 -> equal, no error
  expected_error <- expected_error[1, ]
  
  res <- check_compare_vars(
    df = ae_soc_violation,
    domain_name = "AE",
    target_vars = "--SOCCD",
    var2 = "--BDSYCD",
    relation = "equal",
    rule_id = "SD1463"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1) # Only row 5 mismatches
  expect_equal(res$Row, "5")
  expect_equal(res$Variable, "AESOCCD, AEBDSYCD")
  expect_equal(res$Original_Value, "10017947, 20012345")
  expect_equal(res$Rule_ID, "SD1463")
  expect_equal(res$Message, "Values of AESOCCD and AEBDSYCD should be identical.")
})

# SD2236: ACTARMCD does not equal ARMCD
test_that("SD2236: catches ACTARMCD not equal to ARMCD (one row per error)", {
  # Rows 3, 9 mismatch; rows 1, 6 equal
  expected_error <- data.frame(
    Row = c("3", "9"),
    Variable = c("ACTARMCD, ARMCD", "ACTARMCD, ARMCD"),
    Original_Value = c("CTRL, TRT2", "SCRNFAIL, SCRNFAIL")[c(1, 2)],
    Rule_ID = c("SD2236", "SD2236"),
    Message = rep("Values of ACTARMCD and ARMCD should be identical.", 2),
    stringsAsFactors = FALSE
  )
  # Row 9: ACTARMCD='SCRNFAIL' == ARMCD='SCRNFAIL' -> equal, no error
  expected_error <- expected_error[1, ]
  
  res <- check_compare_vars(
    df = dm_arm_violation,
    domain_name = "DM",
    target_vars = "ACTARMCD",
    var2 = "ARMCD",
    relation = "equal",
    rule_id = "SD2236"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1) # Only row 3 mismatches
  expect_equal(res$Row, "3")
  expect_equal(res$Variable, "ACTARMCD, ARMCD")
  expect_equal(res$Original_Value, "CTRL, TRT2")
  expect_equal(res$Rule_ID, "SD2236")
  expect_equal(res$Message, "Values of ACTARMCD and ARMCD should be identical.")
})

# SD2237: ACTARM does not equal ARM
test_that("SD2237: catches ACTARM not equal to ARM (one row per error)", {
  # Row 3: 'Control' vs 'Treatment 2' -> mismatch
  expected_error <- data.frame(
    Row = "3",
    Variable = "ACTARM, ARM",
    Original_Value = "Control, Treatment 2",
    Rule_ID = "SD2237",
    Message = "Values of ACTARM and ARM should be identical.",
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = dm_arm_violation,
    domain_name = "DM",
    target_vars = "ACTARM",
    var2 = "ARM",
    relation = "equal",
    rule_id = "SD2237"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# ---- Additional behavioral tests ----
test_that("not_equal: ignores rows where either value is missing (check_na = FALSE)", {
  res <- check_compare_vars(
    df = relsub_violation,
    domain_name = "RELSUB",
    target_vars = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327"
  )
  # Row 6 (POOLID=NA) and row 9 (POOLID="") must not be flagged
  expect_false("6" %in% res$Row)
  expect_false("9" %in% res$Row)
})

test_that("not_equal with check_na = TRUE: flags both-missing rows", {
  relsub_na <- data.frame(
    RSUBJID = c("SUB1", NA),
    POOLID = c("SUB1", NA),
    rownumber_new = 1:2,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = relsub_na,
    domain_name = "RELSUB",
    target_vars = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327",
    check_na = TRUE
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2) # Both rows identical (one by value, one by NA)
  expect_equal(res$Row, c("1", "2"))
})

test_that("equal: ignores rows where either value is missing", {
  ie_missing <- data.frame(
    IEORRES = c("Y", NA, "N"),
    IESTRESC = c("Y", "Y", NA),
    rownumber_new = 1:3,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = ie_missing,
    domain_name = "IE",
    target_vars = "IEORRES",
    var2 = "IESTRESC",
    relation = "equal",
    rule_id = "SD0085"
  )
  expect_null(res)
})

test_that("returns NULL when no violation (not_equal)", {
  ae_ok <- data.frame(
    AECAT = "Cardiac",
    AESCAT = "Serious",
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "--CAT",
    var2 = "--SCAT",
    relation = "not_equal",
    rule_id = "SD1041"
  )
  expect_null(res)
})

test_that("returns NULL when no violation (equal)", {
  dm_ok <- data.frame(
    ARMCD = "TRT1",
    ACTARMCD = "TRT1",
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "ACTARMCD",
    var2 = "ARMCD",
    relation = "equal",
    rule_id = "SD2236"
  )
  expect_null(res)
})

# Boundary: Missing required columns
test_that("Boundary: returns NULL when required columns are missing", {
  df_missing <- data.frame(
    RSUBJID = "SUB1",
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = df_missing,
    domain_name = "RELSUB",
    target_vars = "RSUBJID",
    var2 = "POOLID",
    relation = "not_equal",
    rule_id = "SD1327"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when unit_vars columns are missing (SD1353)", {
  lb_no_units <- data.frame(
    LBORRES = "10",
    LBSTRESC = "20",
    rownumber_new = 1,
    stringsAsFactors = FALSE
  )
  
  res <- check_compare_vars(
    df = lb_no_units,
    domain_name = "LB",
    target_vars = "--STRESC",
    var2 = "--ORRES",
    relation = "equal",
    rule_id = "SD1353",
    require_numeric = TRUE,
    unit_vars = c("--STRESU", "--ORRESU")
  )
  expect_null(res)
})

test_that("Boundary: errors on unsupported relation type", {
  expect_error(
    check_compare_vars(
      df = ae_cat_violation,
      domain_name = "AE",
      target_vars = "--CAT",
      var2 = "--SCAT",
      relation = "greater_than",
      rule_id = "SD1041"
    ),
    "not implemented"
  )
})
