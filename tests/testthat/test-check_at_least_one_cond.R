# ---- Datasets ----
# SD0009: AE domain for serious event criteria
ae <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AESER = c("Y", "Y", "N"),
  AESCAN = c("N", "Y", "N"),
  AESCONG = c("N", "N", "N"),
  AESDISAB = c("N", "N", "N"),
  AESDTH = c("N", "N", "N"),
  AESHOSP = c("N", "N", "N"),
  AESLIFE = c("N", "N", "N"),
  AESMIE = c("N", "N", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD0022: EX domain for missing start time-point
ex <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:3,
  EXOCCUR = c("Y", "Y", "N"),
  EXSTAT = c("", "", "NOT DONE"),
  EXSTDTC = c("", "2021-01-01", ""),
  EXSTRF = c("", "", ""),
  EXSTRTPT = c("", "", ""),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1333: AE domain for outcome and end date/duration
ae_out <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AEOUT = c("RECOVERED/RESOLVED", "RECOVERED/RESOLVED", "NOT RECOVERED"),
  AEENDTC = c("", "2021-06-01", ""),
  AEDUR = c("", "", ""),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)



# ---- Datasets ----
# SD1121: DM domain, all records except ARMCD=SCRNFAIL/NOTASSGN/NULL must have AGE or AGETXT populated
dm_sd1121 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:4,
  ARMCD = c("TREAT", "SCRNFAIL", "NOTASSGN", ""),
  AGE = c("", "30", "", ""),
  AGETXT = c("", "", "", ""),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# SD1121 compliant dataset
dm_sd1121_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:4,
  ARMCD = c("TREAT", "SCRNFAIL", "NOTASSGN", ""),
  AGE = c(28, "30", "", ""),
  AGETXT = c("", "", "", ""),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: ARMCD populated non-exception value, both AGE/AGETXT blank multi rows
dm_sd1121_multi_err <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:3,
  ARMCD = c("ARM1", "ARM2", "SCRNFAIL"),
  AGE = c("", "", 25),
  AGETXT = c("", "", ""),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)


# SD2021: DM domain for AGE and AGETXT when AGEU is provided
dm <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  AGE = c(NA, 25, NA),
  AGETXT = c("", "", "18-65"),
  AGEU = c("YEARS", "YEARS", "YEARS"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary 1: Missing target_vars in df
ts <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary 2: Condition not met, but target_vars missing
ae_cond_not_met <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AESER = c("N", "N", "N"),
  AESCAN = c("N", "N", "N"),
  AESCONG = c("N", "N", "N"),
  AESDISAB = c("N", "N", "N"),
  AESDTH = c("N", "N", "N"),
  AESHOSP = c("N", "N", "N"),
  AESLIFE = c("N", "N", "N"),
  AESMIE = c("N", "N", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD0009: No qualifiers set to 'Y' when AE is Serious
test_that("SD0009: catches no qualifiers set to 'Y' when AESER='Y'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AESCAN, AESCONG, AESDISAB, AESDTH, AESHOSP, AESLIFE, AESMIE",
    Original_Value = "",
    Rule_ID = "SD0009",
    Message = "At least one of AESCAN, AESCONG, AESDISAB, AESDTH, AESHOSP, AESLIFE, AESMIE must equal 'Y' when AESER='Y'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_at_least_one_cond(
    df = ae,
    domain_name = "AE",
    target_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    rule_id = "SD0009",
    cond_vars = "AESER",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    expected_val = "Y"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0009: returns NULL when no violation", {
  res <- check_at_least_one_cond(
    df = ae,
    domain_name = "AE",
    target_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    rule_id = "SD0009",
    cond_vars = "AESER",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    expected_val = "Y"
  )
  # The provided dataset has row 2 as valid. We test the entire df and expect only row 1 error.
  # To check for completely NULL, we can make row 1 valid.
  ae_ok <- ae
  ae_ok$AESCAN[1] <- "Y"
  res <- check_at_least_one_cond(
    df = ae_ok,
    domain_name = "AE",
    target_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    rule_id = "SD0009",
    cond_vars = "AESER",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    expected_val = "Y"
  )
  expect_null(res)
})

# SD0022: Missing Start Time-Point value
test_that("SD0022: catches missing Start Time-Point when EXOCCUR='Y' and EXSTAT is missing", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXSTDTC, EXSTRF, EXSTRTPT",
    Original_Value = "",
    Rule_ID = "SD0022",
    Message = "At least one of EXSTDTC, EXSTRF, EXSTRTPT should be populated when EXOCCUR='Y' and EXSTAT is missing.",
    stringsAsFactors = FALSE
  )
  
  res <- check_at_least_one_cond(
    df = ex,
    domain_name = "EX",
    target_vars = c("--STDTC", "--STRF", "--STRTPT"),
    rule_id = "SD0022",
    cond_vars = c("--OCCUR", "--STAT"),
    cond_ops = c("equal", "missing"),
    cond_vals = c("Y", ""),
    logic_op = "AND",
    expected_val = NULL
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0022: returns NULL when no violation", {
  ex_ok <- ex
  ex_ok$EXSTDTC[1] <- "2021-01-01"
  res <- check_at_least_one_cond(
    df = ex_ok,
    domain_name = "EX",
    target_vars = c("--STDTC", "--STRF", "--STRTPT"),
    rule_id = "SD0022",
    cond_vars = c("--OCCUR", "--STAT"),
    cond_ops = c("equal", "missing"),
    cond_vals = c("Y", ""),
    logic_op = "AND",
    expected_val = NULL
  )
  expect_null(res)
})

# SD1333: AEOUT = RECOVERED/RESOLVED, but an end date or collected duration is not provided
test_that("SD1333: catches missing AEENDTC and AEDUR when AEOUT='RECOVERED/RESOLVED'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AEENDTC, AEDUR",
    Original_Value = "",
    Rule_ID = "SD1333",
    Message = "At least one of AEENDTC, AEDUR should be populated when AEOUT='RECOVERED/RESOLVED'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_at_least_one_cond(
    df = ae_out,
    domain_name = "AE",
    target_vars = c("AEENDTC", "AEDUR"),
    rule_id = "SD1333",
    cond_vars = "AEOUT",
    cond_ops = "equal",
    cond_vals = "RECOVERED/RESOLVED",
    logic_op = "AND",
    expected_val = NULL
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1333: returns NULL when no violation", {
  ae_out_ok <- ae_out
  ae_out_ok$AEENDTC[1] <- "2021-06-01"
  res <- check_at_least_one_cond(
    df = ae_out_ok,
    domain_name = "AE",
    target_vars = c("AEENDTC", "AEDUR"),
    rule_id = "SD1333",
    cond_vars = "AEOUT",
    cond_ops = "equal",
    cond_vals = "RECOVERED/RESOLVED",
    logic_op = "AND",
    expected_val = NULL
  )
  expect_null(res)
})

# SD2021: Missing values for both AGE and AGETXT when AGEU is provided
test_that("SD2021: catches missing AGE and AGETXT when AGEU is provided", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AGE, AGETXT",
    Original_Value = "",
    Rule_ID = "SD2021",
    Message = "At least one of AGE, AGETXT should be populated when AGEU is provided.",
    stringsAsFactors = FALSE
  )
  
  res <- check_at_least_one_cond(
    df = dm,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD2021",
    cond_vars = "AGEU",
    cond_ops = "non_missing",
    cond_vals = "",
    logic_op = "AND",
    expected_val = NULL
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2021: returns NULL when no violation", {
  dm_ok <- dm
  dm_ok$AGE[1] <- 30
  res <- check_at_least_one_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD2021",
    cond_vars = "AGEU",
    cond_ops = "non_missing",
    cond_vals = "",
    logic_op = "AND",
    expected_val = NULL
  )
  expect_null(res)
})

# Boundary 1: Missing target_vars in df
test_that("Boundary: returns NULL when target_vars missing in df", {
  res <- check_at_least_one_cond(
    df = ts,
    domain_name = "TS",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD_BOUND1",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    logic_op = "AND",
    expected_val = NULL
  )
  expect_null(res)
})

# Boundary 2: Condition not met, but target_vars missing
test_that("Boundary: returns NULL when condition is not met", {
  res <- check_at_least_one_cond(
    df = ae_cond_not_met,
    domain_name = "AE",
    target_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    rule_id = "SD_BOUND2",
    cond_vars = "AESER",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    expected_val = "Y"
  )
  expect_null(res)
})

# ---- Tests ----
test_that("SD1121: catches non-exception ARMCD with both AGE and AGETXT blank", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AGE, AGETXT",
    Original_Value = "",
    Rule_ID = "SD1121",
    Message = "At least one of AGE, AGETXT should be populated when ARMCD is not in ('SCRNFAIL','NOTASSGN') and is not missing.",
    stringsAsFactors = FALSE
  )
  res <- check_at_least_one_cond(
    df = dm_sd1121,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD1121",
    cond_vars = "ARMCD",
    cond_ops = "not_in",
    cond_vals = "SCRNFAIL,NOTASSGN,__MISSING__",
    logic_op = "AND",
    expected_val = NULL
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1121: returns NULL when non-exception ARMCD has AGE populated", {
  res <- check_at_least_one_cond(
    df = dm_sd1121_ok,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD1121",
    cond_vars = "ARMCD",
    cond_ops = "not_in",
    cond_vals = "SCRNFAIL,NOTASSGN,__MISSING__",
    logic_op = "AND",
    expected_val = NULL
  )
  expect_null(res)
})

test_that("SD1121: reports multiple error rows for non-exception arms", {
  expected_error <- data.frame(
    Row = "1, 2",
    Variable = "AGE, AGETXT",
    Original_Value = "",
    Rule_ID = "SD1121",
    Message = "At least one of AGE, AGETXT should be populated when ARMCD is not in ('SCRNFAIL','NOTASSGN') and is not missing.",
    stringsAsFactors = FALSE
  )
  res <- check_at_least_one_cond(
    df = dm_sd1121_multi_err,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    rule_id = "SD1121",
    cond_vars = "ARMCD",
    cond_ops = "not_in",
    cond_vals = "SCRNFAIL,NOTASSGN,__MISSING__",
    logic_op = "AND",
    expected_val = NULL
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Message, expected_error$Message)
})
