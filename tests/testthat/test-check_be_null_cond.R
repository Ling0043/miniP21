# ---- Datasets ----
# SD1010: SE domain - ELEMENT must be null when ETCD='UNPLAN'
se_sd1010 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:3,
  ETCD = c("UNPLAN", "UNPLAN", "SCREEN"),
  ELEMENT = c("", "Unplanned Element", "Screening Period"),
  stringsAsFactors = FALSE
)

# SD1019: SV domain - VISITDY must be null when SVUPDES is provided
sv_sd1019 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  SVSEQ = 1:3,
  SVUPDES = c("Adverse Event", "Protocol Deviation", ""),
  VISITDY = c(NA_character_, "10", "5"),
  stringsAsFactors = FALSE
)

# SD1238: LB domain - --RFTDTC must be null when --TPTREF is missing (-- prefix test)
lb_sd1238 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBTPTREF = c(NA_character_, "", "PREDOSE"),
  LBRFTDTC = c("", "2023-01-01", "2023-01-02"),
  stringsAsFactors = FALSE
)

# Boundary: multi-condition AND logic
se_and_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:3,
  ETCD = c("UNPLAN", "UNPLAN", "UNPLAN"),
  ELEMENT = c("", "Invalid Value", ""),
  SEDECOD = c("UNPLANNED", "UNPLANNED", "OTHER"),
  stringsAsFactors = FALSE
)

# Boundary: multi-condition OR logic
se_or_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:3,
  ETCD = c("UNPLAN", "UNSCHED", "SCREEN"),
  ELEMENT = c("", "Unscheduled Visit", "Screening"),
  stringsAsFactors = FALSE
)

# Boundary: not_in operator with __MISSING__ token
lb_notin <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBSTAT = c("NOT DONE", "DONE", ""),
  LBORRES = c("20", "30", "40"),
  stringsAsFactors = FALSE
)

# Boundary: missing target column
df_missing_target <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:2,
  ETCD = c("UNPLAN", "SCREEN"),
  stringsAsFactors = FALSE
)

# Boundary: missing condition column
df_missing_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:2,
  ELEMENT = c("", "Test"),
  stringsAsFactors = FALSE
)

# Boundary: all conditions not met
se_all_cond_not_met <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  SESEQ = 1:2,
  ETCD = c("SCREEN", "TREATMENT"),
  ELEMENT = c("Screening", "Treatment"),
  stringsAsFactors = FALSE
)

# ---- Datasets ----
# SD1363: DM domain for ARMCD and ARM
dm_1363 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  ACTARMCD = c("SCRNFAIL", "NOTASSGN", "PLACEBO"),
  ARMCD = c("PLACEBO", NA, "PLACEBO"),
  ARM = c("Placebo", NA, "Placebo"),
  stringsAsFactors = FALSE
)

# SD1364: DM domain for ACTARMCD and ACTARM
dm_1364 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  ARMCD = c("SCRNFAIL", "NOTTRT", "PLACEBO"),
  ACTARMCD = c("PLACEBO", NA, "PLACEBO"),
  ACTARM = c("Placebo", NA, "Placebo"),
  stringsAsFactors = FALSE
)

# SD1375: DM domain for RFENDTC
dm_1375 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  ACTARMCD = c(NA, NA, "PLACEBO"),
  ARMNRS = c("OTHER", "UNPLANNED TREATMENT", "OTHER"),
  RFENDTC = c("2021-01-01", "2021-02-01", "2021-03-01"),
  stringsAsFactors = FALSE
)

# Boundary: Missing target_vars in df
dm_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1"),
  ACTARMCD = c("SCRNFAIL"),
  stringsAsFactors = FALSE
)



# ---- Tests ----
test_that("SD1010: catches populated ELEMENT when ETCD='UNPLAN'", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ELEMENT",
    Original_Value = " ",
    Rule_ID = "SD1010",
    Message = "Value of ( ELEMENT ) must be null when ETCD='UNPLAN'",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = se_sd1010,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    logic_op = "AND",
    rule_id = "SD1010"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1010: returns NULL when ELEMENT is null under met condition", {
  se_ok <- se_sd1010
  se_ok$ELEMENT[2] <- ""
  res <- check_be_null_cond(
    df = se_ok,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    rule_id = "SD1010"
  )
  expect_null(res)
})

test_that("SD1019: catches populated VISITDY when SVUPDES is provided", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "VISITDY",
    Original_Value = " ",
    Rule_ID = "SD1019",
    Message = "Value of ( VISITDY ) must be null when SVUPDES is provided",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = sv_sd1019,
    domain_name = "SV",
    target_vars = "VISITDY",
    cond_vars = "SVUPDES",
    cond_ops = "non_missing",
    cond_vals = "",
    logic_op = "AND",
    rule_id = "SD1019"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1019: returns NULL when VISITDY is null under met condition", {
  sv_ok <- sv_sd1019
  sv_ok$VISITDY[2] <- NA_character_
  res <- check_be_null_cond(
    df = sv_ok,
    domain_name = "SV",
    target_vars = "VISITDY",
    cond_vars = "SVUPDES",
    cond_ops = "non_missing",
    cond_vals = "",
    rule_id = "SD1019"
  )
  expect_null(res)
})

test_that("SD1238: catches populated --RFTDTC when --TPTREF is missing", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBRFTDTC",
    Original_Value = " ",
    Rule_ID = "SD1238",
    Message = "Value of ( LBRFTDTC ) must be null when LBTPTREF is missing",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = lb_sd1238,
    domain_name = "LB",
    target_vars = "--RFTDTC",
    cond_vars = "--TPTREF",
    cond_ops = "missing",
    cond_vals = "",
    logic_op = "AND",
    rule_id = "SD1238"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1238: returns NULL when --RFTDTC is null under met condition", {
  lb_ok <- lb_sd1238
  lb_ok$LBRFTDTC[2] <- ""
  res <- check_be_null_cond(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--RFTDTC",
    cond_vars = "--TPTREF",
    cond_ops = "missing",
    cond_vals = "",
    rule_id = "SD1238"
  )
  expect_null(res)
})

test_that("Boundary: validates multi-condition AND logic correctly", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ELEMENT",
    Original_Value = " ",
    Rule_ID = "SD1010",
    Message = "Value of ( ELEMENT ) must be null when ETCD='UNPLAN' and SEDECOD='UNPLANNED'",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = se_and_cond,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = c("ETCD", "SEDECOD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("UNPLAN", "UNPLANNED"),
    logic_op = "AND",
    rule_id = "SD1010"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("Boundary: validates multi-condition OR logic correctly", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ELEMENT",
    Original_Value = " ",
    Rule_ID = "SD1010",
    Message = "Value of ( ELEMENT ) must be null when ETCD='UNPLAN' or ETCD='UNSCHED'",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = se_or_cond,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = c("ETCD", "ETCD"),
    cond_ops = c("equal", "equal"),
    cond_vals = c("UNPLAN", "UNSCHED"),
    logic_op = "OR",
    rule_id = "SD1010"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("Boundary: validates not_in operator with __MISSING__ token", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBORRES",
    Original_Value = " ",
    Rule_ID = "SD1066",
    Message = "Value of ( LBORRES ) must be null when LBSTAT is not in ('DONE') and is not missing",
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = lb_notin,
    domain_name = "LB",
    target_vars = "--ORRES",
    cond_vars = "--STAT",
    cond_ops = "not_in",
    cond_vals = "DONE,__MISSING__",
    logic_op = "AND",
    rule_id = "SD1066"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("Boundary: returns NULL when target variable is missing from df", {
  res <- check_be_null_cond(
    df = df_missing_target,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    rule_id = "SD1010"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when condition variable is missing from df", {
  res <- check_be_null_cond(
    df = df_missing_cond,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    rule_id = "SD1010"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when all conditions are not met", {
  res <- check_be_null_cond(
    df = se_all_cond_not_met,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    rule_id = "SD1010"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when target is NA under met condition", {
  se_na_target <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "SE",
    SESEQ = 1,
    ETCD = "UNPLAN",
    ELEMENT = NA_character_,
    stringsAsFactors = FALSE
  )
  res <- check_be_null_cond(
    df = se_na_target,
    domain_name = "SE",
    target_vars = "ELEMENT",
    cond_vars = "ETCD",
    cond_ops = "equal",
    cond_vals = "UNPLAN",
    rule_id = "SD1010"
  )
  expect_null(res)
})



# ---- Tests ----
# SD1363: ARMCD/ARM is populated for a subject not assigned to treatment
test_that("SD1363: catches ARMCD/ARM populated for unassigned subject", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "ARMCD, ARM",
    Original_Value = " ",
    Rule_ID = "SD1363",
    Message = "Value of ( ARMCD, ARM ) must be null when ACTARMCD='SCRNFAIL' or ACTARMCD='NOTASSGN'",
    stringsAsFactors = FALSE
  )
  
  res <- check_be_null_cond(
    df = dm_1363,
    domain_name = "DM",
    target_vars = c("ARMCD", "ARM"),
    cond_vars = "ACTARMCD",
    cond_ops = "equal",
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "OR",
    rule_id = "SD1363"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1363: returns NULL when no violation", {
  dm_ok <- dm_1363
  dm_ok$ARMCD[1] <- NA
  dm_ok$ARM[1] <- NA
  res <- check_be_null_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = c("ARMCD", "ARM"),
    cond_vars = "ACTARMCD",
    cond_ops = "equal",
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "OR",
    rule_id = "SD1363"
  )
  expect_null(res)
})

# SD1364: ACTARMCD/ACTARM is populated for a subject who wasn't treated
test_that("SD1364: catches ACTARMCD/ACTARM populated for untreated subject", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "ACTARMCD, ACTARM",
    Original_Value = " ",
    Rule_ID = "SD1364",
    Message = "Value of ( ACTARMCD, ACTARM ) must be null when ARMCD='SCRNFAIL' or ARMCD='NOTASSGN' or ARMCD='NOTTRT'",
    stringsAsFactors = FALSE
  )
  
  res <- check_be_null_cond(
    df = dm_1364,
    domain_name = "DM",
    target_vars = c("ACTARMCD", "ACTARM"),
    cond_vars = "ARMCD",
    cond_ops = "equal",
    cond_vals = c("SCRNFAIL", "NOTASSGN", "NOTTRT"),
    logic_op = "OR",
    rule_id = "SD1364"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1364: returns NULL when no violation", {
  dm_ok <- dm_1364
  dm_ok$ACTARMCD[1] <- NA
  dm_ok$ACTARM[1] <- NA
  res <- check_be_null_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = c("ACTARMCD", "ACTARM"),
    cond_vars = "ARMCD",
    cond_ops = "equal",
    cond_vals = c("SCRNFAIL", "NOTASSGN", "NOTTRT"),
    logic_op = "OR",
    rule_id = "SD1364"
  )
  expect_null(res)
})

# SD1375: RFENDTC is populated for subject who did not receive treatment
test_that("SD1375: catches RFENDTC populated when ACTARMCD is missing and ARMNRS is not in exclusion", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RFENDTC",
    Original_Value = " ",
    Rule_ID = "SD1375",
    Message = "Value of ( RFENDTC ) must be null when ACTARMCD is missing and ARMNRS is not in ('UNPLANNED TREATMENT')",
    stringsAsFactors = FALSE
  )
  
  res <- check_be_null_cond(
    df = dm_1375,
    domain_name = "DM",
    target_vars = "RFENDTC",
    cond_vars = c("ACTARMCD", "ARMNRS"),
    cond_ops = c("missing", "not_in"),
    cond_vals = c("", "UNPLANNED TREATMENT"),
    logic_op = "AND",
    rule_id = "SD1375"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1375: returns NULL when no violation", {
  dm_ok <- dm_1375
  dm_ok$RFENDTC[1] <- NA
  res <- check_be_null_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "RFENDTC",
    cond_vars = c("ACTARMCD", "ARMNRS"),
    cond_ops = c("missing", "not_in"),
    cond_vals = c("", "UNPLANNED TREATMENT"),
    logic_op = "AND",
    rule_id = "SD1375"
  )
  expect_null(res)
})

# Boundary: Missing target_vars in df
test_that("Boundary: returns NULL when target_vars missing in df", {
  res <- check_be_null_cond(
    df = dm_missing,
    domain_name = "DM",
    target_vars = c("ARMCD", "ARM"),
    cond_vars = "ACTARMCD",
    cond_ops = "equal",
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "OR",
    rule_id = "SD_BOUND1"
  )
  expect_null(res)
})