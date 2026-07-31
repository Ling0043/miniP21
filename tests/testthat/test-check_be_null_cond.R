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

# ---- Tests ----
test_that("SD1010: catches populated ELEMENT when ETCD='UNPLAN'", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ELEMENT",
    Original_Value = "Unplanned Element",
    Rule_ID = "SD1010",
    Message = "Value of ELEMENT must be null when ETCD='UNPLAN'",
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
    Original_Value = "10",
    Rule_ID = "SD1019",
    Message = "Value of VISITDY must be null when SVUPDES is provided",
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
    Original_Value = "2023-01-01",
    Rule_ID = "SD1238",
    Message = "Value of LBRFTDTC must be null when LBTPTREF is missing",
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
    Original_Value = "Invalid Value",
    Rule_ID = "SD1010",
    Message = "Value of ELEMENT must be null when ETCD='UNPLAN' and SEDECOD='UNPLANNED'",
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
    Original_Value = "Unscheduled Visit",
    Rule_ID = "SD1010",
    Message = "Value of ELEMENT must be null when ETCD='UNPLAN' or ETCD='UNSCHED'",
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
    Original_Value = "20",
    Rule_ID = "SD1066",
    Message = "Value of LBORRES must be null when LBSTAT is not in ('DONE') and is not missing",
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