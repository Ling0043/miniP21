# ---- Datasets ----
# SD0042: AE domain for --STAT condition
ae_0042 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AEPRESP = c("Y", "Y", "N"),
  AEOCCUR = c(NA, "N", NA),
  AESTAT = c("", "NOT DONE", ""),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD0090: AE domain for AESDTH condition
ae_0090 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AEOUT = c("FATAL", "RECOVERED", "FATAL"),
  AESDTH = c("N", "N", "Y"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1046: IE domain for IESTRESC condition
ie <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "IE",
  IESEQ = 1:3,
  IECAT = c("INCLUSION", "EXCLUSION", "INCLUSION"),
  IESTRESC = c("Y", "Y", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1314: DS domain for DSDECOD condition
ds <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  DSSEQ = 1:3,
  DSTERM = c("COMPLETED", "COMPLETED", "OTHER"),
  DSDECOD = c("OTHER", "COMPLETED", "OTHER"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD2241: TS domain for CURTRT
ts_2241 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("CURTRT", "TITLE", "CURTRT"),
  TSVCDREF = c("UNII", "NA", "WHO"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD2244: TS domain for FCNTRY
ts_2244 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("FCNTRY", "TITLE", "FCNTRY"),
  TSVCDREF = c("GENC", "NA", "OTHER"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1132: AE domain for AESER condition
ae_1132 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "AE",
  AESEQ = 1:3,
  AESER = c("N", "Y", "N"),
  AESCAN = c("N", "N", "N"),
  AESCONG = c("N", "N", "N"),
  AESDISAB = c("N", "N", "N"),
  AESDTH = c("N", "Y", "N"),
  AESHOSP = c("Y", "N", "N"),
  AESLIFE = c("N", "N", "N"),
  AESMIE = c("N", "N", "N"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary 1: Missing target_vars
ts_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("CURTRT", "CURTRT", "CURTRT"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary 2: Condition not met
ts_cond_not_met <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("TITLE", "TITLE", "TITLE"),
  TSVCDREF = c("UNII", "WHO", "OTHER"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD0042: --STAT does not equal 'NOT DONE' when --PRESP='Y' and --OCCUR is null
test_that("SD0042: catches AESTAT not 'NOT DONE' under conditions", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AESTAT",
    Original_Value = "",
    Rule_ID = "SD0042",
    Message = "Value of AESTAT must be 'NOT DONE' when AEPRESP='Y' and AEOCCUR is missing.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ae_0042,
    domain_name = "AE",
    target_vars = "--STAT",
    rule_id = "SD0042",
    expected_values = "NOT DONE",
    cond_vars = c("--PRESP", "--OCCUR"),
    cond_ops = c("equal", "missing"),
    cond_vals = c("Y", ""),
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

test_that("SD0042: returns NULL when no violation", {
  ae_ok <- ae_0042
  ae_ok$AESTAT[1] <- "NOT DONE"
  res <- check_expected_cond(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "--STAT",
    rule_id = "SD0042",
    expected_values = "NOT DONE",
    cond_vars = c("--PRESP", "--OCCUR"),
    cond_ops = c("equal", "missing"),
    cond_vals = c("Y", ""),
    logic_op = "AND"
  )
  expect_null(res)
})

# SD0090: AESDTH is not 'Y' when AEOUT='FATAL'
test_that("SD0090: catches AESDTH not 'Y' when AEOUT='FATAL'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AESDTH",
    Original_Value = "N",
    Rule_ID = "SD0090",
    Message = "Value of AESDTH must be 'Y' when AEOUT='FATAL'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ae_0090,
    domain_name = "AE",
    target_vars = "AESDTH",
    rule_id = "SD0090",
    expected_values = "Y",
    cond_vars = "AEOUT",
    cond_ops = "equal",
    cond_vals = "FATAL",
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

test_that("SD0090: returns NULL when no violation", {
  ae_ok <- ae_0090
  ae_ok$AESDTH[1] <- "Y"
  res <- check_expected_cond(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "AESDTH",
    rule_id = "SD0090",
    expected_values = "Y",
    cond_vars = "AEOUT",
    cond_ops = "equal",
    cond_vals = "FATAL",
    logic_op = "AND"
  )
  expect_null(res)
})

# SD1046: IESTRESC is not 'N' when IECAT ='INCLUSION'
test_that("SD1046: catches IESTRESC not 'N' when IECAT='INCLUSION'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "IESTRESC",
    Original_Value = "Y",
    Rule_ID = "SD1046",
    Message = "Value of IESTRESC must be 'N' when IECAT='INCLUSION'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ie,
    domain_name = "IE",
    target_vars = "IESTRESC",
    rule_id = "SD1046",
    expected_values = "N",
    cond_vars = "IECAT",
    cond_ops = "equal",
    cond_vals = "INCLUSION",
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

test_that("SD1046: returns NULL when no violation", {
  ie_ok <- ie
  ie_ok$IESTRESC[1] <- "N"
  res <- check_expected_cond(
    df = ie_ok,
    domain_name = "IE",
    target_vars = "IESTRESC",
    rule_id = "SD1046",
    expected_values = "N",
    cond_vars = "IECAT",
    cond_ops = "equal",
    cond_vals = "INCLUSION",
    logic_op = "AND"
  )
  expect_null(res)
})

# SD1314: DSDECOD is not 'COMPLETED' when DSTERM='COMPLETED'
test_that("SD1314: catches DSDECOD not 'COMPLETED' when DSTERM='COMPLETED'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DSDECOD",
    Original_Value = "OTHER",
    Rule_ID = "SD1314",
    Message = "Value of DSDECOD must be 'COMPLETED' when DSTERM='COMPLETED'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ds,
    domain_name = "DS",
    target_vars = "DSDECOD",
    rule_id = "SD1314",
    expected_values = "COMPLETED",
    cond_vars = "DSTERM",
    cond_ops = "equal",
    cond_vals = "COMPLETED",
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

test_that("SD1314: returns NULL when no violation", {
  ds_ok <- ds
  ds_ok$DSDECOD[1] <- "COMPLETED"
  res <- check_expected_cond(
    df = ds_ok,
    domain_name = "DS",
    target_vars = "DSDECOD",
    rule_id = "SD1314",
    expected_values = "COMPLETED",
    cond_vars = "DSTERM",
    cond_ops = "equal",
    cond_vals = "COMPLETED",
    logic_op = "AND"
  )
  expect_null(res)
})

# SD2241: Invalid TSVCDREF value for CURTRT
test_that("SD2241: catches invalid TSVCDREF when TSPARMCD='CURTRT'", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "TSVCDREF",
    Original_Value = "WHO",
    Rule_ID = "SD2241",
    Message = "Value of TSVCDREF must be 'UNII' when TSPARMCD='CURTRT'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ts_2241,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD2241",
    expected_values = "UNII",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "CURTRT",
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

test_that("SD2241: returns NULL when no violation", {
  ts_ok <- ts_2241
  ts_ok$TSVCDREF[3] <- "UNII"
  res <- check_expected_cond(
    df = ts_ok,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD2241",
    expected_values = "UNII",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "CURTRT",
    logic_op = "AND"
  )
  expect_null(res)
})

# SD2244: Invalid TSVCDREF value for FCNTRY
test_that("SD2244: catches invalid TSVCDREF when TSPARMCD='FCNTRY'", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "TSVCDREF",
    Original_Value = "OTHER",
    Rule_ID = "SD2244",
    Message = "Value of TSVCDREF must be 'GENC,ISO 3166-1 alpha-3' when TSPARMCD='FCNTRY'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ts_2244,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD2244",
    expected_values = "GENC,ISO 3166-1 alpha-3",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "FCNTRY",
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

test_that("SD2244: returns NULL when no violation", {
  ts_ok <- ts_2244
  ts_ok$TSVCDREF[3] <- "GENC"
  res <- check_expected_cond(
    df = ts_ok,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD2244",
    expected_values = "GENC,ISO 3166-1 alpha-3",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "FCNTRY",
    logic_op = "AND"
  )
  expect_null(res)
})

# SD1132: AESER is not 'Y'
test_that("SD1132: catches AESER not 'Y' when any seriousness criteria is 'Y'", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "AESER",
    Original_Value = "N",
    Rule_ID = "SD1132",
    Message = "Value of AESER must be 'Y' when AESCAN='Y' or AESCONG='Y' or AESDISAB='Y' or AESDTH='Y' or AESHOSP='Y' or AESLIFE='Y' or AESMIE='Y'.",
    stringsAsFactors = FALSE
  )
  
  res <- check_expected_cond(
    df = ae_1132,
    domain_name = "AE",
    target_vars = "AESER",
    rule_id = "SD1132",
    expected_values = "Y",
    cond_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    cond_ops = c("equal", "equal", "equal", "equal", "equal", "equal", "equal"),
    cond_vals = c("Y", "Y", "Y", "Y", "Y", "Y", "Y"),
    logic_op = "OR"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1132: returns NULL when no violation", {
  ae_ok <- ae_1132
  ae_ok$AESER[1] <- "Y"
  res <- check_expected_cond(
    df = ae_ok,
    domain_name = "AE",
    target_vars = "AESER",
    rule_id = "SD1132",
    expected_values = "Y",
    cond_vars = c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE"),
    cond_ops = c("equal", "equal", "equal", "equal", "equal", "equal", "equal"),
    cond_vals = c("Y", "Y", "Y", "Y", "Y", "Y", "Y"),
    logic_op = "OR"
  )
  expect_null(res)
})

# Boundary 1: Missing target_vars in df
test_that("Boundary: returns NULL when target_vars missing in df", {
  res <- check_expected_cond(
    df = ts_missing,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD_BOUND1",
    expected_values = "UNII",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "CURTRT",
    logic_op = "AND"
  )
  expect_null(res)
})

# Boundary 2: Condition not met
test_that("Boundary: returns NULL when condition is not met", {
  res <- check_expected_cond(
    df = ts_cond_not_met,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    rule_id = "SD_BOUND2",
    expected_values = "UNII",
    cond_vars = "TSPARMCD",
    cond_ops = "equal",
    cond_vals = "CURTRT",
    logic_op = "AND"
  )
  expect_null(res)
})
