
# ---- Datasets ----

# SD0016: FINDINGS domain (e.g., LB)
lb <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBTESTCD = c("TEST1", "TEST2", "TEST3"),
  LBTEST = c("Test 1", "Test 2", "Test 3"),
  LBSTRESC = c("100", NA, "200"),
  LBDRVFL = c("", "Y", "Y"),
  stringsAsFactors = FALSE
)

# SD0024: FINDINGS domain with date variables
lb_dates <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBDTC = c(NA, "2021-05-10", "2021-06-15"),
  LBENDTC = c("2021-05-10", NA, "2021-06-15"),
  stringsAsFactors = FALSE
)

# SD0035: INTERVENTIONS domain (e.g., EX)
ex <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:3,
  EXDOSU = c("", "mg", "mL"),
  EXDOSE = c("10", "", ""),
  EXDOSTXT = c("", "two tablets", ""),
  EXDOSTOT = c("", "", "30"),
  stringsAsFactors = FALSE
)

# SD1268: TS domain
ts <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSVCDREF = c(NA, NA, "ICD-10"),
  TSVCDVER = c("1.0", "", "2.0"),
  stringsAsFactors = FALSE
)

# ---- Datasets ----
# SD1035: DS domain, DSCAT must be populated (no condition)
ds_sd1035 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  DSSEQ = 1:3,
  DSCAT = c("", "ADVERSE EVENT", "COMPLETION"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1210: DM domain, RFICDTC must be populated (no condition)
dm_sd1210 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:3,
  RFICDTC = c(NA_character_, "2023-01-05", "2023-02-10"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1234: DI domain, DEVTYPE must be populated (no condition)
di_sd1234 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DI",
  DISEQ = 1:3,
  DEVTYPE = c("", "IMPLANT", "MONITOR"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1368: SM domain, SMSTDTC must be populated (no condition)
sm_sd1368 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SM",
  SMSEQ = 1:3,
  SMSTDTC = c("", "2023-03-01", "2023-04-01"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1476: DM domain, RACE must be populated (no condition)
dm_sd1476 <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:3,
  RACE = c(NA_character_, "WHITE", "BLACK OR AFRICAN AMERICAN"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: missing target variable column
df_missing_target_col <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  DSSEQ = 1:2,
  DSTERM = c("COMPLETED", "DISCONTINUED"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: multiple violation rows
df_multi_violate <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  DMSEQ = 1:3,
  RACE = c("", NA_character_, "ASIAN"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----

# SD0016: Missing --STRESC when --DRVFL='Y'
test_that("SD0016: catches missing LBSTRESC when LBDRVFL='Y'", {
  # domain_name = "LB", target = "--STRESC", cond = "--DRVFL"
  expected_error <- data.frame(
    Row = "2",
    Variable = "LBSTRESC",
    Original_Value = NA_character_,
    Rule_ID = "SD0016",
    Message = "Value for LBSTRESC must not be null when LBDRVFL='Y'",
    stringsAsFactors = FALSE
  )

  res <- check_missing_cond(
    df = lb,
    domain_name = "LB",
    target_vars = "--STRESC",
    cond_vars = "--DRVFL",
    cond_ops = "equal",
    cond_vals = "Y",
    logic_op = "AND",
    rule_id = "SD0016"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0016: returns NULL when no violation", {
  # Modify dataset: all STRESC non-missing when DRVFL='Y'
  lb_ok <- lb
  lb_ok$LBSTRESC[2] <- "150"

  res <- check_missing_cond(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--STRESC",
    cond_vars = "--DRVFL",
    cond_ops = "equal",
    cond_vals = "Y",
    rule_id = "SD0016"
  )

  expect_null(res)
})

# SD0024: Missing --DTC when --ENDTC is provided
test_that("SD0024: catches missing LBDTC when LBENDTC is provided", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBDTC",
    Original_Value = NA_character_,
    Rule_ID = "SD0024",
    Message = "Value for LBDTC must not be null when LBENDTC is provided",
    stringsAsFactors = FALSE
  )

  res <- check_missing_cond(
    df = lb_dates,
    domain_name = "LB",
    target_vars = "--DTC",
    cond_vars = "--ENDTC",
    cond_ops = "non_missing",
    cond_vals = "",
    logic_op = "AND",
    rule_id = "SD0024"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0024: returns NULL when no violation", {
  lb_dates_ok <- lb_dates
  lb_dates_ok$LBDTC[1] <- "2021-05-10"

  res <- check_missing_cond(
    df = lb_dates_ok,
    domain_name = "LB",
    target_vars = "--DTC",
    cond_vars = "--ENDTC",
    cond_ops = "non_missing",
    cond_vals = "",
    rule_id = "SD0024"
  )

  expect_null(res)
})

# SD0035: Missing --DOSU when --DOSE, --DOSTXT or --DOSTOT is provided
test_that("SD0035: catches missing EXDOSU when any of EXDOSE/EXDOSTXT/EXDOSTOT is provided", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXDOSU",
    Original_Value = "",
    Rule_ID = "SD0035",
    Message = "Value for EXDOSU must not be null when EXDOSE is provided or EXDOSTXT is provided or EXDOSTOT is provided",
    stringsAsFactors = FALSE
  )

  res <- check_missing_cond(
    df = ex,
    domain_name = "EX",
    target_vars = "--DOSU",
    cond_vars = c("--DOSE", "--DOSTXT", "--DOSTOT"),
    cond_ops = c("non_missing", "non_missing", "non_missing"),
    cond_vals = c("", "", ""),
    logic_op = "OR",
    rule_id = "SD0035"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD0035: returns NULL when no violation", {
  ex_ok <- ex
  ex_ok$EXDOSU[1] <- "mg"

  res <- check_missing_cond(
    df = ex_ok,
    domain_name = "EX",
    target_vars = "--DOSU",
    cond_vars = c("--DOSE", "--DOSTXT", "--DOSTOT"),
    cond_ops = c("non_missing", "non_missing", "non_missing"),
    cond_vals = c("", "", ""),
    logic_op = "OR",
    rule_id = "SD0035"
  )

  expect_null(res)
})

# SD1268: TSVCDREF is null when TSVCDVER is populated
test_that("SD1268: catches missing TSVCDREF when TSVCDVER is provided", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "TSVCDREF",
    Original_Value = NA_character_,
    Rule_ID = "SD1268",
    Message = "Value for TSVCDREF must not be null when TSVCDVER is provided",
    stringsAsFactors = FALSE
  )

  res <- check_missing_cond(
    df = ts,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    cond_vars = "TSVCDVER",
    cond_ops = "non_missing",
    cond_vals = "",
    logic_op = "AND",
    rule_id = "SD1268"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1268: returns NULL when no violation", {
  ts_ok <- ts
  ts_ok$TSVCDREF[1] <- "SNOMED"

  res <- check_missing_cond(
    df = ts_ok,
    domain_name = "TS",
    target_vars = "TSVCDREF",
    cond_vars = "TSVCDVER",
    cond_ops = "non_missing",
    cond_vals = "",
    rule_id = "SD1268"
  )

  expect_null(res)
})

# ---- Tests ----
test_that("SD1035: catches missing DSCAT (unconditional check)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DSCAT",
    Original_Value = "",
    Rule_ID = "SD1035",
    Message = "Value for DSCAT must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = ds_sd1035,
    domain_name = "DS",
    target_vars = "--CAT",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1035"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1035: returns NULL when DSCAT is populated", {
  ds_ok <- ds_sd1035[-1, ]
  res <- check_missing_cond(
    df = ds_ok,
    domain_name = "DS",
    target_vars = "--CAT",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1035"
  )
  expect_null(res)
})

test_that("SD1210: catches missing RFICDTC (unconditional check)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RFICDTC",
    Original_Value = NA_character_,
    Rule_ID = "SD1210",
    Message = "Value for RFICDTC must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = dm_sd1210,
    domain_name = "DM",
    target_vars = "RFICDTC",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1210"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1210: returns NULL when RFICDTC is populated", {
  dm_ok <- dm_sd1210[-1, ]
  res <- check_missing_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "RFICDTC",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1210"
  )
  expect_null(res)
})

test_that("SD1234: catches missing DEVTYPE (unconditional check)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "DEVTYPE",
    Original_Value = "",
    Rule_ID = "SD1234",
    Message = "Value for DEVTYPE must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = di_sd1234,
    domain_name = "DI",
    target_vars = "DEVTYPE",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1234"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1234: returns NULL when DEVTYPE is populated", {
  di_ok <- di_sd1234[-1, ]
  res <- check_missing_cond(
    df = di_ok,
    domain_name = "DI",
    target_vars = "DEVTYPE",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1234"
  )
  expect_null(res)
})

test_that("SD1368: catches missing SMSTDTC (unconditional check)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "SMSTDTC",
    Original_Value = "",
    Rule_ID = "SD1368",
    Message = "Value for SMSTDTC must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = sm_sd1368,
    domain_name = "SM",
    target_vars = "--STDTC",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1368"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1368: returns NULL when SMSTDTC is populated", {
  sm_ok <- sm_sd1368[-1, ]
  res <- check_missing_cond(
    df = sm_ok,
    domain_name = "SM",
    target_vars = "--STDTC",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1368"
  )
  expect_null(res)
})

test_that("SD1476: catches missing RACE (unconditional check)", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "RACE",
    Original_Value = NA_character_,
    Rule_ID = "SD1476",
    Message = "Value for RACE must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = dm_sd1476,
    domain_name = "DM",
    target_vars = "RACE",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1476"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1476: returns NULL when RACE is populated", {
  dm_ok <- dm_sd1476[-1, ]
  res <- check_missing_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = "RACE",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1476"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when target variable column is missing", {
  res <- check_missing_cond(
    df = df_missing_target_col,
    domain_name = "DS",
    target_vars = "--CAT",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1035"
  )
  expect_null(res)
})

test_that("Boundary: correctly report multiple missing rows", {
  expected_error <- data.frame(
    Row = c("1","2"),
    Variable = "RACE",
    Original_Value = c("", NA_character_),
    Rule_ID = "SD1476",
    Message = "Value for RACE must not be null.",
    stringsAsFactors = FALSE
  )
  res <- check_missing_cond(
    df = df_multi_violate,
    domain_name = "DM",
    target_vars = "RACE",
    cond_vars = NULL,
    cond_ops = NULL,
    cond_vals = NULL,
    rule_id = "SD1476"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
