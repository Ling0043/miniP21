
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