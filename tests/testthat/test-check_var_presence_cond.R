# ---- Datasets ----
# SD1091: FINDINGS domain (e.g., LB) missing LBENDY when LBENDTC is present
lb_missing_endy <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBENDTC = c("2021-01-01", "2021-01-02", "2021-01-03"),
  stringsAsFactors = FALSE
)

# SD1129: DM domain missing both AGE and AGETXT
dm_missing_age <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  USUBJID = c("SUB1", "SUB2", "SUB3"),
  SEX = c("M", "F", "M"),
  stringsAsFactors = FALSE
)

# SD1293: EVENTS domain (e.g., DS) with DSREASND when DSPRESP is missing
ds_reasnd_no_presp <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DS",
  DSSEQ = 1:3,
  DSREASND = c("Adverse Event", "Lost to follow up", "Other"),
  stringsAsFactors = FALSE
)

# SD1299: INTERVENTIONS domain (e.g., EX) with no timing variables
ex_no_timing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:3,
  EXTRT = c("Drug A", "Drug B", "Drug C"),
  EXDOSE = c(10, 20, 30),
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1091: Missing --ENDY variable when --ENDTC variable is present
test_that("SD1091: catches missing LBENDY when LBENDTC is present", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "LBENDY",
    Original_Value = NA_character_,
    Rule_ID = "SD1091",
    Message = "Variable presence logic violation for: LBENDY",
    stringsAsFactors = FALSE
  )
  
  res <- check_var_presence_cond(
    df = lb_missing_endy,
    domain_name = "LB",
    target_vars = "--ENDY",
    cond_vars = "--ENDTC",
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "all",
    rule_id = "SD1091"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1091: returns NULL when no violation", {
  lb_ok <- lb_missing_endy
  lb_ok$LBENDY <- c(1, 2, 3)
  res <- check_var_presence_cond(
    df = lb_ok,
    domain_name = "LB",
    target_vars = "--ENDY",
    cond_vars = "--ENDTC",
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "all",
    rule_id = "SD1091"
  )
  expect_null(res)
})

# SD1129: Neither AGE nor AGETXT variables are present
test_that("SD1129: catches missing AGE and AGETXT in DM", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "AGE, AGETXT",
    Original_Value = NA_character_,
    Rule_ID = "SD1129",
    Message = "Variable presence logic violation for: AGE, AGETXT",
    stringsAsFactors = FALSE
  )
  
  res <- check_var_presence_cond(
    df = dm_missing_age,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    cond_vars = character(0),
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "any",
    rule_id = "SD1129"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1129: returns NULL when no violation", {
  dm_ok <- dm_missing_age
  dm_ok$AGE <- c(25, 30, 40)
  res <- check_var_presence_cond(
    df = dm_ok,
    domain_name = "DM",
    target_vars = c("AGE", "AGETXT"),
    cond_vars = character(0),
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "any",
    rule_id = "SD1129"
  )
  expect_null(res)
})

# SD1293: --REASND variable is present when --PRESP variable is missing
test_that("SD1293: catches DSREASND present when DSPRESP is missing", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "DSREASND",
    Original_Value = NA_character_,
    Rule_ID = "SD1293",
    Message = "Variable presence logic violation for: DSREASND",
    stringsAsFactors = FALSE
  )
  
  res <- check_var_presence_cond(
    df = ds_reasnd_no_presp,
    domain_name = "DS",
    target_vars = "--REASND",
    cond_vars = "--PRESP",
    cond_presence = FALSE,
    cond_type = "all",
    target_presence = FALSE,
    target_type = "all",
    rule_id = "SD1293"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1293: returns NULL when no violation", {
  ds_ok <- ds_reasnd_no_presp
  ds_ok$DSREASND <- NULL
  res <- check_var_presence_cond(
    df = ds_ok,
    domain_name = "DS",
    target_vars = "--REASND",
    cond_vars = "--PRESP",
    cond_presence = FALSE,
    cond_type = "all",
    target_presence = FALSE,
    target_type = "all",
    rule_id = "SD1293"
  )
  expect_null(res)
})

# SD1299: No timing variables are present in dataset
test_that("SD1299: catches no timing variables present in EX", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "EXDTC, EXDY, EXSTDTC, EXENDTC",
    Original_Value = NA_character_,
    Rule_ID = "SD1299",
    Message = "Variable presence logic violation for: EXDTC, EXDY, EXSTDTC, EXENDTC",
    stringsAsFactors = FALSE
  )
  
  res <- check_var_presence_cond(
    df = ex_no_timing,
    domain_name = "EX",
    target_vars = c("--DTC", "--DY", "--STDTC", "--ENDTC"),
    cond_vars = character(0),
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "any",
    rule_id = "SD1299"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1299: returns NULL when no violation", {
  ex_ok <- ex_no_timing
  ex_ok$EXSTDTC <- c("2021-01-01", "2021-01-02", "2021-01-03")
  res <- check_var_presence_cond(
    df = ex_ok,
    domain_name = "EX",
    target_vars = c("--DTC", "--DY", "--STDTC", "--ENDTC"),
    cond_vars = character(0),
    cond_presence = TRUE,
    cond_type = "all",
    target_presence = TRUE,
    target_type = "any",
    rule_id = "SD1299"
  )
  expect_null(res)
})
