# ---- Datasets ----
# SD2235: HLTSUBJI unconditional required parameter
ts_sd2235_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("STYPE", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2235_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "HLTSUBJI", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "Y", "Study Title"),
  stringsAsFactors = FALSE
)

# SD2280: SDTIGVER unconditional required parameter
ts_sd2280_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("STYPE", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2280_empty <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "SDTIGVER", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2280_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "SDTIGVER", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "3.3", "Study Title"),
  stringsAsFactors = FALSE
)

# SD2228: INTMODEL required when STYPE = INTERVENTIONAL
ts_sd2228_cond_met_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("STYPE", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2228_cond_met_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "INTMODEL", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "PARALLEL", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2228_cond_not_met <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("STYPE", "TITLE"),
  TSVAL = c("OBSERVATIONAL", "Study Title"),
  stringsAsFactors = FALSE
)

# SD2223: PCLAS required when STYPE=INTERVENTIONAL and INTTYPE in applicable values
ts_sd2223_cond_met_missing <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "INTTYPE", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "DRUG", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2223_cond_inttype_mismatch <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:3,
  TSPARMCD = c("STYPE", "INTTYPE", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "DEVICE", "Study Title"),
  stringsAsFactors = FALSE
)

ts_sd2223_cond_met_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:4,
  TSPARMCD = c("STYPE", "INTTYPE", "PCLAS", "TITLE"),
  TSVAL = c("INTERVENTIONAL", "DRUG", "ANTIVIRAL", "Study Title"),
  stringsAsFactors = FALSE
)

# Boundary: missing required value column
ts_missing_valcol <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("STYPE", "TITLE"),
  stringsAsFactors = FALSE
)

# Boundary: condition parameter code does not exist in dataset
ts_no_cond_param <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("TITLE", "SDTIGVER"),
  TSVAL = c("Study Title", "3.3"),
  stringsAsFactors = FALSE
)

# Boundary: parameter exists but all values are NA
ts_param_all_na <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TS",
  TSSEQ = 1:2,
  TSPARMCD = c("HLTSUBJI", "HLTSUBJI"),
  TSVAL = c(NA_character_, NA_character_),
  stringsAsFactors = FALSE
)

# ---- Tests ----
test_that("SD2235: catches missing unconditional required HLTSUBJI parameter", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2235",
    Message = "Missing required TSPARMCD parameter 'HLTSUBJI' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_sd2235_missing,
    target_vars = "TSPARMCD",
    param_code = "HLTSUBJI",
    rule_id = "SD2235"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2235: returns NULL when HLTSUBJI parameter is populated", {
  res <- check_ts_param_required(
    df = ts_sd2235_ok,
    target_vars = "TSPARMCD",
    param_code = "HLTSUBJI",
    rule_id = "SD2235"
  )
  expect_null(res)
})

test_that("SD2280: catches missing unconditional required SDTIGVER parameter", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2280",
    Message = "Missing required TSPARMCD parameter 'SDTIGVER' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_sd2280_missing,
    target_vars = "TSPARMCD",
    param_code = "SDTIGVER",
    rule_id = "SD2280"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2280: catches SDTIGVER exists but value is empty string", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2280",
    Message = "Missing required TSPARMCD parameter 'SDTIGVER' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_sd2280_empty,
    target_vars = "TSPARMCD",
    param_code = "SDTIGVER",
    rule_id = "SD2280"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2280: returns NULL when SDTIGVER parameter has valid value", {
  res <- check_ts_param_required(
    df = ts_sd2280_ok,
    target_vars = "TSPARMCD",
    param_code = "SDTIGVER",
    rule_id = "SD2280"
  )
  expect_null(res)
})

test_that("SD2228: catches missing INTMODEL when STYPE equals INTERVENTIONAL", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2228",
    Message = "Missing required TSPARMCD parameter 'INTMODEL' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_sd2228_cond_met_missing,
    target_vars = "TSPARMCD",
    param_code = "INTMODEL",
    cond_param = "STYPE",
    cond_val = "INTERVENTIONAL",
    rule_id = "SD2228"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2228: returns NULL when INTMODEL is populated under met condition", {
  res <- check_ts_param_required(
    df = ts_sd2228_cond_met_ok,
    target_vars = "TSPARMCD",
    param_code = "INTMODEL",
    cond_param = "STYPE",
    cond_val = "INTERVENTIONAL",
    rule_id = "SD2228"
  )
  expect_null(res)
})

test_that("SD2228: returns NULL when STYPE does not match condition value", {
  res <- check_ts_param_required(
    df = ts_sd2228_cond_not_met,
    target_vars = "TSPARMCD",
    param_code = "INTMODEL",
    cond_param = "STYPE",
    cond_val = "INTERVENTIONAL",
    rule_id = "SD2228"
  )
  expect_null(res)
})

test_that("SD2223: catches missing PCLAS when INTTYPE is in applicable value list", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2223",
    Message = "Missing required TSPARMCD parameter 'PCLAS' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_sd2223_cond_met_missing,
    target_vars = "TSPARMCD",
    param_code = "PCLAS",
    cond_param = "INTTYPE",
    cond_val = c("DRUG", "BIOLOGICAL"),
    rule_id = "SD2223"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD2223: returns NULL when INTTYPE is not in applicable value list", {
  res <- check_ts_param_required(
    df = ts_sd2223_cond_inttype_mismatch,
    target_vars = "TSPARMCD",
    param_code = "PCLAS",
    cond_param = "INTTYPE",
    cond_val = c("DRUG", "BIOLOGICAL"),
    rule_id = "SD2223"
  )
  expect_null(res)
})

test_that("SD2223: returns NULL when PCLAS is populated under met conditions", {
  res <- check_ts_param_required(
    df = ts_sd2223_cond_met_ok,
    target_vars = "TSPARMCD",
    param_code = "PCLAS",
    cond_param = "INTTYPE",
    cond_val = c("DRUG", "BIOLOGICAL"),
    rule_id = "SD2223"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when value column is missing from dataset", {
  res <- check_ts_param_required(
    df = ts_missing_valcol,
    target_vars = "TSPARMCD",
    param_code = "HLTSUBJI",
    rule_id = "SD2235"
  )
  expect_null(res)
})

test_that("Boundary: returns NULL when condition parameter code does not exist", {
  res <- check_ts_param_required(
    df = ts_no_cond_param,
    target_vars = "TSPARMCD",
    param_code = "INTMODEL",
    cond_param = "STYPE",
    cond_val = "INTERVENTIONAL",
    rule_id = "SD2228"
  )
  expect_null(res)
})

test_that("Boundary: catches parameter exists but all values are NA", {
  expected_error <- data.frame(
    Row = NA_character_,
    Variable = "TSPARMCD",
    Original_Value = NA_character_,
    Rule_ID = "SD2235",
    Message = "Missing required TSPARMCD parameter 'HLTSUBJI' in TS domain",
    stringsAsFactors = FALSE
  )
  res <- check_ts_param_required(
    df = ts_param_all_na,
    target_vars = "TSPARMCD",
    param_code = "HLTSUBJI",
    rule_id = "SD2235"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})