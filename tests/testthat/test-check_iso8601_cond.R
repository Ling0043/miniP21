# Test for check_iso8601_cond using TS dataset ---------------------------

# 原始正确数据集
ts <- data.frame(
  STUDYID = c("XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ", "XYZ"),
  DOMAIN = c("TS", "TS", "TS", "TS", "TS", "TS", "TS", "TS", "TS", "TS", "TS", "TS"),
  TSSEQ = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  TSGRPID = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  TSPARMCD = c("ADDON", "AGEMAX", "AGEMIN", "LENGTH", "STYPE", "INTTYPE", "SSTDTC", "SENDTC", "ACTSUB", "HLTSUBJI", "SDMDUR", "CRMDUR"),
  TSPARM = c(
    "Added on to Existing Treatments",
    "Planned Maximum Age of Subjects",
    "Planned Minimum Age of Subjects",
    "Trial Length",
    "Study Type",
    "Intervention Type",
    "Study Start Date",
    "Study End Date",
    "Actual Number of Subjects",
    "Healthy Subject Indicator",
    "Stable Disease Minimum Duration",
    "Confirmed Response Minimum Duration"
  ),
  TSVAL = c("Y", "P70Y", "P18M", "P3M", "INTERVENTIONAL", "DRUG", "2009-03-11", "2011-04-01", "304", "N", "P3W", "P28D"),
  TSVALNF = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  TSVALCD = c("C49488", NA, NA, NA, "C98388", "C1909", NA, NA, NA, "C49487", NA, NA),
  TSVCDREF = c("CDISC CT", "ISO 8601", "ISO 8601", "ISO 8601", "CDISC CT", "CDISC CT", "ISO 8601", "ISO 8601", NA, "CDISC CT", "ISO 8601", "ISO 8601"),
  TSVCDVER = c("2011-06-10", NA, NA, NA, "2011-06-10", "2011-06-10", NA, NA, NA, "2011-06-10", NA, NA)
)

# 1. No error: all existing TSVAL values pass their format checks ----------
test_that("check_iso8601_cond returns NULL for all valid TSVAL in TS dataset", {
  # Duration rules: AGEMAX, AGEMIN, LENGTH
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "AGEMAX", type = "duration", rule_id = "SD1215")
  )
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "AGEMIN", type = "duration", rule_id = "SD1217")
  )
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "LENGTH", type = "duration", rule_id = "SD1219")
  )
  
  # Date rules: SSTDTC, SENDTC
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "SSTDTC", type = "date", rule_id = "SD2247")
  )
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "SENDTC", type = "date", rule_id = "SD2248")
  )
  # DCUTDTC not present -> target rows = 0 -> must return NULL
  expect_null(
    check_iso8601_cond(ts, target_vars = "TSVAL", cond_var = "TSPARMCD",
                       cond_val = "DCUTDTC", type = "date", rule_id = "SD2245")
  )
})

# 2. Error case 1: invalid duration for AGEMAX ----------
test_that("check_iso8601_cond catches invalid duration for TSPARMCD='AGEMAX'", {
  ts_bad_dur <- ts
  # error 1: let AGEMAX be "P70" (missing Y)
  ts_bad_dur$TSVAL[ts_bad_dur$TSPARMCD == "AGEMAX"] <- "P70"

  expected_error <- data.frame(
    Row = "2",
    Variable = "TSVAL",
    Original_Value = "P70",
    Rule_ID = "SD1215",
    Message = "Value must be ISO 8601 format when TSPARMCD='AGEMAX'"
  )

  res <- check_iso8601_cond(ts_bad_dur, target_vars = "TSVAL", cond_var = "TSPARMCD",
                            cond_val = "AGEMAX", type = "duration", rule_id = "SD1215")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# 3. Error case 2: invalid date for SSTDTC ----------
test_that("check_iso8601_cond catches invalid date for TSPARMCD='SSTDTC'", {
  ts_bad_date <- ts
  # error 2: let SSTDTC be "2009/03/11" (slashes instead of hyphens)
  ts_bad_date$TSVAL[ts_bad_date$TSPARMCD == "SSTDTC"] <- "2009/03/11"

  expected_error <- data.frame(
    Row = "7",
    Variable = "TSVAL",
    Original_Value = "2009/03/11",
    Rule_ID = "SD2247",
    Message = "Value must be ISO 8601 format when TSPARMCD='SSTDTC'",
    stringsAsFactors = FALSE
  )

  res <- check_iso8601_cond(ts_bad_date, target_vars = "TSVAL", cond_var = "TSPARMCD",
                            cond_val = "SSTDTC", type = "date", rule_id = "SD2247")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# 4. Error case 3: invalid date for DCUTDTC (requires adding a row) ----------
test_that("check_iso8601_cond catches invalid date for TSPARMCD='DCUTDTC'", {
  # error 3: add a row with TSPARMCD='DCUTDTC' and invalid date "2010-13-01" (13th month)
  ts_dcut <- rbind(ts,
    data.frame(
      STUDYID = "XYZ", DOMAIN = "TS", TSSEQ = 1, TSGRPID = NA,
      TSPARMCD = "DCUTDTC", TSPARM = "Data Cutoff Date",
      TSVAL = "2010-13-01", TSVALNF = NA, TSVALCD = NA,
      TSVCDREF = "ISO 8601", TSVCDVER = NA,
      stringsAsFactors = FALSE
    )
  )

  expected_error <- data.frame(
    Row = "13",   # 13
    Variable = "TSVAL",
    Original_Value = "2010-13-01",
    Rule_ID = "SD2245",
    Message = "Value must be ISO 8601 format when TSPARMCD='DCUTDTC'",
    stringsAsFactors = FALSE
  )
  
  res <- check_iso8601_cond(ts_dcut, target_vars = "TSVAL", cond_var = "TSPARMCD",
                            cond_val = "DCUTDTC", type = "date", rule_id = "SD2245")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})