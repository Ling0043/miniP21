# Test for check_date_logic ----------------------------------------------

# 1. No error: all dates satisfy the comparison ----------
test_that("check_date_logic returns NULL when all dates pass (RFSTDTC <= RFENDTC)", {
  # 1.a correct DM dataframe (exactly as provided)
  dm <- data.frame(
    Row = 1:6,
    STUDYID = c("ABC123", "ABC123", "ABC123", "ABC123", "ABC123", "ABC123"),
    DOMAIN = c("DM", "DM", "DM", "DM", "DM", "DM"),
    USUBJID = c("ABC12301001", "ABC12301002", "ABC12301003", "ABC12301004", "ABC12302001", "ABC12302002"),
    SUBJID = c("01001", "01002", "01003", "01004", "02001", "02002"),
    RFSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-02-03"),
    RFENDTC = c("2006-03-10", "2006-02-28", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    RFXSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-02-03"),
    RFXENDTC = c("2006-03-10", "2006-02-28", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    RFICDTC = c("2006-01-03", "2006-01-04", "2006-01-02", "2006-01-07", "2006-01-15", "2006-01-10"),
    RFPENDTC = c("2006-04-01", "2006-03-26", "2006-03-19", "2006-01-08", "2006-04-12", "2006-04-25"),
    SITEID = c("01", "01", "01", "01", "02", "02"),
    INVNAM = c("JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "GONZALEZ, E", "GONZALEZ, E"),
    BRTHDTC = c("1948-12-13", "1955-03-22", "1938-01-19", "1941-07-02", "1950-06-23", "1956-05-05"),
    AGE = c(57, 50, 68, NA, 55, 49),
    AGEU = c("YEARS", "YEARS", "YEARS", "", "YEARS", "YEARS"),
    SEX = c("M", "M", "F", "M", "F", "F"),
    RACE = c(
      "WHITE",
      "WHITE",
      "BLACK OR AFRICAN AMERICAN",
      "ASIAN",
      "AMERICAN INDIAN OR ALASKA NATIVE",
      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDERS"
    ),
    ETHNIC = c(
      "HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO"
    ),
    ARMCD = c("A", "P", "P", "", "P", "A"),
    ARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ACTARMCD = c("A", "P", "P", "", "P", "A"),
    ACTARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ARMREAS = c("", "", "", "SCREENING FAILURE", "", ""),
    ACTARMULD = c("", "", "", "", "", ""),
    COUNTRY = c("USA", "USA", "USA", "USA", "USA", "USA")
  )

  # 2. Run test: RFSTDTC must be <= RFENDTC
  res_clean <- check_date_logic(
    dm,
    domain_name = "DM",
    target_vars = c("--STDTC", "--ENDTC"),
    rule_id = "SD0013",
    operator = "<="
  )
  expect_null(res_clean)
})

# 2. Error case 1: RFSTDTC > RFENDTC ----------
test_that("check_date_logic detects RFSTDTC > RFENDTC", {
  dm <- data.frame(
    Row = 1:6,
    STUDYID = c("ABC123", "ABC123", "ABC123", "ABC123", "ABC123", "ABC123"),
    DOMAIN = c("DM", "DM", "DM", "DM", "DM", "DM"),
    USUBJID = c("ABC12301001", "ABC12301002", "ABC12301003", "ABC12301004", "ABC12302001", "ABC12302002"),
    SUBJID = c("01001", "01002", "01003", "01004", "02001", "02002"),
    # Row 2: RFSTDTC = "2006-01-15", RFENDTC changed to "2006-01-14" -> error
    RFSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-06-03"),
    RFENDTC = c("2006-03-10", "2006-01-14", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    RFXSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-02-03"),
    RFXENDTC = c("2006-03-10", "2006-02-28", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    RFICDTC = c("2006-01-03", "2006-01-04", "2006-01-02", "2006-01-07", "2006-01-15", "2006-01-10"),
    RFPENDTC = c("2006-04-01", "2006-03-26", "2006-03-19", "2006-01-08", "2006-04-12", "2006-04-25"),
    SITEID = c("01", "01", "01", "01", "02", "02"),
    INVNAM = c("JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "GONZALEZ, E", "GONZALEZ, E"),
    BRTHDTC = c("1948-12-13", "1955-03-22", "1938-01-19", "1941-07-02", "1950-06-23", "1956-05-05"),
    AGE = c(57, 50, 68, NA, 55, 49),
    AGEU = c("YEARS", "YEARS", "YEARS", "", "YEARS", "YEARS"),
    SEX = c("M", "M", "F", "M", "F", "F"),
    RACE = c(
      "WHITE",
      "WHITE",
      "BLACK OR AFRICAN AMERICAN",
      "ASIAN",
      "AMERICAN INDIAN OR ALASKA NATIVE",
      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDERS"
    ),
    ETHNIC = c(
      "HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO"
    ),
    ARMCD = c("A", "P", "P", "", "P", "A"),
    ARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ACTARMCD = c("A", "P", "P", "", "P", "A"),
    ACTARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ARMREAS = c("", "", "", "SCREENING FAILURE", "", ""),
    ACTARMULD = c("", "", "", "", "", ""),
    COUNTRY = c("USA", "USA", "USA", "USA", "USA", "USA")
  )

  expected_error <- data.frame(
    Row = c("2", "6"),
    Variable = c("RFSTDTC vs RFENDTC","RFSTDTC vs RFENDTC"),
    Original_Value = c("2006-01-15 vs 2006-01-14", "2006-06-03 vs 2006-04-05"),
    Rule_ID = c("SD0013", "SD0013"),
    Message = c("Value of RFSTDTC must be <= RFENDTC.",
                "Value of RFSTDTC must be <= RFENDTC."),
    stringsAsFactors = FALSE
  )

  res_error <- check_date_logic(
    dm,
    domain_name = "DM",
    target_vars = c("--STDTC", "--ENDTC"),
    rule_id = "SD0013",
    operator = "<="
  )

  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 2)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Original_Value, expected_error$Original_Value)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})

# 3. Error case 2: RFICDTC > RFSTDTC (informed consent after start) ----------
test_that("check_date_logic detects RFICDTC > RFSTDTC", {
  dm <- data.frame(
    Row = 1:6,
    STUDYID = c("ABC123", "ABC123", "ABC123", "ABC123", "ABC123", "ABC123"),
    DOMAIN = c("DM", "DM", "DM", "DM", "DM", "DM"),
    USUBJID = c("ABC12301001", "ABC12301002", "ABC12301003", "ABC12301004", "ABC12302001", "ABC12302002"),
    SUBJID = c("01001", "01002", "01003", "01004", "02001", "02002"),
    RFSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-02-03"),
    RFENDTC = c("2006-03-10", "2006-02-28", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    RFXSTDTC = c("2006-01-12", "2006-01-15", "2006-01-16", "", "2006-02-02", "2006-02-03"),
    RFXENDTC = c("2006-03-10", "2006-02-28", "2006-03-19", "", "2006-03-31", "2006-04-05"),
    # Row 3: RFSTDTC = "2006-01-16", RFICDTC changed to "2006-01-17" -> error
    RFICDTC = c("2006-01-03", "2006-01-04", "2006-01-17", "2006-01-07", "2006-01-15", "2006-01-10"),
    RFPENDTC = c("2006-04-01", "2006-03-26", "2006-03-19", "2006-01-08", "2006-04-12", "2006-04-25"),
    SITEID = c("01", "01", "01", "01", "02", "02"),
    INVNAM = c("JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "JOHNSON, M", "GONZALEZ, E", "GONZALEZ, E"),
    BRTHDTC = c("1948-12-13", "1955-03-22", "1938-01-19", "1941-07-02", "1950-06-23", "1956-05-05"),
    AGE = c(57, 50, 68, NA, 55, 49),
    AGEU = c("YEARS", "YEARS", "YEARS", "", "YEARS", "YEARS"),
    SEX = c("M", "M", "F", "M", "F", "F"),
    RACE = c(
      "WHITE",
      "WHITE",
      "BLACK OR AFRICAN AMERICAN",
      "ASIAN",
      "AMERICAN INDIAN OR ALASKA NATIVE",
      "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDERS"
    ),
    ETHNIC = c(
      "HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO",
      "NOT HISPANIC OR LATINO"
    ),
    ARMCD = c("A", "P", "P", "", "P", "A"),
    ARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ACTARMCD = c("A", "P", "P", "", "P", "A"),
    ACTARM = c("Drug A", "Placebo", "Placebo", "", "Placebo", "Drug A"),
    ARMREAS = c("", "", "", "SCREENING FAILURE", "", ""),
    ACTARMULD = c("", "", "", "", "", ""),
    COUNTRY = c("USA", "USA", "USA", "USA", "USA", "USA")
  )

  expected_error <- data.frame(
    Row = "3",
    Variable = "RFICDTC vs RFSTDTC",
    Rule_ID = "SD1334",
    Message = "Value of RFICDTC must be <= RFSTDTC.",
    Original_Value = "2006-01-17 vs 2006-01-16",
    stringsAsFactors = FALSE
  )

  res_error <- check_date_logic(
    dm,
    domain_name = "DM",
    target_vars = c("RFICDTC", "RFSTDTC"),
    rule_id = "SD1334",
    operator = "<="
  )

  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 1)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Original_Value, expected_error$Original_Value)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})