# no error ---
test_that("check_length return null(all pass)", {
  # 1.a correct dataframe
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

  # 2. run test
  res_clean <- check_length(dm, "DM", "SD1004", "ARMCD", 20)
  expect_null(res_clean)
})

# # 2. test-1
test_that("check_length return 1 error",{
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
  ARMCD = c("A", "P", "P", "", "Pooooooooooooooooooooooo", "A"),
  ARM = c("Drug A", "Placebo", "Placebo", "", "Placebo2", "Drug A"),
  ACTARMCD = c("A", "P", "P", "", "P", "A")
  )

  expected_error <- data.frame(
    Row = c("5"),
    Variable = c("ARMCD"),
    Rule_ID = c("SD1004"),
    Message = c("The value of 'ARMCD' should be <= 20 characters in length. But actual value is 'Pooooooooooooooooooooooo'"),
    stringsAsFactors = FALSE
  )

  res_error <- check_length(dm, "DM", "SD1004", "ARMCD", 20)
  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 1)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})



test_that("check_length return 1 error",{
  te <- data.frame(
  STUDYID = c("EX1", "EX1", "EX1", "EX1", "EX1"),
  DOMAIN = c("TE", "TE", "TE", "TE", "TE"),
  ETCD = c("SCRN", "RI", "Placebo11", "A", "B"),
  ELEMENT = c("Screen", "Run-In", "Placebo", "Drug A", "Drug B"),
  TESTRL = c(
    "Informed consent",
    "Eligibility confirmed",
    "First dose of study drug, where drug is placebo",
    "First dose of study drug, where drug is Drug A",
    "First dose of study drug, where drug is Drug B"
  ),
  TENRL = c(
    "1 week after start of Element",
    "2 weeks after start of Element",
    "2 weeks after start of Element",
    "2 weeks after start of Element",
    "2 weeks after start of Element"
  ),
  TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D")
  )

  expected_error2 <- data.frame(
    Row = c("3"),
    Variable = c("ETCD"),
    Rule_ID = c("SD1009"),
    Message = c("The value of 'ETCD' should be <= 8 characters in length. But actual value is 'Placebo11'"),
    stringsAsFactors = FALSE
  )

  res_error2 <- check_length(te, "TE", "SD1009", "ETCD", 8)
  expect_s3_class(res_error2, "data.frame")
  expect_equal(nrow(res_error2), 1)
  expect_equal(res_error2$Row, expected_error2$Row)
  expect_equal(res_error2$Variable, expected_error2$Variable)
  expect_equal(res_error2$Rule_ID, expected_error2$Rule_ID)
  expect_equal(res_error2$Message, expected_error2$Message)
})