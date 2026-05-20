test_that("check_sd1022 return null(all pass)", {
  suppqual <- data.frame(
  Row = 1:6,
  STUDYID = rep("TEST-001", 6),
  RDOMAIN = c("AE", "AE", "LB", "VS", "PE", "CM"),
  USUBJID = c(
    "TEST-001-001",
    "TEST-001-001",
    "TEST-001-002",
    "TEST-001-003",
    "TEST-001-003",
    "TEST-001-004"
  ),
  IDVAR    = c("AESEQ", "AESEQ", "LBSEQ", "VSSEQ", "PESEQ", "CMSEQ"),
  IDVARVAL = c("1", "2", "3", "1", "2", "1"),
  QNAM     = c("AEREL", "AESERCAT", "LBREM", "VSPOS", "PEREM", "CMIND"),
  QLABEL   = c(
    "Relationship to Study Drug",
    "Serious Event Category",
    "Laboratory Remark",
    "Vital Signs Position",
    "Physical Exam Remark",
    "Concomitant Med Indication"
  ),
  QVAL    = c(
    "POSSIBLE",
    "HOSPITAL",
    "CONTAMINATED SAMPLE",
    "SITTING",
    "SKIN RASH OBSERVED",
    "HYPERTENSION"
  ),
  QORIG = c("CRF", "DERIVED", "CRF", "ASSIGNED", "CRF", "CRF"),
  QEVAL = c("", "ADJUDICATION", "", "", "", "")
)

  res_clean <- check_sd1022(suppqual, "SUPPQUAL")
  expect_null(res_clean)
})


test_that("check_sd1022 return 1 error", {
  suppqual <- data.frame(
    Row = 1:6,
    STUDYID = rep("TEST-001", 6),
    RDOMAIN = c("AE", "AE", "LB", "VS", "PE", "CM"),
    USUBJID = c(
      "TEST-001-001",
      "TEST-001-001",
      "TEST-001-002",
      "TEST-001-003",
      "TEST-001-003",
      "TEST-001-004"
    ),
    IDVAR    = c("AESEQ", "AESEQ", "LBSEQ", "VSSEQ", "PESEQ", "CMSEQ"),
    IDVARVAL = c("1", "2", "3", "1", "2", "1"),
    QNAM     = c(
      "AEREL",
      "AESERCATX9", 
      "LBREM",
      "9VSPOS",
      "PEREM",
      "CMIND"
    )
  )
  
  expected_error <- data.frame(
    Row = c("2", "4"),
    Variable = c("QNAM", "QNAM"),
    Rule_ID = c("SD1022", "SD1022"),
    Message = c(
      "Invalid QNAM value 'AESERCATX9' (should be <= 8 characters in length).",
      "Invalid QNAM value '9VSPOS' (can't starts with a number)."
    ),
    stringsAsFactors = FALSE
  )

  res_error <- check_sd1022(suppqual, "SUPPQUAL")
  expect_equal(nrow(res_error), 2)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})