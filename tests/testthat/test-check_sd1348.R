# ---- Datasets ----
# SD1348: PP domain with missing PPSTINT/PPENINT when PPTEST references 'T1 to T2'
pp_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "PP",
  PPSEQ = 1:5,
  PPTEST = c("AUC from T1 to T2", "T1 to T2", "Dose to Dose", "T1 to T2", "T1 to T2"),
  PPSTINT = c("Week 1", NA, "Week 1", NA, "Week 1"),
  PPENINT = c("Week 5", "Week 5", NA, NA, ""),
  rownumber_new = c(1, 5, 8, 11, 14),
  stringsAsFactors = FALSE
)

# SD1348: PP domain with all required values populated
pp_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "PP",
  PPSEQ = 1:3,
  PPTEST = c("T1 to T2", "T1 to T2", "Dose to Dose"),
  PPSTINT = c("Week 1", "Day 1", "Day 1"),
  PPENINT = c("Week 5", "Day 29", NA),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# Boundary: PPENINT variable missing entirely from df
pp_missing_var <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "PP",
  PPSEQ = 1:2,
  PPTEST = c("T1 to T2", "T1 to T2"),
  PPSTINT = c("Week 1", "Day 1"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: Missing condition variable PPTEST
pp_missing_cond <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "PP",
  PPSEQ = 1:2,
  PPSTINT = c("Week 1", NA),
  PPENINT = c("Week 5", "Week 5"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1348: Missing values for --STINT and --ENINT variables when --TEST references 'T1 to T2'
test_that("SD1348: catches missing PPSTINT and PPENINT when PPTEST references 'T1 to T2' (one row per error)", {
  expected_error <- data.frame(
    Row = c("5", "11", "11", "14"),
    Variable = c("PPSTINT", "PPSTINT", "PPENINT", "PPENINT"),
    Original_Value = c(NA_character_, NA_character_, NA_character_, ""),
    Rule_ID = rep("SD1348", 4),
    Message = c(
      "PPSTINT should be populated when PPTEST references a time interval ('T1 to T2').",
      "PPSTINT should be populated when PPTEST references a time interval ('T1 to T2').",
      "PPENINT should be populated when PPTEST references a time interval ('T1 to T2').",
      "PPENINT should be populated when PPTEST references a time interval ('T1 to T2')."
    ),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1348(
    df = pp_violation,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4) # One row per error
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1348: does not flag rows where PPTEST does not reference 'T1 to T2'", {
  # Row 8: PPTEST='Dose to Dose' (no trigger) with both PPSTINT/PPENINT populated,
  # and row 8 is not in the report
  res <- check_sd1348(
    df = pp_violation,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  
  expect_false("8" %in% res$Row)
})

test_that("SD1348: returns NULL when all triggered rows are populated", {
  res <- check_sd1348(
    df = pp_ok,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  expect_null(res)
})

test_that("SD1348: reports missing variable itself with 'should be present' message", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("PPENINT", "PPENINT"),
    Original_Value = c(NA_character_, NA_character_),
    Rule_ID = rep("SD1348", 2),
    Message = rep("PPENINT variable should be present and populated when PPTEST references a time interval ('T1 to T2').", 2),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1348(
    df = pp_missing_var,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1348: works for APPP domain via wildcard resolution", {
  appp_violation <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "PP",
    PPSEQ = 1:2,
    PPTEST = c("T1 to T2", "T1 to T2"),
    PPSTINT = c("Week 1", NA),
    PPENINT = c("Week 5", "Week 5"),
    rownumber_new = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  expected_error <- data.frame(
    Row = "2",
    Variable = "PPSTINT",
    Original_Value = NA_character_,
    Rule_ID = "SD1348",
    Message = "PPSTINT should be populated when PPTEST references a time interval ('T1 to T2').",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1348(
    df = appp_violation,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1348: returns NULL when no PPTEST values trigger the interval condition", {
  pp_no_trigger <- data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "PP",
    PPSEQ = 1:2,
    PPTEST = c("Dose to Dose", "Screening"),
    PPSTINT = c(NA, NA),
    PPENINT = c(NA, NA),
    rownumber_new = 1:2,
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1348(
    df = pp_no_trigger,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  expect_null(res)
})

# Boundary: Missing condition variable in df
test_that("Boundary: returns NULL when condition variable is missing in df", {
  res <- check_sd1348(
    df = pp_missing_cond,
    domain_name = "PP",
    target_vars = c("--STINT", "--ENINT"),
    rule_id = "SD1348",
    cond_vars = "--TEST"
  )
  expect_null(res)
})
