# ---- Datasets ----
# SD1212: LB domain with mismatched STRESN and STRESC
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBSTRESN = c(6, 5.5, NA),
  LBSTRESC = c("5.5", "5.5", "Normal"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1212: LB domain with correct match and non-numeric STRESC
lb_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBSTRESN = c(5.5, 10, NA),
  LBSTRESC = c("5.5", "10", "Negative"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# SD1212: VS domain with missing STRESN when STRESC is numeric
vs_missing <- data.frame(
  STUDYID = c("STUDY01", "STUDY01"),
  DOMAIN = "VS",
  VSSEQ = 1:2,
  VSSTRESN = c(NA_real_, 130),
  VSSTRESC = c("120", "130"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# Boundary: LB domain missing target variable
lb_missing_var <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBSTRESC = c("5.5", "10", "Negative"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1212: --STRESN does not equal --STRESC
test_that("SD1212: catches LBSTRESN not equal to LBSTRESC when LBSTRESC is numeric", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBSTRESN, LBSTRESC",
    Original_Value = "",
    Rule_ID = "SD1212",
    Message = "Value of LBSTRESN should be equal to LBSTRESC when LBSTRESC represents a numeric value.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1212(
    df = lb_violation,
    domain_name = "LB",
    target_var = "--STRESN",
    cond_var = "--STRESC",
    rule_id = "SD1212"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1212: returns NULL when no violation", {
  res <- check_sd1212(
    df = lb_ok,
    domain_name = "LB",
    target_var = "--STRESN",
    cond_var = "--STRESC",
    rule_id = "SD1212"
  )
  expect_null(res)
})

test_that("SD1212: catches missing VSSTRESN when VSSTRESC is numeric", {
  expected_error <- data.frame(
    Row = c("1", "1"),
    Variable = "VSSTRESN, VSSTRESC",
    Original_Value = "",
    Rule_ID = c("SD1122", "SD1212"),
    Message = c("Value of VSSTRESN should be populated when VSSTRESC represents a numeric value.",
                "Value of VSSTRESN should be equal to VSSTRESC when VSSTRESC represents a numeric value."),
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1212(
    df = vs_missing,
    domain_name = "VS",
    target_var = "--STRESN",
    cond_var = "--STRESC",
    rule_id = "SD1212"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

# Boundary: Missing target_vars in df
test_that("Boundary: returns NULL when target_vars missing in df", {
  res <- check_sd1212(
    df = lb_missing_var,
    domain_name = "LB",
    target_var = "--STRESN",
    cond_var = "--STRESC",
    rule_id = "SD1212"
  )
  expect_null(res)
})
