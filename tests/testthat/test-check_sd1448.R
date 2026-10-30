# ---- Datasets ----
# SD1448: LB domain for --STRESN populated when --STRESC is not numeric
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:6,
  LBSTRESC = c("BLQ", "<5", "+1", "10", "BLQ", NA),
  LBSTRESN = c(1, 5, 1, 10, NA, 10),
  rownumber_new = 1:6,
  stringsAsFactors = FALSE
)

# SD1448: LB domain with valid combinations
lb_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:4,
  LBSTRESC = c("10", "Negative", "+1", NA),
  LBSTRESN = c(10, NA, NA, NA),
  rownumber_new = 1:4,
  stringsAsFactors = FALSE
)

# Boundary: Missing required target variables
lb_missing_cols <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:2,
  LBSTRESC = c("10", "<5"),
  rownumber_new = 1:2,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1448: --STRESN is populated when --STRESC value is not numeric
test_that("SD1448: catches LBSTRESN populated when LBSTRESC is not numeric", {
  expected_error <- data.frame(
    Row = c("1", "2", "3"),
    Variable = "LBSTRESN",
    Original_Value = c("1", "5", "1"),
    Rule_ID = "SD1448",
    Message = "LBSTRESN should be null when LBSTRESC is not numeric",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1448(
    df = lb_violation,
    domain_name = "LB",
    rule_id = "SD1448"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3) # Collapsed into a single row error report
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1448: returns NULL when no violation", {
  res <- check_sd1448(
    df = lb_ok,
    domain_name = "LB",
    rule_id = "SD1448"
  )
  expect_null(res)
})

# Boundary: Missing required columns in df
test_that("Boundary: returns NULL when required columns are missing in df", {
  res <- check_sd1448(
    df = lb_missing_cols,
    domain_name = "LB",
    rule_id = "SD1448"
  )
  expect_null(res)
})
