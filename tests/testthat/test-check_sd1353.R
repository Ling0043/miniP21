# ---- Datasets ----
# SD1353: LB domain for --STRESC vs --ORRES comparison
lb_violation <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:5,
  LBSTRESC = c("1.0", "5", "5", "10", "Negative"),
  LBORRES  = c("1",   "5", "6", "10", "0"),
  LBSTRESU = c("mg/mL", "mg/dL", "g/dL", NA, "mg/mL"),
  LBORRESU = c("mg/mL", "mg/dL", "mg/dL", "", "mg/mL"),
  rownumber_new = 1:5,
  stringsAsFactors = FALSE
)

# Boundary: Missing required target variables
lb_missing_cols <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  LBSEQ = 1:3,
  LBSTRESC = c("1", "2", "3"),
  LBORRES  = c("1", "2", "3"),
  rownumber_new = 1:3,
  stringsAsFactors = FALSE
)

# ---- Tests ----
# SD1353: --STRESC does not equal --ORRES when --STRESU is equal to --ORRESU
test_that("SD1353: catches LBSTRESC not equal to LBORRES when units match and both are numeric", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "LBSTRESC,LBORRES,LBSTRESU,LBORRESU",
    Original_Value = " ",
    Rule_ID = "SD1353",
    Message = "When LBSTRESU equals LBORRESU (or both null) and both values are numeric, LBSTRESC should equal LBORRES.",
    stringsAsFactors = FALSE
  )
  
  res <- check_sd1353(
    df = lb_violation,
    domain_name = "LB",
    rule_id = "SD1353"
  )
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})

test_that("SD1353: returns NULL when no violation (various valid scenarios)", {
  # Row 2: Units match, both numeric, values match
  # Row 3: Units differ (g/dL vs mg/dL), so no check (values differ but valid)
  # Row 4: Units both null, both numeric, values match
  # Row 5: Units match, but STRESC is not numeric ("Negative"), so no check
  
  # To ensure these rows don't trigger errors, we can run the entire df.
  # Since only row 1 violates, if we fix row 1, we expect NULL.
  lb_ok <- lb_violation
  lb_ok$LBSTRESC[1] <- "1"
  
  res <- check_sd1353(
    df = lb_ok,
    domain_name = "LB",
    rule_id = "SD1353"
  )
  expect_null(res)
})

# Boundary: Missing required target variables in df
test_that("Boundary: returns NULL when required columns are missing in df", {
  res <- check_sd1353(
    df = lb_missing_cols,
    domain_name = "LB",
    rule_id = "SD1353"
  )
  expect_null(res)
})
