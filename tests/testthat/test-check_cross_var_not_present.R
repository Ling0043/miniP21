# ---- Datasets ----
# EC reference dataset (non-empty, so target variables should not be populated)
ec_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EC",
  rownumber_new = as.character(1:2),
  ECSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  ECDOSE = c("10", "20"),
  ECDOSU = c("mg", "mg"),
  stringsAsFactors = FALSE
)
# SD1468: EX domain with EXVAMT populated on row 1
ex_vamt <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXVAMT = c("10", "", NA),
  stringsAsFactors = FALSE
)
# SD1468: compliant EX domain (EXVAMT empty everywhere)
ex_vamt_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXVAMT = c("", NA, ""),
  stringsAsFactors = FALSE
)
# SD1469: EX domain with EXVAMTU populated on rows 1 and 3
ex_vamtu <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXVAMTU = c("mg", "", "mL"),
  stringsAsFactors = FALSE
)
# SD1469: compliant EX domain (EXVAMTU empty everywhere)
ex_vamtu_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  rownumber_new = as.character(1:3),
  EXSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  EXTRT = c("DRUG", "DRUG", "PLACEBO"),
  EXVAMTU = c(NA, "", NA),
  stringsAsFactors = FALSE
)
# Boundary: EX domain without rownumber_new column
ex_no_rownr <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "EX",
  EXSEQ = 1:2,
  USUBJID = c("S01", "S02"),
  EXVAMT = c("10", ""),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD1468: EXVAMT should not be populated when EC domain exists
test_that("SD1468: catches EXVAMT populated when EC domain exists", {
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXVAMT",
    Original_Value = "10",
    Rule_ID = "SD1468",
    Message = "EXVAMT should not be populated when EC domain exists.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1468: returns NULL when EXVAMT is never populated", {
  res <- check_cross_var_not_present(
    main_df = ex_vamt_ok,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_null(res)
})
test_that("SD1468: reports one row per violation when multiple rows fail", {
  expected_error <- data.frame(
    Row = c("1", "2"),
    Variable = c("EXVAMT", "EXVAMT"),
    Original_Value = c("10", "15"),
    Rule_ID = c("SD1468", "SD1468"),
    Message = c(
      "EXVAMT should not be populated when EC domain exists.",
      "EXVAMT should not be populated when EC domain exists."
    ),
    stringsAsFactors = FALSE
  )
  ex_multi <- ex_vamt
  ex_multi$EXVAMT <- c("10", "15", "")
  res <- check_cross_var_not_present(
    main_df = ex_multi,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# SD1469: EXVAMTU should not be populated when EC domain exists
test_that("SD1469: catches EXVAMTU populated when EC domain exists", {
  expected_error <- data.frame(
    Row = c("1", "3"),
    Variable = c("EXVAMTU", "EXVAMTU"),
    Original_Value = c("mg", "mL"),
    Rule_ID = c("SD1469", "SD1469"),
    Message = c(
      "EXVAMTU should not be populated when EC domain exists.",
      "EXVAMTU should not be populated when EC domain exists."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_vamtu,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMTU",
    rule_id = "SD1469",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1469: returns NULL when EXVAMTU is never populated", {
  res <- check_cross_var_not_present(
    main_df = ex_vamtu_ok,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMTU",
    rule_id = "SD1469",
    ref_domains = "EC"
  )
  expect_null(res)
})
test_that("SD1469: catches single EXVAMTU violation", {
  ex_single <- ex_vamtu
  ex_single$EXVAMTU <- c("", "", "mL")
  expected_error <- data.frame(
    Row = "3",
    Variable = "EXVAMTU",
    Original_Value = "mL",
    Rule_ID = "SD1469",
    Message = "EXVAMTU should not be populated when EC domain exists.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_single,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMTU",
    rule_id = "SD1469",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_var_not_present(
    main_df = ex_vamt[0, ],
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_null(res)
})
# Boundary: reference domain missing or empty returns NULL
test_that("Boundary: returns NULL when reference domain is missing", {
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EX = ex_vamt),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_null(res)
})
test_that("Boundary: returns NULL when reference domain is empty", {
  ec_empty <- ec_ref[0, ]
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_empty),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_null(res)
})
# Boundary: multiple reference domains, any non-empty triggers the check
test_that("Boundary: returns NULL when all reference domains are empty", {
  ec_empty <- ec_ref[0, ]
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_empty, XX = ec_empty),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = c("EC", "XX")
  )
  expect_null(res)
})
test_that("Boundary: check is triggered when the second reference domain is non-empty", {
  ec_empty <- ec_ref[0, ]
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXVAMT",
    Original_Value = "10",
    Rule_ID = "SD1468",
    Message = "EXVAMT should not be populated when EC, XX domain exists.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_empty, XX = ec_ref),
    domain_name = "EX",
    target_vars = "EXVAMT",
    rule_id = "SD1468",
    ref_domains = c("EC", "XX")
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: target variable not found in current domain
test_that("Boundary: reports error when target variable not found in main df", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1468",
    Message = "Required variable(s) not found in current domain: NOTAVAR.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "NOTAVAR",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: more than one target variable
test_that("Boundary: reports error when target_vars has more than one variable", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD1468",
    Message = "target_vars must contain exactly one variable for check_cross_var_not_present.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_vamt,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = c("EXVAMT", "EXVAMTU"),
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})


# Boundary: -- wildcard resolved with domain_name
test_that("Boundary: resolves -- prefix with domain_name", {
  ex_wildcard <- ex_vamt
  ex_wildcard$EXVAMT <- c("10", "", "")
  expected_error <- data.frame(
    Row = "1",
    Variable = "EXVAMT",
    Original_Value = "10",
    Rule_ID = "SD1468",
    Message = "EXVAMT should not be populated when EC domain exists.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_var_not_present(
    main_df = ex_wildcard,
    ref_dfs = list(EC = ec_ref),
    domain_name = "EX",
    target_vars = "--VAMT",
    rule_id = "SD1468",
    ref_domains = "EC"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
