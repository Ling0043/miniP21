
# Test 1: All valid cases return NULL
test_that("check_expected_cond returns NULL for all valid TSVCDREF values", {
  # Rule 1: TSVCDREF must be 'SNOMED' when TSPARMCD='INDIC'
  expect_null(
    check_expected_cond(ts, domain_name = "TS",
                        target_vars = "TSVCDREF",
                        cond_vars = "TSPARMCD",
                        cond_vals = "INDIC",
                        expected_val = "SNOMED",
                        rule_id = "SD2240")
  )
  
  # Rule 2: TSVCDREF must be 'UNII' when TSPARMCD='CURTRT'
  expect_null(
    check_expected_cond(ts, domain_name = "TS", 
                        target_vars = "TSVCDREF", 
                        cond_vars = "TSPARMCD", 
                        cond_vals = "CURTRT", 
                        expected_val = "UNII", 
                        rule_id = "SD2241")
  )
  
  # Rule 3: TSVCDREF must be 'MED-RT' when TSPARMCD='PCLAS' and version > 2018-02-05
  # Note: Testing with version 2020-01-01
  expect_null(
    check_expected_cond(ts[ts$TSVCDVER == "2020-01-01", ], 
                        domain_name = "TS", 
                        target_vars = "TSVCDREF", 
                        cond_vars = "TSPARMCD", 
                        cond_vals = "PCLAS", 
                        expected_val = "MED-RT", 
                        rule_id = "SD2242")
  )
  
  # Rule 4: TSVCDREF must be 'UNII' when TSPARMCD='COMPTRT'
  expect_null(
    check_expected_cond(ts, domain_name = "TS", 
                        target_vars = "TSVCDREF", 
                        cond_vars = "TSPARMCD", 
                        cond_vals = "COMPTRT", 
                        expected_val = "UNII", 
                        rule_id = "SD2243")
  )
  
  # Rule 5: TSVCDREF must be 'UNII' when TSPARMCD='TRT'
  expect_null(
    check_expected_cond(ts, domain_name = "TS", 
                        target_vars = "TSVCDREF", 
                        cond_vars = "TSPARMCD", 
                        cond_vals = "TRT", 
                        expected_val = "UNII", 
                        rule_id = "SD2256")
  )
  
  # Rule 6: TSVCDREF must be 'SNOMED' when TSPARAMCD='TDIGRP'
  expect_null(
    check_expected_cond(ts, domain_name = "TS", 
                        target_vars = "TSVCDREF", 
                        cond_vars = "TSPARAMCD", 
                        cond_vals = "TDIGRP", 
                        expected_val = "SNOMED", 
                        rule_id = "SD2266")
  )
})

# Test 2: Error case - TSPARMCD='INDIC' with wrong TSVCDREF
test_that("check_expected_cond catches invalid TSVCDREF for TSPARMCD='INDIC'", {
  ts_error <- ts
  ts_error$TSVCDREF[ts_error$TSPARMCD == "INDIC"] <- "LOINC"

  expected_error <- data.frame(
    Row = "12",
    Variable = "TSVCDREF",
    Original_Value = "LOINC",
    Rule_ID = "SD2240",
    Message = "Value must be 'SNOMED' when TSPARMCD='INDIC'",
    stringsAsFactors = FALSE
  )

  result <- check_expected_cond(ts_error, domain_name = "TS",
                               target_vars = "TSVCDREF",
                               cond_vars = "TSPARMCD",
                               cond_vals = "INDIC",
                               expected_val = "SNOMED",
                               rule_id = "SD2240")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Row, expected_error$Row)
  expect_equal(result$Variable, expected_error$Variable)
  expect_equal(result$Original_Value, expected_error$Original_Value)
  expect_equal(result$Rule_ID, expected_error$Rule_ID)
  expect_equal(result$Message, expected_error$Message)
})

# Test 3: Error case - TSPARMCD='CURTRT' with wrong TSVCDREF
test_that("check_expected_cond catches invalid TSVCDREF for TSPARMCD='CURTRT'", {
  ts_error <- ts
  ts_error$TSVCDREF[ts_error$TSPARMCD == "CURTRT"] <- "SNOMED"

  expected_error <- data.frame(
    Row = "18",
    Variable = "TSVCDREF",
    Original_Value = "SNOMED",
    Rule_ID = "SD2241",
    Message = "Value must be 'UNII' when TSPARMCD='CURTRT'",
    stringsAsFactors = FALSE
  )
  
  result <- check_expected_cond(ts_error, domain_name = "TS", 
                               target_vars = "TSVCDREF", 
                               cond_vars = "TSPARMCD", 
                               cond_vals = "CURTRT", 
                               expected_val = "UNII", 
                               rule_id = "SD2241")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Row, expected_error$Row)
  expect_equal(result$Variable, expected_error$Variable)
  expect_equal(result$Original_Value, expected_error$Original_Value)
  expect_equal(result$Rule_ID, expected_error$Rule_ID)
  expect_equal(result$Message, expected_error$Message)
})

# Test 4: Error case - TSPARMCD='PCLAS' with wrong TSVCDREF for lastest versions
test_that("check_expected_cond catches invalid TSVCDREF for TSPARMCD='PCLAS'", {
  ts_error <- ts
  ts_error$TSVCDREF[ts_error$TSPARMCD == "PCLAS"] <- "MED-RT"
  
  expected_error <- data.frame(
    Row = "32",
    Variable = "TSVCDREF",
    Original_Value = "MED-RT",
    Rule_ID = "SD2242",
    Message = "Value must be 'NDF-RT' when TSPARMCD='PCLAS'",
    stringsAsFactors = FALSE
  )
  
  result <- check_expected_cond(ts_error, domain_name = "TS", 
                               target_vars = "TSVCDREF", 
                               cond_vars = "TSPARMCD", 
                               cond_vals = "PCLAS", 
                               expected_val = "NDF-RT", 
                               rule_id = "SD2242")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Row, expected_error$Row)
  expect_equal(result$Variable, expected_error$Variable)
  expect_equal(result$Original_Value, expected_error$Original_Value)
  expect_equal(result$Rule_ID, expected_error$Rule_ID)
  expect_equal(result$Message, expected_error$Message)
})

# Test 5: Error case - TSPARMCD='TRT' with wrong TSVCDREF
test_that("check_expected_cond catches invalid TSVCDREF for TSPARMCD='TRT'", {
  ts_error <- ts
  ts_error$TSVCDREF[ts_error$TSPARMCD == "TRT"] <- "SNOMED"
  
  expected_error <- data.frame(
    Row = "23",
    Variable = "TSVCDREF",
    Original_Value = "SNOMED",
    Rule_ID = "SD2245",
    Message = "Value must be 'UNII' when TSPARMCD='TRT'",
    stringsAsFactors = FALSE
  )
  
  result <- check_expected_cond(ts_error, domain_name = "TS", 
                               target_vars = "TSVCDREF", 
                               cond_vars = "TSPARMCD", 
                               cond_vals = "TRT", 
                               expected_val = "UNII", 
                               rule_id = "SD2245")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Row, expected_error$Row)
  expect_equal(result$Variable, expected_error$Variable)
  expect_equal(result$Original_Value, expected_error$Original_Value)
  expect_equal(result$Rule_ID, expected_error$Rule_ID)
  expect_equal(result$Message, expected_error$Message)
})

# Test 6: Error case - TSPARMCD='TDIGRP' with wrong TSVCDREF
test_that("check_expected_cond catches invalid TSVCDREF for TSPARMCD='TDIGRP'", {
  ts_error <- ts
  ts_error$TSVCDREF[ts_error$TSPARMCD == "TDIGRP"] <- "LOINC"

  expected_error <- data.frame(
    Row = "11",
    Variable = "TSVCDREF",
    Original_Value = "LOINC",
    Rule_ID = "SD2246",
    Message = "Value must be 'SNOMED' when TSPARMCD='TDIGRP'",
    stringsAsFactors = FALSE
  )

  result <- check_expected_cond(ts_error, domain_name = "TS",
                               target_vars = "TSVCDREF",
                               cond_vars = "TSPARMCD",
                               cond_vals = "TDIGRP", 
                               expected_val = "SNOMED",
                               rule_id = "SD2246")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Row, expected_error$Row)
  expect_equal(result$Variable, expected_error$Variable)
  expect_equal(result$Original_Value, expected_error$Original_Value)
  expect_equal(result$Rule_ID, expected_error$Rule_ID)
  expect_equal(result$Message, expected_error$Message)
})

# Test 7: No matching rows returns NULL
test_that("check_expected_cond returns NULL when no rows match condition", {
  ts_no_match <- ts[ts$TSPARMCD != "INDIC", ]
  
  result <- check_expected_cond(ts_no_match, domain_name = "TS", 
                                target_vars = "TSVCDREF", 
                                cond_vars = "TSPARMCD", 
                                cond_vals = "INDIC", 
                                expected_val = "SNOMED", 
                                rule_id = "SD2248")
  
  expect_null(result)
})

# Test 8: Missing required columns returns NULL
test_that("check_expected_cond returns NULL when required columns are missing", {
  ts_missing_col <- ts[, !(names(ts) %in% c("TSVCDREF"))]
  
  result <- check_expected_cond(ts_missing_col, domain_name = "TS", 
                                target_vars = "TSVCDREF", 
                                cond_vars = "TSPARMCD", 
                                cond_vals = "INDIC", 
                                expected_val = "SNOMED", 
                                rule_id = "SD2249")

  expect_null(result)
})