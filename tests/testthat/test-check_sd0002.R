# test-check_sd0002.R
# no error
test_that("SD0002 return null(all pass)", {
  # 1. a correct dataframe
  ts <- data.frame(
    STUDYID = rep("CDISCPILOT01", 6),
    DOMAIN = rep("TS", 6),
    TSSEQ = c(1, 1, 1, 1, 2, 1),
    TSPARMCD = c("ADDON", "AGEMAX", "AGEMIN", "AGESPAN", "AGESPAN",
                 "TBLIND"),
    TSPARM = c("Added on to Existing Treatments",
               "Planned Maximum Age of Subjects",
               "Planned Minimum Age of Subjects",
               "Age Group", 
               "Age Group",
               "Trial Blinding Schema"),
    TSVAL = c("Y",
              "No maximum",
              "50 years",
              "ADULT (18-65)",
              "ELDERLY (> 65)",
              "DOUBLE BLIND"),
    stringsAsFactors = FALSE
  )

  # 2. run test
  res_clean <- check_sd0002(ts, "TS")
  expect_null(res_clean)
})

# 3 error
test_that("SD0002 return error", {
  # 1. 3 error dataframe
  ts <- data.frame(
    STUDYID = rep("CDISCPILOT01", 6),
    DOMAIN = rep("TS", 6),
    TSSEQ = c(1, 1, 1, 1, 2, 1),
    TSPARMCD = c("ADDON", "", "AGEMIN", "AGESPAN", "AGESPAN",
                 "TBLIND"),
    TSPARM = c("Added on to Existing Treatments",
               "Planned Maximum Age of Subjects",
               "Planned Minimum Age of Subjects",
               NA_character_,
               "Trial Blinding Schema",
               "Comparative Treatment Name"),
    TSVAL = c("Y",
              "No maximum",
              "50 years",
              "ADULT (18-65)",
              "ELDERLY (> 65)",
              "DOUBLE BLIND"),
    stringsAsFactors = FALSE
  )

  expected_error <- data.frame(
    Row = c("2", "4"),
    Variable = c("TSPARMCD", "TSPARM"),
    Rule_ID = c("SD0002", "SD0002"),
    Message = c(
      "Required variable 'TSPARMCD' is null or empty.",
      "Required variable 'TSPARM' is null or empty."
    ),
    stringsAsFactors = FALSE
  )

  # 2. run test
  res_error <- check_sd0002(ts, "TS")

  # 3.check
  expect_equal(nrow(res_error), 2)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})