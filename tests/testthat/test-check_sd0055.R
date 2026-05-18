# no error ---
test_that("SD0055 return null(all pass)", {
  # 1.a correct dataframe
  ta <- data.frame(
    STUDYID = c("EX7", "EX7", "EX7", "EX7", "EX7", "EX7"),
    DOMAIN = c("TA", "TA", "TA", "TA", "TA", "TA"),
    ARMCD = c("1", "1", "1", "1", "1", "1"),
    ARM = c("CR", "CR", "CR", "CR", "CR", "CR"),
    TAETORD = 1:6,
    ETCD = c("SCRN", "ICR", "CRNS", "C", "C", "FU"),
    ELEMENT = c("Screen", "Initial Chemo + RT",
                "Chemo+RT (non-Surgery)", "Chemo",
                "Chemo", "Off Treatment Follow-up"),
    TABRANCH = c("Randomized to CR", "", "",
                "", "", ""),
    TATRANS = c("", "", "If progression, skip to Follow-up.",
                "", "", ""),
    EPOCH = c("SCREENING", "INDUCTION TREATMENT",
              "INDUCTION TREATMENT", "CONTINUATION TREATMENT",
              "CONTINUATION TREATMENT", "FOLLOW-UP")
  )

  # 2. run test
  res_clean <- check_sd0055(ta, "TA")
  expect_null(res_clean)
})

# there are two error ----
test_that("SD0055 return 2 errors", {
  # 1.a correct dataframe
  ta <- data.frame(
    STUDYID = c("EX7", "EX7", "EX7", "EX7", "EX7", "EX7"),
    DOMAIN = c("TA", "TA", "TA", "TA", "TA", "TA"),
    ARMCD = c(1, 1, 1, 1, 1, 1),
    ARM = c("CR", "CR", "CR", "CR", "CR", "CR"),
    TAETORD = as.character(1:6),
    ETCD = c("SCRN", "ICR", "CRNS", "C", "C", "FU"),
    ELEMENT = c("Screen", "Initial Chemo + RT",
                "Chemo+RT (non-Surgery)", "Chemo",
                "Chemo", "Off Treatment Follow-up"),
    TABRANCH = c("Randomized to CR", "", "",
                 "", "", ""),
    TATRANS = c("", "", "If progression, skip to Follow-up.",
                "", "", ""),
    EPOCH = c("SCREENING", "INDUCTION TREATMENT",
              "INDUCTION TREATMENT", "CONTINUATION TREATMENT",
              "CONTINUATION TREATMENT", "FOLLOW-UP")
  )

  expected_error <- data.frame(
    Row = c("-", "-"),
    Variable = c("ARMCD", "TAETORD"),
    Rule_ID = c("SD0055", "SD0055"),
    Message = c(
      "Type mismatch: Expected 'character', but 'numeric' in dataframe.",
      "Type mismatch: Expected 'numeric', but 'character' in dataframe."
    ),
    stringsAsFactors = FALSE
  )

  # 2. run test
  res_error <- check_sd0055(ta, "TA")

  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 2)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})
