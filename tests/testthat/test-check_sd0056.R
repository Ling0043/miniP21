# no error ---
test_that("SD0056 return null(all pass)", {
  # 1.a correct dataframe
  te <- data.frame(
    STUDYID = c("EX1", "EX1", "EX1", "EX1", "EX1"),
    DOMAIN = c("TE", "TE", "TE", "TE", "TE"),
    ETCD = c("SCRN", "RI", "P", "A", "B"),
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

  # 2. run test
  res_clean <- check_sd0056(te, "TE")
  expect_null(res_clean)
})

# there are 1 error ----
test_that("SD0056 return 2 errors", {
  # 1.a correct dataframe
  te <- data.frame(
    DOMAIN = c("TE", "TE", "TE", "TE", "TE"),
    ETCD = c("SCRN", "RI", "Placebo11", "A", "B"),
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

  expected_error <- data.frame(
    Row = c("-", "-"),
    Variable = c("STUDYID", "ELEMENT"),
    Rule_ID = c("SD0056", "SD0056"),
    Message = c("Required variable 'STUDYID' is not found.",
                "Required variable 'ELEMENT' is not found."),
    stringsAsFactors = FALSE
  )

  # 2. run test
  res_error <- check_sd0056(te, "TE")

  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 2)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})
