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
    TEDUR = c("P7D", "P14D", "P14D", "P14D", "P14D")
  )
  # 2. run test
  res_clean <- check_variable(te, "TE", rule_id = "SD0056")
  expect_null(res_clean)
})

# there are 1 error ----
test_that("SD0058 return 1 error", {
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
    Row = c("-"),
    Variable = c("TENRL"),
    Original_value = c("-"),
    Rule_ID = c("SD0058"),
    Message = c("Only variables listed in SDTM model should appear in a dataset"),
    stringsAsFactors = FALSE
  )

  # 2. run test
  res_error <- check_variable(te, "TE", rule_id = "SD0058")

  expect_s3_class(res_error, "data.frame")
  expect_equal(nrow(res_error), 1)
  expect_equal(res_error$Row, expected_error$Row)
  expect_equal(res_error$Variable, expected_error$Variable)
  expect_equal(res_error$Rule_ID, expected_error$Rule_ID)
  expect_equal(res_error$Message, expected_error$Message)
})
