# ---- Datasets ----
# Trial Arms (TA) reference dataset
ta <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  rownumber_new = as.character(1:4),
  TASEQ = 1:4,
  ARMCD = c("ARM1", "ARM1", "ARM2", "ARM2"),
  TAETORD = c("1", "2", "1", "2"),
  EPOCH = c("SCREENING", "TREATMENT", "SCREENING", "TREATMENT"),
  ETCD = c("EL1", "EL2", "EL1", "EL2"),
  stringsAsFactors = FALSE
)
# Trial Elements (TE) reference dataset
te <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TE",
  rownumber_new = as.character(1:2),
  TESEQ = 1:2,
  ETCD = c("EL1", "EL2"),
  ELEMENT = c("Element 1", "Element 2"),
  stringsAsFactors = FALSE
)
# Trial Inclusion/Exclusion Criteria (TI) reference dataset
ti <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TI",
  rownumber_new = as.character(1:2),
  IETESTCD = c("INCL01", "EXCL01"),
  IETEST = c("Inclusion 1", "Exclusion 1"),
  IECAT = c("INCLUSION", "EXCLUSION"),
  stringsAsFactors = FALSE
)
# Trial Visits (TV) reference dataset
tv <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TV",
  rownumber_new = as.character(1:4),
  VISITNUM = c("1", "2", "3", "99"),
  VISIT = c("SCREENING", "TREATMENT", "FOLLOW-UP", "UNSCHEDULED VISIT"),
  stringsAsFactors = FALSE
)
# Demographics (DM) reference dataset for SD1354
dm_ref <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:3),
  USUBJID = c("S01", "S02", "S03"),
  ARMCD = c("ARM1", "ARM2", "ARM1"),
  stringsAsFactors = FALSE
)
# SD0066: DM domain with ARMCD (row 4 invalid)
dm_armcd <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "DM",
  rownumber_new = as.character(1:4),
  USUBJID = c("S01", "S02", "S03", "S04"),
  ARMCD = c("ARM1", "SCRNFAIL", "NOTASSGN", "WRONGCD"),
  stringsAsFactors = FALSE
)
# SD0067: SE domain with ETCD (row 3 invalid)
se <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  rownumber_new = as.character(1:3),
  SESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  ETCD = c("EL1", "UNPLAN", "BADCD"),
  stringsAsFactors = FALSE
)
# SD0068: IE domain with IETESTCD (row 3 invalid)
ie_cd <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "IE",
  rownumber_new = as.character(1:3),
  IESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  IETESTCD = c("INCL01", "EXCL01", "BADCD"),
  stringsAsFactors = FALSE
)
# SD1016: IE domain with IETESTCD/IETEST/IECAT (row 2 combination invalid)
ie_combo <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "IE",
  rownumber_new = as.character(1:3),
  IESEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  IETESTCD = c("INCL01", "INCL01", "EXCL01"),
  IETEST = c("Inclusion 1", "Inclusion 1", "Exclusion 1"),
  IECAT = c("INCLUSION", "EXCLUSION", "EXCLUSION"),
  stringsAsFactors = FALSE
)
# SD1017: SV domain with VISITNUM and SVUPDES (row 2 unplanned but invalid)
sv_visit <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  rownumber_new = as.character(1:3),
  SVSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  VISITNUM = c("1", "99", "5"),
  SVUPDES = c("", "", "PROTOCOL DEVIATION"),
  stringsAsFactors = FALSE
)
# SD1017: compliant SV domain
sv_visit_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SV",
  rownumber_new = as.character(1:3),
  SVSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  VISITNUM = c("1", "99", "3"),
  SVUPDES = c("", "UNEXPECTED", ""),
  stringsAsFactors = FALSE
)
# SD1354: TA domain with ARMCD (row 2 invalid, row 3 blank, row 4 duplicate invalid)
ta_armcd <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  rownumber_new = as.character(1:4),
  TASEQ = 1:4,
  ARMCD = c("ARM1", "ARM3", "", "ARM3"),
  TAETORD = c("1", "1", "1", "1"),
  stringsAsFactors = FALSE
)
# SD1354: TA domain without distinct_check (rows 2 and 4 both reported)
ta_armcd_dup <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  rownumber_new = as.character(1:4),
  TASEQ = 1:4,
  ARMCD = c("ARM1", "ARM3", "ARM9", "ARM3"),
  TAETORD = c("1", "1", "1", "1"),
  stringsAsFactors = FALSE
)
# SD1378: TE domain with ETCD (row 2 invalid, row 3 blank skipped)
te_etcd <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TE",
  rownumber_new = as.character(1:3),
  TESEQ = 1:3,
  ETCD = c("EL1", "EL9", ""),
  ELEMENT = c("Element 1", "Element 9", ""),
  stringsAsFactors = FALSE
)
# SD1023: LB domain with VISIT/VISITNUM (rows 2 and 4 invalid)
lb_visit <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:5),
  LBSEQ = 1:5,
  USUBJID = c("S01", "S02", "S03", "S04", "S05"),
  VISITNUM = c("1", "1", "99", "4", NA),
  VISIT = c("SCREENING", "SCREEING", "UNSCHEDULED VISIT", "VISIT 4", "SCREENING"),
  stringsAsFactors = FALSE
)
# SD1023: compliant LB domain
lb_visit_ok <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "LB",
  rownumber_new = as.character(1:3),
  LBSEQ = 1:3,
  USUBJID = c("S01", "S02", "S03"),
  VISITNUM = c("1", "2", "99"),
  VISIT = c("SCREENING", "TREATMENT", "UNSCHEDULED VISIT"),
  stringsAsFactors = FALSE
)
# Boundary: dataset where no row satisfies the conditions
se_all_unplan <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "SE",
  rownumber_new = as.character(1:2),
  SESEQ = 1:2,
  USUBJID = c("S01", "S02"),
  ETCD = c("UNPLAN", "UNPLAN"),
  stringsAsFactors = FALSE
)
# Boundary: dataset where all keys are blank with skip_missing_key
ta_all_blank <- data.frame(
  STUDYID = "STUDY01",
  DOMAIN = "TA",
  rownumber_new = as.character(1:2),
  TASEQ = 1:2,
  ARMCD = c(NA, ""),
  TAETORD = c("1", "1"),
  stringsAsFactors = FALSE
)
# ---- Tests ----
# SD0066: Invalid ARMCD not in TA, excluding SCRNFAIL and NOTASSGN
test_that("SD0066: catches DM ARMCD not found in TA excluding SCRNFAIL/NOTASSGN", {
  expected_error <- data.frame(
    Row = "4",
    Variable = "ARMCD",
    Original_Value = "WRONGCD",
    Rule_ID = "SD0066",
    Message = "ARMCD value(s) not found in TA(ARMCD) (only checked when ARMCD is not_equal and ARMCD is not_equal).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = dm_armcd,
    ref_dfs = list(TA = ta),
    domain_name = "DM",
    ref_domain = "TA",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD0066",
    cond_vars = "ARMCD",
    cond_ops = c("not_equal", "not_equal"),
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0066: returns NULL when all ARMCD valid or excluded", {
  dm_ok <- dm_armcd
  dm_ok$ARMCD[4] <- "ARM2"
  res <- check_cross_lookup_match(
    main_df = dm_ok,
    ref_dfs = list(TA = ta),
    domain_name = "DM",
    ref_domain = "TA",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD0066",
    cond_vars = "ARMCD",
    cond_ops = c("not_equal", "not_equal"),
    cond_vals = c("SCRNFAIL", "NOTASSGN"),
    logic_op = "AND"
  )
  expect_null(res)
})
# SD0067: Invalid ETCD not in TE, excluding UNPLAN
test_that("SD0067: catches SE ETCD not found in TE excluding UNPLAN", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "ETCD",
    Original_Value = "BADCD",
    Rule_ID = "SD0067",
    Message = "ETCD value(s) not found in TE(ETCD) (only checked when ETCD is not_equal).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = se,
    ref_dfs = list(TE = te),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN",
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0067: returns NULL when all ETCD valid or UNPLAN", {
  se_ok <- se
  se_ok$ETCD[3] <- "EL2"
  res <- check_cross_lookup_match(
    main_df = se_ok,
    ref_dfs = list(TE = te),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN",
    logic_op = "AND"
  )
  expect_null(res)
})
# SD0068: IETESTCD must exist in TI
test_that("SD0068: catches IE IETESTCD not found in TI", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "IETESTCD",
    Original_Value = "BADCD",
    Rule_ID = "SD0068",
    Message = "IETESTCD value(s) not found in TI(IETESTCD).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ie_cd,
    ref_dfs = list(TI = ti),
    domain_name = "IE",
    ref_domain = "TI",
    target_vars = "IETESTCD",
    ref_vars = "IETESTCD",
    rule_id = "SD0068"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD0068: returns NULL when all IETESTCD found in TI", {
  ie_ok <- ie_cd
  ie_ok$IETESTCD[3] <- "INCL01"
  res <- check_cross_lookup_match(
    main_df = ie_ok,
    ref_dfs = list(TI = ti),
    domain_name = "IE",
    ref_domain = "TI",
    target_vars = "IETESTCD",
    ref_vars = "IETESTCD",
    rule_id = "SD0068"
  )
  expect_null(res)
})
# SD1016: IETESTCD/IETEST/IECAT combination must exist in TI
test_that("SD1016: catches IE combination not found in TI", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "IETESTCD, IETEST, IECAT",
    Original_Value = "INCL01 | Inclusion 1 | EXCLUSION",
    Rule_ID = "SD1016",
    Message = "IETESTCD, IETEST, IECAT value(s) not found in TI(IETESTCD, IETEST, IECAT).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ie_combo,
    ref_dfs = list(TI = ti),
    domain_name = "IE",
    ref_domain = "TI",
    target_vars = c("IETESTCD", "IETEST", "IECAT"),
    ref_vars = c("IETESTCD", "IETEST", "IECAT"),
    rule_id = "SD1016"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1016: returns NULL when all combinations found in TI", {
  ie_ok <- ie_combo
  ie_ok$IECAT[2] <- "INCLUSION"
  res <- check_cross_lookup_match(
    main_df = ie_ok,
    ref_dfs = list(TI = ti),
    domain_name = "IE",
    ref_domain = "TI",
    target_vars = c("IETESTCD", "IETEST", "IECAT"),
    ref_vars = c("IETESTCD", "IETEST", "IECAT"),
    rule_id = "SD1016"
  )
  expect_null(res)
})
# SD1017: VISITNUM must match TV when SVUPDES is missing
test_that("SD1017: catches SV VISITNUM not found in TV when SVUPDES missing", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "VISITNUM",
    Original_Value = "99",
    Rule_ID = "SD1017",
    Message = "VISITNUM value(s) not found in TV(VISITNUM) (only checked when SVUPDES is missing).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = sv_visit,
    ref_dfs = list(TV = tv),
    domain_name = "SV",
    ref_domain = "TV",
    target_vars = "VISITNUM",
    ref_vars = "VISITNUM",
    rule_id = "SD1017",
    cond_vars = "SVUPDES",
    cond_ops = "missing",
    cond_vals = "",
    logic_op = "AND"
  )
  expect_null(res)
})
test_that("SD1017: returns NULL when unplanned visits excluded and planned valid", {
  res <- check_cross_lookup_match(
    main_df = sv_visit_ok,
    ref_dfs = list(TV = tv),
    domain_name = "SV",
    ref_domain = "TV",
    target_vars = "VISITNUM",
    ref_vars = "VISITNUM",
    rule_id = "SD1017",
    cond_vars = "SVUPDES",
    cond_ops = "missing",
    cond_vals = "",
    logic_op = "AND"
  )
  expect_null(res)
})
# SD1354: TA ARMCD must be assigned to at least one DM subject
test_that("SD1354: catches TA ARMCD not in DM, skipping blank keys and duplicates", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ARMCD",
    Original_Value = "ARM3",
    Rule_ID = "SD1354",
    Message = "ARMCD value(s) not found in DM(ARMCD).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ta_armcd,
    ref_dfs = list(DM = dm_ref),
    domain_name = "TA",
    ref_domain = "DM",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD1354",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1354: reports duplicate invalid keys separately without distinct_check", {
  expected_error <- data.frame(
    Row = c("2", "3"),
    Variable = c("ARMCD", "ARMCD"),
    Original_Value = c("ARM3", "ARM9"),
    Rule_ID = c("SD1354", "SD1354"),
    Message = c(
      "ARMCD value(s) not found in DM(ARMCD).",
      "ARMCD value(s) not found in DM(ARMCD)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ta_armcd_dup,
    ref_dfs = list(DM = dm_ref),
    domain_name = "TA",
    ref_domain = "DM",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD1354",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1354: returns NULL when all non-blank ARMCD found in DM", {
  ta_ok <- ta_armcd
  ta_ok$ARMCD <- c("ARM1", "ARM2", "", "ARM1")
  res <- check_cross_lookup_match(
    main_df = ta_ok,
    ref_dfs = list(DM = dm_ref),
    domain_name = "TA",
    ref_domain = "DM",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD1354",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_null(res)
})
# SD1378: TE ETCD must be used in TA
test_that("SD1378: catches TE ETCD not found in TA, skipping blank keys", {
  expected_error <- data.frame(
    Row = "2",
    Variable = "ETCD",
    Original_Value = "EL9",
    Rule_ID = "SD1378",
    Message = "ETCD value(s) not found in TA(ETCD).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = te_etcd,
    ref_dfs = list(TA = ta),
    domain_name = "TE",
    ref_domain = "TA",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD1378",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1378: returns NULL when all non-blank ETCD found in TA", {
  te_ok <- te_etcd
  te_ok$ETCD[2] <- "EL2"
  res <- check_cross_lookup_match(
    main_df = te_ok,
    ref_dfs = list(TA = ta),
    domain_name = "TE",
    ref_domain = "TA",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD1378",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_null(res)
})
# SD1023: VISIT/VISITNUM combination must match TV except unscheduled/unplanned
test_that("SD1023: catches LB VISIT/VISITNUM combination not found in TV", {
  expected_error <- data.frame(
    Row = c("2", "4"),
    Variable = c("VISIT, VISITNUM", "VISIT, VISITNUM"),
    Original_Value = c("SCREEING | 1", "VISIT 4 | 4"),
    Rule_ID = c("SD1023", "SD1023"),
    Message = c(
      "VISIT, VISITNUM value(s) not found in TV(VISIT, VISITNUM) (only checked when VISITNUM is non_missing and VISIT is not_contains and VISIT is not_contains).",
      "VISIT, VISITNUM value(s) not found in TV(VISIT, VISITNUM) (only checked when VISITNUM is non_missing and VISIT is not_contains and VISIT is not_contains)."
    ),
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = lb_visit,
    ref_dfs = list(TV = tv),
    domain_name = "LB",
    ref_domain = "TV",
    target_vars = c("VISIT", "VISITNUM"),
    ref_vars = c("VISIT", "VISITNUM"),
    rule_id = "SD1023",
    cond_vars = c("VISITNUM", "VISIT", "VISIT"),
    cond_ops = c("non_missing", "not_contains", "not_contains"),
    cond_vals = c("", "UNSCHEDULED", "UNPLANNED"),
    logic_op = "AND"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
test_that("SD1023: returns NULL when all combinations match TV or are excluded", {
  res <- check_cross_lookup_match(
    main_df = lb_visit_ok,
    ref_dfs = list(TV = tv),
    domain_name = "LB",
    ref_domain = "TV",
    target_vars = c("VISIT", "VISITNUM"),
    ref_vars = c("VISIT", "VISITNUM"),
    rule_id = "SD1023",
    cond_vars = c("VISITNUM", "VISIT", "VISIT"),
    cond_ops = c("non_missing", "not_contains", "not_contains"),
    cond_vals = c("", "UNSCHEDULED", "UNPLANNED"),
    logic_op = "AND"
  )
  expect_null(res)
})
# Boundary: no row satisfies the conditions
test_that("Boundary: returns NULL when no row satisfies the conditions", {
  res <- check_cross_lookup_match(
    main_df = se_all_unplan,
    ref_dfs = list(TE = te),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN",
    logic_op = "AND"
  )
  expect_null(res)
})
# Boundary: all keys are blank with skip_missing_key
test_that("Boundary: returns NULL when all keys are blank and skip_missing_key is TRUE", {
  res <- check_cross_lookup_match(
    main_df = ta_all_blank,
    ref_dfs = list(DM = dm_ref),
    domain_name = "TA",
    ref_domain = "DM",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD1354",
    distinct_check = TRUE,
    skip_missing_key = TRUE
  )
  expect_null(res)
})
# Boundary: blank keys are reported when skip_missing_key is FALSE
test_that("Boundary: reports blank keys as <NA> when skip_missing_key is FALSE", {
  expected_error <- data.frame(
    Row = c("2", "3"),
    Variable = "ARMCD",
    Original_Value = c("ARM3","<NA>"),
    Rule_ID = "SD1354",
    Message = "ARMCD value(s) not found in DM(ARMCD).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ta_armcd,
    ref_dfs = list(DM = dm_ref),
    domain_name = "TA",
    ref_domain = "DM",
    target_vars = "ARMCD",
    ref_vars = "ARMCD",
    rule_id = "SD1354",
    distinct_check = TRUE,
    skip_missing_key = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: empty main dataset
test_that("Boundary: returns NULL when main_df is empty", {
  res <- check_cross_lookup_match(
    main_df = se[0, ],
    ref_dfs = list(TE = te),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN"
  )
  expect_null(res)
})
# Boundary: reference domain missing from ref_dfs list
test_that("Boundary: reports error when reference domain is missing", {
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0067",
    Message = "Reference domain TE is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = se,
    ref_dfs = list(TA = ta),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: reference dataset empty
test_that("Boundary: reports error when reference dataset is empty", {
  te_empty <- te[0, ]
  expected_error <- data.frame(
    Row = "-",
    Variable = "-",
    Original_Value = "-",
    Rule_ID = "SD0067",
    Message = "Reference domain TE is missing or empty.",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = se,
    ref_dfs = list(TE = te_empty),
    domain_name = "SE",
    ref_domain = "TE",
    target_vars = "ETCD",
    ref_vars = "ETCD",
    rule_id = "SD0067",
    cond_vars = "ETCD",
    cond_ops = "not_equal",
    cond_vals = "UNPLAN"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})
# Boundary: ref_vars defaults to target_vars when NULL
test_that("Boundary: ref_vars defaults to target_vars when NULL", {
  expected_error <- data.frame(
    Row = "3",
    Variable = "IETESTCD",
    Original_Value = "BADCD",
    Rule_ID = "SD0068",
    Message = "IETESTCD value(s) not found in TI(IETESTCD).",
    stringsAsFactors = FALSE
  )
  res <- check_cross_lookup_match(
    main_df = ie_cd,
    ref_dfs = list(TI = ti),
    domain_name = "IE",
    ref_domain = "TI",
    target_vars = "IETESTCD",
    rule_id = "SD0068"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$Row, expected_error$Row)
  expect_equal(res$Variable, expected_error$Variable)
  expect_equal(res$Original_Value, expected_error$Original_Value)
  expect_equal(res$Rule_ID, expected_error$Rule_ID)
  expect_equal(res$Message, expected_error$Message)
})