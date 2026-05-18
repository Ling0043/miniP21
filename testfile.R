install.packages(c("devtools", "usethis", "roxygen2", "testthat"))


usethis::edit_r_environ()
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

devtools::load_all()
devtools::document()


renv::status()
renv::snapshot()

# a test
ta <- data.frame(
  Row = 1:6,
  STUDYID = c("EX7", "EX7", "EX7", "EX7", "EX7", "EX7"),
  DOMAIN = c("TA", "TA", "TA", "TA", "TA", "TA"),
  ARMCD = c(1, 1, 1, 1, 1, 1),
  ARM = c("CR", "CR", NA, "CR", "", "CR"),
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

fct_validate_sdtm(ta)
renv::snapshot()

usethis::use_git()
