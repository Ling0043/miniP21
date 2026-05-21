# MiniP21

This is a R tool to check SDTM datasets

## Installation
To get a bug fix or to use a feature from the development version, you can install the development version of dplyr from GitHub.


```
pak::pak("Ling0043/miniP21")
library(miniP21)
```

## Usage

Input an SDTM dataset (e.g., ta, te, ts, etc.) to the  **`fct_validate_sdtm(df)`** function.

> e.g. fct_validate_sdtm(ta)

Upon execution completion, a validation report including a summary and specific error sources will be generated in the working directory. Reports are typically named following the convention: Domain Name + P21 Report + Timestamp.html.

> e.g. TA_P21_Report_20260518.html

