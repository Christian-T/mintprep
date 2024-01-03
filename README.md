# mintprep
This is a package for the Swiss MINT Study. It helps in scoring and combining data from the students


## Installation
``` r
library(devtools)
devtools::install_github("Christian-T/genderplot", force = TRUE)
library(genderplot)
```

## A typical workflow

1. Read in your test data via ` name_of_data  <- read.csv(path_to_file, sep = "\t")`
2. Calculate scores via `r calculate_mintscore(name_of_data)` for Bridges, Airpressure, and Sound. Calculate scores via `r calclate_icu(name_of_data)` for Floating and Sinking
3. Use the function `r calculate_sumscore(name_of_data, name_of_solutionfile)` to calculate scores for other tests such as CVS
4. Read in student sensitive data
5. Combine files via `r sens <- selection_binder(LL_pr_v2, "_ll_pr2", sens)`
for all files you want to merge. 


##Citation
--------

To cite package `mintprep` in publications use:

Thurn, C. M. (2024). Package "mintprep". Availabe at https://github.com/Christian-T/mintprep
