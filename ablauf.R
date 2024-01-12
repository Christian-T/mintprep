
xembed solution files in package
xif bridges v4 <- sonderfunktion
xif fs - sonderfunktion



library(car)
library(data.table)
library(dplyr)

usethis::use_data(solution_b_pr_v1, solution_b_pr_v2,
                  solution_b_pr_v3, solution_b_pr_v4,
                  solution_b_po_v2, solution_b_po_v3,
                  solution_b_po_v4,
                  solution_ll_pr_v1, solution_ll_pr_v2,
                  solution_ll_pr_v3,
                  solution_ll_po_v2, solution_ll_po_v3,
                  solution_s_pr_v1, solution_s_po_v1,
                  internal = TRUE)

usethis::use_git()

devtools::load_all()



#usecase
library(mintprep3)
library(plyr)
library(tidyr)
library(dplyr)
library(stringr)
sens <- read.delim("//gess-fs.d.ethz.ch/home$/thurnc/Documents/Database MINT/Welches Kind hat welchen Test gemacht/sensdata_id-_2023_10_11_8_39_12.tsv")
filepath <- "//gess-fs.d.ethz.ch/home$/thurnc/Documents/Database MINT/Welches Kind hat welchen Test gemacht/Test Data Oktober 2023"
pip <- child_test_mapping(filepath,sens)
pip2 <- mintprep3::count_tests(pip)
pip2

b_pr_v2 <- read.delim("//gess-fs.d.ethz.ch/home$/thurnc/Documents/Database MINT/Welches Kind hat welchen Test gemacht/Test Data Oktober 2023/testdata_test-33_2023_10_11_8_30_30.tsv")
scored_b_pr_v2 <- calculate_mintscore(b_pr_v2)

b_po_v3 <- read.delim("//gess-fs.d.ethz.ch/home$/thurnc/Documents/Database MINT/Welches Kind hat welchen Test gemacht/Test Data Oktober 2023/testdata_test-30_2023_10_11_8_29_56.tsv")
scored_b_po_v3 <- calculate_mintscore(b_po_v3)


sens <- selection_binder(scored_b_pr_v2, "_b_pr2", sens)
sens <- selection_binder(scored_b_po_v3, "_b_po3", sens)



