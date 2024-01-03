
xembed solution files in package
xif bridges v4 <- sonderfunktion
xif fs - sonderfunktion

dann einmal durchspielen
dann auf github

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



