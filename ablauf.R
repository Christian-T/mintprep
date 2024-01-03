
-embed solution files in package
-if bridges v4 <- sonderfunktion
-if fs - sonderfunktion

dann einmal durchspielen 
dann auf github


A typical workflow

1. read in your test data
2. calculate scores via calulate_sumscore
3. read in student sensitive data
4. combine files via sens <- selection_binder(LL_pr_v2, "_ll_pr2", sens)




usethis::use_data(internal_this, internal_that, internal = TRUE)

devtools::load_all()


usethis::create_from_github(
  "https://github.com/Christian-T/mintprep.git",
  destdir = "//gess-fs.d.ethz.ch/home$/thurnc/Documents/Project JACOBS"
)
