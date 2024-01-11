count_tests <- function(pip_temp){

  pip_tests <- select(pip_temp, starts_with("date"))

  #count the tests here
  CountTests <- apply(pip_tests, c(1,2), function(x) str_count(x, ",")+1 * NA^!nzchar(x))
  CountTests <- as.data.frame(CountTests)

  name <- names(CountTests)
  name <- gsub("date_","",name)
  names(CountTests) <- name
  CountTests$student_id <- pip_temp$student_id

  return(CountTests)
}
