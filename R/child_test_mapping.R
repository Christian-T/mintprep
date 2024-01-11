#' Mapping students to tests and schools
#' @description
#' returns some nice file
child_test_mapping <- function(filepath, sens_data){

  files <-list.files(filepath, recursive=TRUE, full.names = TRUE)
  data <- lapply(files, read.delim)

  lapply(data, function (x) x[c('student_id','test_id','school_id','teacher_id','test_year','test_month', 'test_day', 'group')]) -> mar3
  mar4 <- do.call("rbind", mar3)
  tail(mar4)#that is already quite good!
  mar4$date <- paste(mar4$test_day,mar4$test_month,mar4$test_year, sep=".")
  mar4$date <- as.Date(mar4$date, format="%d.%m.%Y")

  mar4 <- mar4 %>% filter(group =="Treatment 1")

  mar4$school_id_teacher_id_year <-   paste(mar4$school_id,mar4$teacher_id,mar4$test_year,sep="_")
  #Schulen, Teachers und Students exportieren und auch abspeichern (aber in einem anderen Pfad)
 pip <- data.frame( sens_data[,"student_ID"])

  names(pip) <- "student_id"
  pip$student_id <- as.numeric(pip$student_id)

  pip_temp <- pip



  mar4 <- mar4[,c("student_id","test_id","school_teacher_year", "group", "date")]


  for(i in unique(mar4$test_id)){
    sub <- subset(mar4, mar4$test_id ==i)
    #flatten duplicate lists -> find all duplicate values and append them to the one original (first)
    sub <- aggregate(sub[,2:4], list(sub[,1]), function(x) paste0(unique(x)))  #and that works like a charm :)
    names(sub)[1] <- "student_id" #it gets renamed to "group.1
    pip_temp <- merge(pip_temp, sub, by="student_id", all=TRUE)
    pip_temp <- subset(pip_temp, select=-test_id)
    names <- names(pip_temp)
    names(pip_temp) <- c(names[1:(length(names)-2)], paste0("school_teacher_year_",i),paste0("date_",i))
  }

  pip_temp[pip_temp == "NA"] <- ""
  return(pip_temp)
}
