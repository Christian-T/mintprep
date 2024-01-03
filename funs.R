#recode NAs
recode_to_zero <- function(dat){
  demographics <- dat[ ,c(1:19)]
  data <- dat[ ,c(20:length(dat))]
  data[is.na(data)] <- 0  #then recode single missings to 0
  dat <- cbind(demographics, data)
}

# Berechnungsmethode mit Funktion s
s <- function(x, na.rm = FALSE) {
  if (!na.rm) return(sum(x))
  if (all(is.na(x))) {
    o <- NA
    class(o) <- class(x)
    return(o)}
  sum(x, na.rm = TRUE)
}



#compare with solution
calculate_sumscore <- function(dat,  solution_data){
  #Add variable string parameters (because R cannot use names starting with numbers)
  solution_data$Antwort_id <- gsub("#",".",solution_data$Antwort_id)
  solution_data$Antwort_id <-paste0("X",solution_data$Antwort_id)
  
  questions <- unique(solution_data$Frage)
  if("[opentext]" %in% questions) questions <- questions[-which(questions=="[opentext]")]
  
  dat <- recode_to_zero(dat)
  
  #new variables
  dat[ ,questions] <- NA
  
  for( i in 1:nrow(dat)){
    for( var in questions){
      answers <- unlist(solution_data[solution_data$Frage==var,]$Antwort_id)
      solutions <- unlist(solution_data[solution_data$Frage==var,]$Korrekt)
      if( all(( dat[i, answers] == solutions) == TRUE))  dat[i, var] <- 1 
    }}#end of both fors
  
  # if contains _Transfer, build an extra sum (Transfer) for these questions
  if( length(grep("Transfer",questions))>0){
    sum_questions <- questions[-grep("Transfer",questions)]
    transferquestions <- questions[grep("Transfer",questions)]
    v <- dat[,sum_questions]
    t <- dat[,transferquestions]
    for(i in 1:nrow(dat)){
      dat$Sum[i] <- s(v[i,],na.rm=T)
      dat$Transfer[i] <- s(t[i,], na.rm=T)
      # TODO: some question names in Posttest need to be renamed to _Transfer
      
    }} else{
      v <- dat[,questions]
      for(i in 1:nrow(dat)){
        dat$Sum[i] <- s(v[i,],na.rm=T)
      }
    }
  
  #clean data
  data <- dat
  data$date <- paste(data$test_day, data$test_month, data$test_year, sep=".")
  #keep important columns; return Transfer, if it exists
  if( length(grep("Transfer",questions))>0){
    dat <- data[,c("test_id","school_id","teacher_id","date","CYCLESTARTDATE","student_id","class_name","student_grade","student_age",  "student_gender","group", questions, "Sum","Transfer")]
  }else{
    dat <- data[,c("test_id","school_id","teacher_id","date","CYCLESTARTDATE","student_id","class_name","student_grade","student_age",  "student_gender","group", questions, "Sum")]}
  
  return(dat)
}


selection_binder <- function(data, topicversion, sens){
  
  dat <- data[ ,c("student_id","school_id","date","CYCLESTARTDATE","student_grade","Sum")]  #CSD nötig??
  dat$date <- as.Date(dat$date, format="%d.%m.%Y")
  dat2 <- dat %>% group_by(student_id) %>% arrange(desc(date)) %>%  filter(row_number() ==1) %>% ungroup
  
  names(dat2)[2:6] <- paste0(names(dat2)[2:6],topicversion)
  names(dat2)[1] <- "student_ID"
  
  dat3 <- merge(sens, dat2, by="student_ID",all=TRUE)
  return(dat3)
  
}

