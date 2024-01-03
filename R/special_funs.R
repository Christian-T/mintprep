


for(i in 1:nrow(bpr_v4)){
  A1 <- bpr_v4[i,c("X01.brueckennamen.1","X01.brueckennamen.2","X01.brueckennamen.3","X01.brueckennamen.4")]
  A1[is.na(A1)] <- 0 
  if(all(inrange(unlist(A1),0,1))) {   #min(A1)==0 & max(A1)==1){
    bpr_v4$X01.brueckennamen.1[i] <- car::recode(bpr_v4$X01.brueckennamen.1[i],"1=2"); 
    #1=1;
    bpr_v4$X01.brueckennamen.3[i] <- car::recode(bpr_v4$X01.brueckennamen.3[i],"1=4"); 
    bpr_v4$X01.brueckennamen.4[i] <- car::recode(bpr_v4$X01.brueckennamen.4[i],"1=3") }
  
  A2 <- bpr_v4[i, c("X02.brueckenteile.1", "X02.brueckenteile.2","X02.brueckenteile.3","X02.brueckenteile.4")]
  A2[is.na(A2)] <- 0 
  if(all(inrange(unlist(A2),0,1))){ #min(A2)==0 & max(A2)==1){
    bpr_v4$X02.brueckenteile.1[i] <- car::recode(bpr_v4$X02.brueckenteile.1[i],"1=3");
    #1=1
    bpr_v4$X02.brueckenteile.3[i] <- car::recode(bpr_v4$X02.brueckenteile.3[i],"1=2");
    bpr_v4$X02.brueckenteile.4[i] <- car::recode(bpr_v4$X02.brueckenteile.4[i],"1=3")}
  
}


for(i in 1:nrow(bpo_v4)){
  A1 <- bpo_v4[i,c("X01.brueckennamen.1","X01.brueckennamen.2","X01.brueckennamen.3","X01.brueckennamen.4")]
  A1[is.na(A1)] <- 0 
  if(all(inrange(unlist(A1),0,1))){ #min(A1)==0 & max(A1)==1){
    bpo_v4$X01.brueckennamen.1[i] <- car::recode(bpo_v4$X01.brueckennamen.1[i],"1=2"); 
    #1=1;
    bpo_v4$X01.brueckennamen.3[i] <- car::recode(bpo_v4$X01.brueckennamen.3[i],"1=4"); 
    bpo_v4$X01.brueckennamen.4[i] <- car::recode(bpo_v4$X01.brueckennamen.4[i],"1=3") }
  
  A2 <- bpo_v4[i, c("X02.brueckenteile.1", "X02.brueckenteile.2","X02.brueckenteile.3","X02.brueckenteile.4")]
  A2[is.na(A2)] <- 0 
  if(all(inrange(unlist(A2),0,1))){ #min(A2)==0 & max(A2)==1){
    bpo_v4$X02.brueckenteile.1[i] <- car::recode(bpo_v4$X02.brueckenteile.1[i],"1=3");
    #1=1
    bpo_v4$X02.brueckenteile.3[i] <- car::recode(bpo_v4$X02.brueckenteile.3[i],"1=2");
    bpo_v4$X02.brueckenteile.4[i] <- car::recode(bpo_v4$X02.brueckenteile.4[i],"1=3")}
  
}



scores_floatsink_pr <- function(data){
  
  # t <- read.csv(list.files(pattern = "testdata_test-22_*"), sep = "\t")
  
  ##Misconceptions
  ssprA$misconceptions <- rowSums(ssprA[, c("X01.wuerfel.2", "X01.wuerfel.3",
                                            "X04.tauch_begruendung.4", "X04.tauch_begruendung.5", "X04.tauch_begruendung.6",
                                            "X04.tauch_begruendung.11", "X04.tauch_begruendung.13", "X04.tauch_begruendung.16",
                                            "X04.tauch_begruendung.19", "X04.tauch_begruendung.21", "X04.tauch_begruendung.22", "X04.tauch_begruendung.24",
                                            "X04.tauch_begruendung.27", "X04.tauch_begruendung.30", "X04.tauch_begruendung.32",
                                            "X04.tauch_begruendung.35", "X04.tauch_begruendung.36", "X04.tauch_begruendung.38",
                                            "X04.tauch_begruendung.43", "X04.tauch_begruendung.45", "X04.tauch_begruendung.46",
                                            "X09.saetze_richtigkeit.1", "X09.saetze_richtigkeit.2", "X09.saetze_richtigkeit.3",
                                            "X09.saetze_richtigkeit.5", "X09.saetze_richtigkeit.7",
                                            "X12.was_macht_wasser.1", "X12.was_macht_wasser.2", "X12.was_macht_wasser.5",  
                                            "X13.ball_ss.1", "X13.ball_ss.2", "X13.ball_ss.3", "X13.ball_ss.5",
                                            "X14.schiff_aus_eisen.1", "X14.schiff_aus_eisen.3", "X14.schiff_aus_eisen.5", "X14.schiff_aus_eisen.6"
  )])
  ssprA$misconceptions <- ifelse(ssprA$X02.kugeln_wasserglas.1 == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA$X02.kugeln_wasserglas.2 == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA$X02.kugeln_wasserglas.3 == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X08.verschiedene_wuerfel.4"] == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X08.verschiedene_wuerfel.7"] == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X08.verschiedene_wuerfel.11"] == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X11.verschiedene_kugeln.1"] == 0 & ssprA["X11.verschiedene_kugeln.2"] == 1, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X11.verschiedene_kugeln.3"] == 0 & ssprA["X11.verschiedene_kugeln.4"] == 1, ssprA$misconceptions + 1, ssprA$misconceptions)
  ssprA$misconceptions <- ifelse(ssprA["X11.verschiedene_kugeln.5"] == 1 & ssprA["X11.verschiedene_kugeln.6"] == 0, ssprA$misconceptions + 1, ssprA$misconceptions)
  
  ##Everyday conceptions
  ssprA$everydayconceptions <- rowSums(ssprA[, c("X04.tauch_begruendung.7", "X04.tauch_begruendung.12", "X04.tauch_begruendung.29", "X04.tauch_begruendung.37",
                                                 "X04.tauch_begruendung.44",
                                                 "X07.klein_und_gross.2", "X07.klein_und_gross.4", "X07.klein_und_gross.6",
                                                 "X07.klein_und_gross.7", "X07.klein_und_gross.9",
                                                 "X14.schiff_aus_eisen.4")])
  
  ##Scientific concepts
  ssprA$scientificconcepts <- rowSums(ssprA[, c("X04.tauch_begruendung.3", "X04.tauch_begruendung.8",
                                                "X04.tauch_begruendung.14", "X04.tauch_begruendung.15",
                                                "X04.tauch_begruendung.20", "X04.tauch_begruendung.23",
                                                "X04.tauch_begruendung.28", "X04.tauch_begruendung.31",
                                                "X04.tauch_begruendung.39", "X04.tauch_begruendung.40",
                                                "X04.tauch_begruendung.47", "X04.tauch_begruendung.48",
                                                "X09.saetze_richtigkeit.4", "X09.saetze_richtigkeit.6",
                                                "X12.was_macht_wasser.3", "X12.was_macht_wasser.4",
                                                "X13.ball_ss.4",
                                                "X14.schiff_aus_eisen.2", "X14.schiff_aus_eisen.7")])
  
  ##ICU
  
  ssprA$ICU_1 <- c(ifelse(ssprA$X02.kugeln_wasserglas.1 == 1 & ssprA$X02.kugeln_wasserglas.2 == 1 & ssprA$X02.kugeln_wasserglas.3 == 1, 1, 0))
  ssprA$ICU_2 <- c(ifelse((ssprA$X04.tauch_begruendung.4 == 0 & ssprA$X04.tauch_begruendung.5 == 0 & ssprA$X04.tauch_begruendung.6 == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.3", "X04.tauch_begruendung.8")]) > 0), 1, 0))
  ssprA$ICU_3 <- c(ifelse(rowSums(ssprA[, c("X04.tauch_begruendung.11", "X04.tauch_begruendung.13", "X04.tauch_begruendung.16")]) == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.14", "X04.tauch_begruendung.15")]) > 0, 1, 0))
  ssprA$ICU_4 <- c(ifelse(rowSums(ssprA[, c("X04.tauch_begruendung.19", "X04.tauch_begruendung.21", "X04.tauch_begruendung.22", "X04.tauch_begruendung.24")]) == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.20", "X04.tauch_begruendung.23")]) > 0, 1, 0))
  ssprA$ICU_5 <- c(ifelse(rowSums(ssprA[, c("X04.tauch_begruendung.27", "X04.tauch_begruendung.30", "X04.tauch_begruendung.32")]) == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.28", "X04.tauch_begruendung.31")]) > 0, 1, 0))
  ssprA$ICU_6 <- c(ifelse(rowSums(ssprA[, c("X04.tauch_begruendung.35", "X04.tauch_begruendung.36", "X04.tauch_begruendung.38")]) == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.39", "X04.tauch_begruendung.40")]) > 0, 1, 0))
  ssprA$ICU_7 <- c(ifelse(rowSums(ssprA[, c("X04.tauch_begruendung.43", "X04.tauch_begruendung.45", "X04.tauch_begruendung.46")]) == 0 & rowSums(ssprA[, c("X04.tauch_begruendung.47", "X04.tauch_begruendung.48")]) > 0, 1, 0))
  ssprA$ICU_8 <- c(ifelse(rowSums(ssprA[, c("X07.klein_und_gross.2", "X07.klein_und_gross.4", "X07.klein_und_gross.6")]) == 3, 1, 0))
  ssprA$ICU_9 <- c(ifelse(rowSums(ssprA[, c("X07.klein_und_gross.7", "X07.klein_und_gross.9")]) == 2, 1, 0))
  ssprA$ICU_10 <- c(ifelse(ssprA["X08.verschiedene_wuerfel.1"] == 0 & ssprA["X08.verschiedene_wuerfel.2"] == 0 & ssprA["X08.verschiedene_wuerfel.3"] == 0 & ssprA["X08.verschiedene_wuerfel.4"] == 1 & ssprA["X08.verschiedene_wuerfel.5"] == 0 & ssprA["X08.verschiedene_wuerfel.6"] == 0 & ssprA["X08.verschiedene_wuerfel.7"] == 1 & ssprA["X08.verschiedene_wuerfel.8"] == 0 & ssprA["X08.verschiedene_wuerfel.9"] == 0 & ssprA["X08.verschiedene_wuerfel.10"] == 0 & ssprA["X08.verschiedene_wuerfel.11"] == 1 & ssprA["X08.verschiedene_wuerfel.12"] == 0, 1, 0))
  ssprA$ICU_11 <- c(ifelse(rowSums(ssprA[, c("X09.saetze_richtigkeit.1", "X09.saetze_richtigkeit.2", "X09.saetze_richtigkeit.3", "X09.saetze_richtigkeit.5", "X09.saetze_richtigkeit.7")]) == 0 & rowSums(ssprA[, c("X09.saetze_richtigkeit.4", "X09.saetze_richtigkeit.6")]) > 0, 1, 0))
  ssprA$ICU_12 <- c(ifelse(ssprA["X10.wuerfel_wasserbecken.1"] == 0 & ssprA["X10.wuerfel_wasserbecken.2"] == 1 & ssprA["X10.wuerfel_wasserbecken.3"] == 0, 1, 0))
  ssprA$ICU_13 <- c(ifelse(ssprA["X10.wuerfel_wasserbecken.4"] == 0 & ssprA["X10.wuerfel_wasserbecken.5"] == 0 & ssprA["X10.wuerfel_wasserbecken.6"] == 1, 1, 0))
  ssprA$ICU_14 <- c(ifelse(ssprA["X11.verschiedene_kugeln.1"] == 1 & ssprA["X11.verschiedene_kugeln.2"] == 0 & ssprA["X11.verschiedene_kugeln.3"] == 1 & ssprA["X11.verschiedene_kugeln.4"] == 0 & ssprA["X11.verschiedene_kugeln.5"] == 0 & ssprA["X11.verschiedene_kugeln.6"] == 1, 1, 0))
  ssprA$ICU_15 <- c(ifelse(ssprA["X12.was_macht_wasser.1"] == 0 & ssprA["X12.was_macht_wasser.2"] == 0 & ssprA["X12.was_macht_wasser.3"] == 1 & ssprA["X12.was_macht_wasser.4"] == 1 & ssprA["X12.was_macht_wasser.5"] == 0, 1, 0))
  ssprA$ICU_16 <- c(ifelse(ssprA["X13.ball_ss.1"] == 0 & ssprA["X13.ball_ss.2"] == 0 & ssprA["X13.ball_ss.3"] == 0 & ssprA["X13.ball_ss.4"] == 1 & ssprA["X13.ball_ss.5"] == 0, 1, 0))
  ssprA$ICU_17 <- c(ifelse(ssprA["X14.schiff_aus_eisen.1"] == 0 & ssprA["X14.schiff_aus_eisen.3"] == 0 & ssprA["X14.schiff_aus_eisen.5"] == 0 & ssprA["X14.schiff_aus_eisen.6"] == 0 & (ssprA["X14.schiff_aus_eisen.2"] == 1 | ssprA["X14.schiff_aus_eisen.6"] == 1), 1, 0))
  
  library(dplyr)
  ssprA$ICU <- rowSums(select(ssprA, tidyr::contains("ICU_")))
  
  #Make regular data frame columns from lists resulting from ifelse-functions for MCs and ICU
  ssprA$misconceptions <- c(ssprA$misconceptions)
  ssprA$ICU <- c(ssprA$ICU)
  
  ssprA$Sum <- ssprA$ICU
  
  ssprA$date <- paste(ssprA$test_day, ssprA$test_month, ssprA$test_year, sep=".")
  
  ssprA <- ssprA %>% select("test_id","school_id","teacher_id","date","CYCLESTARTDATE","student_id","class_name","student_grade","student_age",  "student_gender","group", "ICU_1", "ICU_2", 
                            "ICU_3", "ICU_4", "ICU_5", "ICU_6", "ICU_7", "ICU_8", "ICU_9", 
                            "ICU_10", "ICU_11", "ICU_12", "ICU_13", "ICU_14", "ICU_15", "ICU_16", 
                            "ICU_17", "Sum","misconceptions","everydayconceptions",
                            "scientificconcepts")

}


library(tidyr)
# function zum score bilden
Match_score <- function(df, cor_pat){
  
  # create score
  df <- df %>%
    unite("concatcol", names(df), sep = "", remove = F)%>%
    mutate(score = ifelse(concatcol %in% cor_pat, 1, 0))
  
  return(df$score)
}

score <- function(df,df3,x,t){
  data <- df
  for (i in seq_along(x)){
    df <- data
    df3[[paste0(x[i], "_score")]] <- Match_score(df=select(df, tidyr::contains(x[i])),t[i])
    
  }
  return(df3)
}


scores_floatsink_po <- function(data){
  sspoA <- read.csv(list.files(pattern = "testdata_test-21_*"), sep = "\t")
  
  ##Misconceptions
  sspoA$misconceptions <- rowSums(sspoA[, c("X01.wuerfel.2", "X01.wuerfel.3",
                                            "X03.tauch_begruendung.4", "X03.tauch_begruendung.5", "X03.tauch_begruendung.6",
                                            "X03.tauch_begruendung.11", "X03.tauch_begruendung.13", "X03.tauch_begruendung.16",
                                            "X03.tauch_begruendung.19", "X03.tauch_begruendung.21", "X03.tauch_begruendung.22", "X03.tauch_begruendung.24",
                                            "X03.tauch_begruendung.27", "X03.tauch_begruendung.30", "X03.tauch_begruendung.32",
                                            "X03.tauch_begruendung.35", "X03.tauch_begruendung.36", "X03.tauch_begruendung.38",
                                            "X03.tauch_begruendung.43", "X03.tauch_begruendung.45", "X03.tauch_begruendung.46",
                                            "X06.saetze_richtigkeit.1", "X06.saetze_richtigkeit.2", "X06.saetze_richtigkeit.3",
                                            "X06.saetze_richtigkeit.5", "X06.saetze_richtigkeit.7",
                                            "X09.was_macht_wasser.1", "X09.was_macht_wasser.2", "X09.was_macht_wasser.5",  
                                            "X10.ball_ss.1", "X10.ball_ss.2", "X10.ball_ss.3", "X10.ball_ss.5",
                                            "X11.schiff_aus_eisen.1", "X11.schiff_aus_eisen.3", "X11.schiff_aus_eisen.5", "X11.schiff_aus_eisen.6"
  )])
  sspoA$misconceptions <- ifelse(sspoA$X02.kugeln_wasserglas.1 == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA$X02.kugeln_wasserglas.2 == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA$X02.kugeln_wasserglas.3 == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X05.verschiedene_wuerfel.4"] == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X05.verschiedene_wuerfel.7"] == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X05.verschiedene_wuerfel.11"] == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X08.verschiedene_kugeln.1"] == 0 & sspoA["X08.verschiedene_kugeln.2"] == 1, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X08.verschiedene_kugeln.3"] == 0 & sspoA["X08.verschiedene_kugeln.4"] == 1, sspoA$misconceptions + 1, sspoA$misconceptions)
  sspoA$misconceptions <- ifelse(sspoA["X08.verschiedene_kugeln.5"] == 1 & sspoA["X08.verschiedene_kugeln.6"] == 0, sspoA$misconceptions + 1, sspoA$misconceptions)
  
  ##Everyday conceptions
  sspoA$everydayconceptions <- rowSums(sspoA[, c("X03.tauch_begruendung.7", "X03.tauch_begruendung.12", "X03.tauch_begruendung.29", "X03.tauch_begruendung.37",
                                                 "X03.tauch_begruendung.44",
                                                 "X04.klein_und_gross.2", "X04.klein_und_gross.4", "X04.klein_und_gross.6",
                                                 "X04.klein_und_gross.7", "X04.klein_und_gross.9",
                                                 "X11.schiff_aus_eisen.4")])
  
  ##Scientific concepts
  sspoA$scientificconcepts <- rowSums(sspoA[, c("X03.tauch_begruendung.3", "X03.tauch_begruendung.8",
                                                "X03.tauch_begruendung.14", "X03.tauch_begruendung.15",
                                                "X03.tauch_begruendung.20", "X03.tauch_begruendung.23",
                                                "X03.tauch_begruendung.28", "X03.tauch_begruendung.31",
                                                "X03.tauch_begruendung.39", "X03.tauch_begruendung.40",
                                                "X03.tauch_begruendung.47", "X03.tauch_begruendung.48",
                                                "X06.saetze_richtigkeit.4", "X06.saetze_richtigkeit.6",
                                                "X09.was_macht_wasser.3", "X09.was_macht_wasser.4",
                                                "X10.ball_ss.4",
                                                "X11.schiff_aus_eisen.2", "X11.schiff_aus_eisen.7")])
  
  ##ICU
  sspoA$ICU_1 <- c(ifelse(sspoA$X02.kugeln_wasserglas.1 == 1 & sspoA$X02.kugeln_wasserglas.2 == 1 & sspoA$X02.kugeln_wasserglas.3 == 1, 1, 0))
  sspoA$ICU_2 <- c(ifelse((sspoA$X03.tauch_begruendung.4 == 0 & sspoA$X03.tauch_begruendung.5 == 0 & sspoA$X03.tauch_begruendung.6 == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.3", "X03.tauch_begruendung.8")]) > 0), 1, 0))
  sspoA$ICU_3 <- c(ifelse(rowSums(sspoA[, c("X03.tauch_begruendung.11", "X03.tauch_begruendung.13", "X03.tauch_begruendung.16")]) == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.14", "X03.tauch_begruendung.15")]) > 0, 1, 0))
  sspoA$ICU_4 <- c(ifelse(rowSums(sspoA[, c("X03.tauch_begruendung.19", "X03.tauch_begruendung.21", "X03.tauch_begruendung.22", "X03.tauch_begruendung.24")]) == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.20", "X03.tauch_begruendung.23")]) > 0, 1, 0))
  sspoA$ICU_5 <- c(ifelse(rowSums(sspoA[, c("X03.tauch_begruendung.27", "X03.tauch_begruendung.30", "X03.tauch_begruendung.32")]) == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.28", "X03.tauch_begruendung.31")]) > 0, 1, 0))
  sspoA$ICU_6 <- c(ifelse(rowSums(sspoA[, c("X03.tauch_begruendung.35", "X03.tauch_begruendung.36", "X03.tauch_begruendung.38")]) == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.39", "X03.tauch_begruendung.40")]) > 0, 1, 0))
  sspoA$ICU_7 <- c(ifelse(rowSums(sspoA[, c("X03.tauch_begruendung.43", "X03.tauch_begruendung.45", "X03.tauch_begruendung.46")]) == 0 & rowSums(sspoA[, c("X03.tauch_begruendung.47", "X03.tauch_begruendung.48")]) > 0, 1, 0))
  sspoA$ICU_8 <- c(ifelse(rowSums(sspoA[, c("X04.klein_und_gross.2", "X04.klein_und_gross.4", "X04.klein_und_gross.6")]) == 3, 1, 0))
  sspoA$ICU_9 <- c(ifelse(rowSums(sspoA[, c("X04.klein_und_gross.7", "X04.klein_und_gross.9")]) == 2, 1, 0))
  sspoA$ICU_10 <- c(ifelse(sspoA["X05.verschiedene_wuerfel.1"] == 0 & sspoA["X05.verschiedene_wuerfel.2"] == 0 & sspoA["X05.verschiedene_wuerfel.3"] == 0 & sspoA["X05.verschiedene_wuerfel.4"] == 1 & sspoA["X05.verschiedene_wuerfel.5"] == 0 & sspoA["X05.verschiedene_wuerfel.6"] == 0 & sspoA["X05.verschiedene_wuerfel.7"] == 1 & sspoA["X05.verschiedene_wuerfel.8"] == 0 & sspoA["X05.verschiedene_wuerfel.9"] == 0 & sspoA["X05.verschiedene_wuerfel.10"] == 0 & sspoA["X05.verschiedene_wuerfel.11"] == 1 & sspoA["X05.verschiedene_wuerfel.12"] == 0, 1, 0))
  sspoA$ICU_11 <- c(ifelse(rowSums(sspoA[, c("X06.saetze_richtigkeit.1", "X06.saetze_richtigkeit.2", "X06.saetze_richtigkeit.3", "X06.saetze_richtigkeit.5", "X06.saetze_richtigkeit.7")]) == 0 & rowSums(sspoA[, c("X06.saetze_richtigkeit.4", "X06.saetze_richtigkeit.6")]) > 0, 1, 0))
  sspoA$ICU_12 <- c(ifelse(sspoA["X07.wuerfel_wasserbecken.1"] == 0 & sspoA["X07.wuerfel_wasserbecken.2"] == 1 & sspoA["X07.wuerfel_wasserbecken.3"] == 0, 1, 0))
  sspoA$ICU_13 <- c(ifelse(sspoA["X07.wuerfel_wasserbecken.4"] == 0 & sspoA["X07.wuerfel_wasserbecken.5"] == 0 & sspoA["X07.wuerfel_wasserbecken.6"] == 1, 1, 0))
  sspoA$ICU_14 <- c(ifelse(sspoA["X08.verschiedene_kugeln.1"] == 1 & sspoA["X08.verschiedene_kugeln.2"] == 0 & sspoA["X08.verschiedene_kugeln.3"] == 1 & sspoA["X08.verschiedene_kugeln.4"] == 0 & sspoA["X08.verschiedene_kugeln.5"] == 0 & sspoA["X08.verschiedene_kugeln.6"] == 1, 1, 0))
  sspoA$ICU_15 <- c(ifelse(sspoA["X09.was_macht_wasser.1"] == 0 & sspoA["X09.was_macht_wasser.2"] == 0 & sspoA["X09.was_macht_wasser.3"] == 1 & sspoA["X09.was_macht_wasser.4"] == 1 & sspoA["X09.was_macht_wasser.5"] == 0, 1, 0))
  sspoA$ICU_16 <- c(ifelse(sspoA["X10.ball_ss.1"] == 0 & sspoA["X10.ball_ss.2"] == 0 & sspoA["X10.ball_ss.3"] == 0 & sspoA["X10.ball_ss.4"] == 1 & sspoA["X10.ball_ss.5"] == 0,  1, 0))
  sspoA$ICU_17 <- c(ifelse(sspoA["X11.schiff_aus_eisen.1"] == 0 & sspoA["X11.schiff_aus_eisen.3"] == 0 & sspoA["X11.schiff_aus_eisen.5"] == 0 & sspoA["X11.schiff_aus_eisen.6"] == 0 & (sspoA["X11.schiff_aus_eisen.2"] == 1 | sspoA["X11.schiff_aus_eisen.6"] == 1), 1, 0))
  sspoA$ICU <- rowSums(select(sspoA, tidyr::contains("ICU_")))
  
  #Make regular data frame columns from lists resulting from ifelse-functions for MCs and ICU
  sspoA$misconceptions <- c(sspoA$misconceptions)
  ##Transfer items
  x = c("X12", "X13", "X15", "X16", "X17", "X18")
  t = c("01001", "0100", "01010", "0010", "00100", "0001")
  
  

  
  sspoA <- score(sspoA, sspoA, x, t)
  sspoA$sum_cor_sspoA_Transfer <- rowSums(sspoA[, c("X12_score", "X13_score", "X15_score", "X16_score", "X17_score", "X18_score")])
  sspoA$sum_cor_sspoA_Transfer <- ifelse(sspoA$X14.unterschiedliche_wuerfel.1 == 0 & sspoA$X14.unterschiedliche_wuerfel.2 == 0 & sspoA$X14.unterschiedliche_wuerfel.3 == 1 & sspoA$X14.unterschiedliche_wuerfel.4 == 1, sspoA$sum_cor_sspoA_Transfer + 0.5, sspoA$sum_cor_sspoA_Transfer)
  sspoA$sum_cor_sspoA_Transfer <- ifelse(sspoA$X14.unterschiedliche_wuerfel.5 == 1 & sspoA$X14.unterschiedliche_wuerfel.6 == 0 & sspoA$X14.unterschiedliche_wuerfel.7 == 1 & sspoA$X14.unterschiedliche_wuerfel.8 == 0, sspoA$sum_cor_sspoA_Transfer + 0.5, sspoA$sum_cor_sspoA_Transfer)
  
  sspoA[, c("misconceptions", "everydayconceptions", "scientificconcepts", "ICU", "sum_cor_sspoA_Transfer")] %>% reshape2::melt() %>% ggplot(aes(x = value)) + facet_wrap(~ variable) + geom_histogram(color = "white")
  
  sspoA$Sum <- sspoA$ICU
  sspoA$date <- paste(sspoA$test_day, sspoA$test_month, sspoA$test_year, sep=".")
  
  
  sspoA <- sspoA %>% select("test_id","school_id","teacher_id","date","CYCLESTARTDATE","student_id","class_name","student_grade","student_age",  "student_gender","group", "ICU_1", "ICU_2", "ICU_3", "ICU_4", "ICU_5", "ICU_6", "ICU_7", 
                            "ICU_8", "ICU_9", "ICU_10", "ICU_11", "ICU_12", "ICU_13", "ICU_14", 
                            "ICU_15", "ICU_16", "ICU_17", "Sum","misconceptions","everydayconceptions",
                            "scientificconcepts")
  
}

