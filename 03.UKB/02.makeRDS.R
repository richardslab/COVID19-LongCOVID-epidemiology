setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

t <- readRDS("LongCOVID.rds")

##COVID status
test1 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_result_england_20211007.txt.gz")

t1 <- test1
t2 <- data.frame(unique(t1$eid))
colnames(t2) <- "eid"
t2$status <- 0
t2$COVIDdate <- 0
t2$hospital <- 0
for(i in seq(1,dim(t2)[1])){
  tmp <- t1[t1$eid == t2$eid[i],]
  a <- ifelse(any(tmp$result == 1), 1, 0)
  b <- ifelse(any(tmp$result == 1), min(as.Date(tmp$specdate[tmp$result == 1], format="%d/%m/%Y")), max(as.Date(tmp$specdate[tmp$result == 0], format="%d/%m/%Y")))
  c <- ifelse(any(tmp$hosaq[tmp$result == 1] == 1) | any(tmp$reqorg[tmp$result == 1] == 1), 1, 0) 
  t2$status[i] <- a
  t2$COVIDdate[i] <- as.Date(b, origin="1970-01-01")
  t2$hospital[i] <- c
}

t2$COVIDdate <- as.Date(t2$COVIDdate, origin="1970-01-01")
colnames(t2)[1] <- "ID"
res <- merge(t, t2, by="ID", all.x=T)

test2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_result_scotland_20210825.txt.gz")
t1 <- test2
t2 <- data.frame(unique(t1$eid))
colnames(t2) <- "eid"
for(i in seq(1,dim(t2)[1])){
  tmp <- t1[t1$eid == t2$eid[i],]
  a <- ifelse(any(tmp$result == 1), 1, 0)
  b <- ifelse(any(tmp$result == 1), min(as.Date(tmp$specdate[tmp$result == 1], format="%d/%m/%Y")), max(as.Date(tmp$specdate[tmp$result == 0], format="%d/%m/%Y")))
  c <- ifelse(any(tmp$factype[tmp$result == 1] == 3), 1, 0) 
  res$status[res$ID == t2$eid[i]] <- a
  res$COVIDdate[res$ID == t2$eid[i]] <- as.Date(b, origin="1970-01-01")
  res$hospital[res$ID == t2$eid[i]] <- c
}

test3 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_result_wales_20210812.txt.gz")
t1 <- test3
t2 <- data.frame(unique(t1$eid))
colnames(t2) <- "eid"
for(i in seq(1,dim(t2)[1])){
  tmp <- t1[t1$eid == t2$eid[i],]
  a <- ifelse(any(tmp$result == 1), 1, 0)
  b <- ifelse(any(tmp$result == 1), min(as.Date(tmp$specdate[tmp$result == 1], format="%d/%m/%Y")), max(as.Date(tmp$specdate[tmp$result == 0], format="%d/%m/%Y")))
  c <- ifelse(any(tmp$pattype[tmp$result == 1] == 7 & !(tmp$perstype[tmp$result == 1] %in% c(1:38,76,77))), 1, 0) 
  res$status[res$ID == t2$eid[i]] <- a
  res$COVIDdate[res$ID == t2$eid[i]] <- as.Date(b, origin="1970-01-01")
  res$hospital[res$ID == t2$eid[i]] <- c
}

res$status[is.na(res$status)] <- 0
res$COVIDdate[is.na(res$COVIDdate)] <- max(as.Date(test1$specdate, format="%d/%m/%Y"))
res$hospital[is.na(res$hospital)] <- 0

res$Duration <- round(as.numeric(as.Date(res$COVIDdate) - as.Date(res$`1stDATE`))/365)
res$newage <- res$AGE + res$Duration

res <- res %>% rename(covid19_test = status)

saveRDS(res,file="LongCOVID1.rds")

res <- readRDS("LongCOVID1.rds")
#####
data <- res
#hesin
hesin_diag <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/hesin_diag_20210620.txt.gz", sep="\t")
hesin <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/hesin_20210620.txt.gz", sep="\t")
neg <- unique(res$ID[res$covid19_test == 0])
tmp <- hesin[hesin$eid %in% neg,]
hesin_new_neg <- merge(tmp, hesin_diag, by=c("eid", "ins_index"))
hesin_new_neg <- hesin_new_neg %>% filter(diag_icd10 %in% c("U071","U072"))
hesin_new_neg <- hesin_new_neg[order(as.Date(hesin_new_neg$epistart, format="%d/%m/%Y")),]
hesin_new_neg <- hesin_new_neg[!duplicated(hesin_new_neg$eid),]
data <- data %>% mutate(case_hesin = ifelse(ID %in% unique(hesin_new_neg$eid[hesin_new_neg$diag_icd10 %in% c("U071","U072")]), 1, 0))

table(data$case_hesin)#488
tmp <- data %>% filter(case_hesin == 1)
for(i in seq(1,dim(tmp)[1])){
  tmp_hesin <- hesin_new_neg %>% filter(eid == tmp$ID[i])
  tmp$COVIDdate[i] <- as.Date(tmp_hesin$epistart, format="%d/%m/%Y")
}
data[data$case_hesin == 1,] <- tmp
data <- data %>% mutate(hospital = case_when(case_hesin == 1 ~ 1,
                                             TRUE ~ hospital))


#smoking (current, past, never, at 2020 as never being reference)1239.0.0	1249.0.0	2644.0.0
#comorbidities (asthma, COPD, 
#hypertension, diabetes, autoimmune diseases, cancer, 
#               dementia)

t <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID/ukb27449_20688_Sx.tab.gz")

#BMI
t <- t %>% mutate(BMI = case_when(!is.na(f.21001.0.0) ~ f.21001.0.0,
                                  !is.na(f.21001.1.0) ~ f.21001.1.0,
                                  !is.na(f.21001.2.0) ~ f.21001.2.0))

#self-reported ethnicity (white vs non-white as white being reference),  21000
t <- t %>% mutate(ethnicity = case_when(f.21000.0.0 %in% c(1, 1001, 1002, 1003) ~ "0White",
                                        f.21000.1.0 %in% c(1, 1001, 1002, 1003) ~ "0White",
                                        f.21000.2.0 %in% c(1, 1001, 1002, 1003) ~ "0White",
                                        TRUE ~ "Non-white"))
t$ethnicity[t$f.21000.0.0 %in% c(-1, -3)] <- NA
#smoking
t <- t %>% mutate(smoking = case_when(f.1239.0.0 %in% c(1,2) ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 == 1 ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 %in% c(2,3) & f.2644.0.0 == 1 ~ "Ever",
                                      f.1239.0.0 == -3 & f.1249.0.0 %in% c(1) ~ "Ever",
                                      f.1239.0.0 == -3 & f.1249.0.0 %in% c(2) & f.2644.0.0 == 1 ~ "Ever",
                                      f.1239.1.0 %in% c(1,2) ~ "Ever",
                                      f.1239.1.0 == 0 & f.1249.1.0 == 1 ~ "Ever",
                                      f.1239.1.0 == 0 & f.1249.1.0 %in% c(2,3) & f.2644.1.0 == 1 ~ "Ever",
                                      f.1239.1.0 == -3 & f.1249.1.0 %in% c(1) ~ "Ever",
                                      f.1239.1.0 == -3 & f.1249.1.0 %in% c(2) & f.2644.1.0 == 1 ~ "Ever",
                                      f.1239.2.0 %in% c(1,2) ~ "Ever",
                                      f.1239.2.0 == 0 & f.1249.2.0 == 1 ~ "Ever",
                                      f.1239.2.0 == 0 & f.1249.2.0 %in% c(2,3) & f.2644.2.0 == 1 ~ "Ever",
                                      f.1239.2.0 == -3 & f.1249.2.0 %in% c(1) ~ "Ever",
                                      f.1239.2.0 == -3 & f.1249.2.0 %in% c(2) & f.2644.2.0 == 1 ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 == 4 ~ "0Never",
                                      f.1239.0.0 == 0 & f.1249.0.0 %in% c(2,3) & f.2644.0.0 == 0 ~ "0Never",
                                      f.1239.1.0 == 0 & f.1249.1.0 == 4 ~ "0Never",
                                      f.1239.1.0 == 0 & f.1249.1.0 %in% c(2,3) & f.2644.1.0 == 0 ~ "0Never",
                                      f.1239.2.0 == 0 & f.1249.2.0 == 4 ~ "0Never",
                                      f.1239.2.0 == 0 & f.1249.2.0 %in% c(2,3) & f.2644.2.0 == 0 ~ "0Never"))

#non cancer illness
t$hypertension <- 0
t$diabetes <- 0
t$autoimmune <- 0
t$asthma <- 0
t$COPD <- 0
t$cancer <- 0
t$dementia <- 0


##Non-cancer Illness
f <- names(t)[names(t) %like% "f.20002."]
t <- t[, f.20002.x.x := do.call(paste0,list(c(.SD, "NA"), sep=",", collapse="")), by=f.eid, .SDcols=f]

#HTN 1065, 1072,
t <- t %>% mutate(hypertension = case_when(grepl(1065, f.20002.x.x) ~ 1,
                                           grepl(1072, f.20002.x.x) ~ 1,
                                           TRUE ~ hypertension),
                  diabetes = case_when(grepl(1220, f.20002.x.x) ~ 1,
                                       grepl(1222, f.20002.x.x) ~ 1,
                                       grepl(1223, f.20002.x.x) ~ 1,
                                       TRUE ~ diabetes))

#autoimmune diseases 
#1462	crohns disease
#1463	ulcerative colitis
#1464	rheumatoid arthritis
#1475	sclerosing cholangitis
#1477	psoriatic arthropathy
#1478	cervical spondylosis
#1480	dermatomyositis
#1481	polymyositis

t <- t %>% mutate(autoimmune = case_when(grepl(1462, f.20002.x.x) ~ 1,
                                         grepl(1463, f.20002.x.x) ~ 1,
                                         grepl(1464, f.20002.x.x) ~ 1,
                                         grepl(1475, f.20002.x.x) ~ 1,
                                         grepl(1477, f.20002.x.x) ~ 1,
                                         grepl(1478, f.20002.x.x) ~ 1,
                                         grepl(1480, f.20002.x.x) ~ 1,
                                         grepl(1481, f.20002.x.x) ~ 1,
                                         TRUE ~ autoimmune))

t <- t %>% mutate(asthma = case_when(grepl(1111, f.20002.x.x) ~ 1,
                                     TRUE ~ asthma),
                  COPD = case_when(grepl(1112, f.20002.x.x) ~ 1,
                                   grepl(1113, f.20002.x.x) ~ 1,
                                   grepl(1472, f.20002.x.x) ~ 1,
                                   TRUE ~ COPD))

t <- t %>% mutate(dementia = case_when(grepl(1263, f.20002.x.x) ~ 1,
                                     TRUE ~ dementia))

#cancer
f <- names(t)[names(t) %like% "f.20001."]
t <- t[, f.20001.x.x := do.call(paste0,list(c(.SD, "NA"), sep=",", collapse="")), by=f.eid, .SDcols=f]

t <- t %>% mutate(cancer = case_when(f.20001.x.x > 0 & f.20001.x.x < 99999 ~ 1,
                                       TRUE ~ cancer))

t <- t %>% mutate(COPD = case_when(f.22129.0.0 == 1 ~ 1,
                                   f.22130.0.0 == 1 ~ 1,
                                   f.22168.0.0 == 1 ~ 1,
                                   f.22169.0.0 == 1 ~ 1,
                                   f.22170.0.0 == 1 ~ 1,
                                   TRUE ~ COPD),
                  )

f <- names(t)[names(t) %like% "f.6152."]
t <- t[, f.6152.x.x := do.call(paste0,list(c(.SD, "NA"), sep=",", collapse="")), by=f.eid, .SDcols=f]

t <- t %>% mutate(COPD = case_when(grepl(6, f.6152.x.x) ~ 1,
                                   TRUE ~ COPD),
                  asthma = case_when(grepl(8, f.6152.x.x) ~ 1,
                                     TRUE ~ asthma))
t <- t %>% mutate(asthma = case_when(f.22127.0.0 == 1 ~ 1,
                                   f.22167.0.0 == 1 ~ 1,
                                   TRUE ~ asthma),
                  diabetes = case_when(f.2443.0.0 == 1 ~ 1,
                                       f.2443.1.0 == 1 ~ 1,
                                       f.2443.2.0 == 1 ~ 1,
                                       TRUE ~ diabetes),
                  cancer = case_when(f.2453.0.0 == 1 ~ 1,
                                     f.2443.1.0 == 1 ~ 1,
                                     f.2443.2.0 == 1 ~ 1,
                                     f.22140.0.0 == 1 ~ 1,
                                     TRUE ~ cancer)
)

#ICD codings
f <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/hesin_diag_20210620.txt.gz")
#asthma
f1 <- f[grepl("^J45", f$diag_icd10),]
t <- t %>% mutate(asthma = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ asthma))
f1 <- f[grepl("^J46", f$diag_icd10),]
t <- t %>% mutate(asthma = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ asthma))
f1 <- f[grepl("^4930", f$diag_icd9),]
t <- t %>% mutate(asthma = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ asthma))
f1 <- f[grepl("^4931", f$diag_icd9),]
t <- t %>% mutate(asthma = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ asthma))
f1 <- f[grepl("^4939", f$diag_icd9),]
t <- t %>% mutate(asthma = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ asthma))

#COPD
f1 <- f[grepl("^J41", f$diag_icd10),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^J42", f$diag_icd10),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^J43", f$diag_icd10),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^J44", f$diag_icd10),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^4910", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^4911", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^4912", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ COPD))
f1 <- f[grepl("^4918", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                   TRUE ~ COPD))
f1 <- f[grepl("^4919", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                   TRUE ~ COPD))
f1 <- f[grepl("^4929", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                   TRUE ~ COPD))
f1 <- f[grepl("^496", f$diag_icd9),]
t <- t %>% mutate(COPD = case_when(f.eid %in% f1$eid ~ 1,
                                   TRUE ~ COPD))

#HTN I1*
f1 <- f[grepl("^I1", f$diag_icd10),]
t <- t %>% mutate(hypertension = case_when(f.eid %in% f1$eid ~ 1,
                                   TRUE ~ hypertension))
f1 <- f[grepl("^40", f$diag_icd9),]
t <- t %>% mutate(hypertension = case_when(f.eid %in% f1$eid ~ 1,
                                           TRUE ~ hypertension))

#diabetes
f1 <- f[grepl("^E10", f$diag_icd10),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                           TRUE ~ diabetes))
f1 <- f[grepl("^E11", f$diag_icd10),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ diabetes))
f1 <- f[grepl("^E12", f$diag_icd10),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ diabetes))
f1 <- f[grepl("^E13", f$diag_icd10),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ diabetes))
f1 <- f[grepl("^E14", f$diag_icd10),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ diabetes))
f1 <- f[grepl("^250", f$diag_icd9),]
t <- t %>% mutate(diabetes = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ diabetes))

#autoimmune
#type1
#Celiac disease
#Pernicious anemia
f1 <- f[grepl("^2810", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ autoimmune))

#Autoimmune vasculitis
f1 <- f[grepl("^446", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^M30", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^M31", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Myasthenia gravis
f1 <- f[grepl("^3580", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^G700", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Hashimoto’s thyroiditis
f1 <- f[grepl("^E063", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Sjögren’s syndrome
f1 <- f[grepl("^M350", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))


#Graves' disease, 
f1 <- f[grepl("^E050", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^2420", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Addison’s disease
f1 <- f[grepl("^E271", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^E272", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^25540", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Systemic lupus erythematosus (SLE)
f1 <- f[grepl("^M32", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^7100", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
#Multiple sclerosis
f1 <- f[grepl("^G35", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^340", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#Psoriasis/psoriatic arthritis
f1 <- f[grepl("^L40", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^M072", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^M073", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#inflammatory bowel disease, 
#1462	crohns disease
f1 <- f[grepl("^K50", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
#1463	ulcerative colitis
f1 <- f[grepl("^K51", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))

#rheumatoid arthritis, and 
f1 <- f[grepl("^M05", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^M060", f$diag_icd10),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^7140", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^7141", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^7142", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))




#1477	psoriasis
f1 <- f[grepl("^6960", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))
f1 <- f[grepl("^6961", f$diag_icd9),]
t <- t %>% mutate(autoimmune = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ autoimmune))


#cancer
f1 <- f[grepl("^C", f$diag_icd10),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ cancer))
f1 <- f[grepl("^14", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))
f1 <- f[grepl("^15", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))
f1 <- f[grepl("^16", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))
f1 <- f[grepl("^17", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))
f1 <- f[grepl("^19", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))
f1 <- f[grepl("^20", f$diag_icd9),]
t <- t %>% mutate(cancer = case_when(f.eid %in% f1$eid ~ 1,
                                     TRUE ~ cancer))


#dementia
f1 <- f[grepl("^2900", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                         TRUE ~ dementia))
f1 <- f[grepl("^2901", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^2902", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^2903", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^2904", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^2912", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^2941", f$diag_icd9),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^F00", f$diag_icd10),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^F01", f$diag_icd10),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^F02", f$diag_icd10),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))
f1 <- f[grepl("^F03", f$diag_icd10),]
t <- t %>% mutate(dementia = case_when(f.eid %in% f1$eid ~ 1,
                                       TRUE ~ dementia))


t1 <- t %>% select(f.eid, BMI, ethnicity, smoking, hypertension, diabetes, autoimmune, asthma, COPD, cancer, dementia)

data1 <- data %>% merge(t1, by.x = "ID", by.y="f.eid", all.x=T)

saveRDS(data1, file="LongCOVID2.rds")
