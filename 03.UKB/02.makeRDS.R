setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

library(data.table)
library(tidyr)
library(dplyr)

t <- readRDS("LongCOVID.rds")

covid1 <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/covid19_result_england_20210930.txt.gz")
covid2 <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/covid19_result_scotland_20210831.txt.gz")
covid3 <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/covid19_result_wales_20210831.txt.gz")

covid1 <- covid1 %>% filter(result == 1) %>% group_by(eid) %>%
  mutate(hospital = max(hosaq),
         covid_date = min(as.Date(specdate, format="%d/%m/%Y"))) %>%
  distinct(eid, .keep_all=TRUE)
  
covid1 <- covid1 %>% mutate(hospital = ifelse(hospital==1, 1, 0))
covid1 <- covid1 %>% select(eid, result, hospital, covid_date)

covid2 <- covid2 %>% filter(result == 1) %>% group_by(eid) %>%
  mutate(hospital = ifelse(any(factype == 3), 1, 0),
         covid_date = min(as.Date(specdate, format="%d/%m/%Y"))) %>%
  distinct(eid, .keep_all=TRUE)
covid2 <- covid2 %>% select(eid, result, hospital, covid_date)

covid3 <- covid3 %>% filter(result == 1) %>% group_by(eid) %>%
  mutate(hospital = ifelse(any(pattype == 7 & !(perstype %in% c(1:38,76:77,82:94,100:101))), 1, 0),
         covid_date = min(as.Date(specdate, format="%d/%m/%Y"))) %>%
  distinct(eid, .keep_all=TRUE)
covid3 <- covid3 %>% select(eid, result, hospital, covid_date)

covid <- bind_rows(covid1, covid2, covid3)
covid <- covid %>% arrange(covid_date)

covid <- covid %>% group_by(eid) %>%
  mutate(hospital = max(hospital),
         covid_date = min(covid_date)) %>% 
  distinct(eid, .keep_all=TRUE)

t <- t %>% left_join(covid, by=c("ID"="eid"))
t <- t %>% mutate(result = ifelse(is.na(result), 0, result),
                  hospital = ifelse(is.na(hospital), 0, hospital),
                  covid_date = ifelse(is.na(covid_date), as.Date(max(covid$covid_date)), as.Date(covid_date)))

t$covid_date <- as.Date(t$covid_date, origin="1970/01/01")
t$Duration <- round(as.numeric(as.Date(t$covid_date) - as.Date(t$`1stDATE`))/365)
t$newage <- t$AGE + t$Duration

t <- t %>% rename(covid19_test = result)

saveRDS(t,file="LongCOVID1.rds")

res <- readRDS("LongCOVID1.rds")
#####
data <- res
#hesin
hesin_diag <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/hesin_diag_20210930.txt.gz")
hesin_covid <- hesin_diag %>% filter(diag_icd10 %in% c("U071","U072"))
data <- data %>% mutate(hospital = case_when(ID %in% hesin_covid$eid ~ 1,
                                             TRUE ~ hospital))


#smoking (current, past, never, at 2020 as never being reference)1239.0.0	1249.0.0	2644.0.0
#comorbidities (asthma, COPD, 
#hypertension, diabetes, autoimmune diseases, cancer, 
#               dementia)

t <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID/ukb27449_20688_Sx.tab.gz")

#BMI
t <- t %>% mutate(BMI = f.21001.0.0)

#self-reported ethnicity (white vs non-white as white being reference),  21000
t <- t %>% mutate(ethnicity = case_when(f.21000.0.0 %in% c(1, 1001, 1002, 1003) ~ "0White",
                                        TRUE ~ "Non-white"))
t$ethnicity[t$f.21000.0.0 %in% c(-1, -3)] <- NA
#smoking
t <- t %>% mutate(smoking = case_when(f.1239.0.0 %in% c(1,2) ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 == 1 ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 %in% c(2,3) & f.2644.0.0 == 1 ~ "Ever",
                                      f.1239.0.0 == -3 & f.1249.0.0 %in% c(1) ~ "Ever",
                                      f.1239.0.0 == -3 & f.1249.0.0 %in% c(2) & f.2644.0.0 == 1 ~ "Ever",
                                      f.1239.0.0 == 0 & f.1249.0.0 == 4 ~ "0Never",
                                      f.1239.0.0 == 0 & f.1249.0.0 %in% c(2,3) & f.2644.0.0 == 0 ~ "0Never"))

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
                                     f.2453.1.0 == 1 ~ 1,
                                     f.2453.2.0 == 1 ~ 1,
                                     f.22140.0.0 == 1 ~ 1,
                                     TRUE ~ cancer)
)

#ICD codings
f <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/hesin_diag_20210930.txt.gz")

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
