setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

##define COVID19 status

pheno_raw <- read.csv("/project/richards/restricted/bqc19_release6/Clinical_Phenotypic/redcap_clinical_data_raw_2022-03-24.csv", header = T, fileEncoding="latin1", sep="\t")

pheno <- pheno_raw %>% select(BQCID, covidstatus, contains("covidtest") & (ends_with("_date") | ends_with("_result")), sx_date,sx_date_na,
                              consverbpa,
                              dconsverbpa,
                              age)

pheno[pheno == ""] <- NA

pheno <- pheno %>% select(c("BQCID","covidstatus", "sx_date", ends_with("result"), ends_with("date")))

pheno1 <- pheno %>% mutate_at(vars(ends_with("result")),as.character) %>%
  pivot_longer(cols = c(-BQCID, -covidstatus, -sx_date),
               names_to = c( "Store",".value"),
               names_sep =  "_")

covid_tests <- pheno1 %>% filter(!is.na(date) | !is.na(result)) %>% mutate(date=date(date),result=as.numeric(result))

covid_tests$result <- factor(covid_tests$result,levels=c(0,1,2), labels=c("Positive","Negative","Equivocal"))

##first positive/negative test 

# ignore any test that doesn't have a date.....
first_covid_test <- covid_tests %>% subset(!is.na(date)) %>% group_by(BQCID) %>%
  mutate(covid19_test_result = case_when(any(result == "Positive") ~ "Positive",
                                         any(result == "Equivocal") ~ "Indeterminate",
                                         any(result == "Negative") ~ "Negative",
                                         TRUE ~ "Unavailable"
  ),
  first_covid19_test_date = case_when(
    any(result == "Positive")  ~ min(date[result == "Positive"]),
    any(result == "Equivocal") ~ min(date[result == "Equivocal"]),
    any(result == "Negative")  ~ min(date[result == "Negative"]),
    TRUE ~ date(NA)
  )) %>%
  ungroup() 

reinfection <- pheno_raw %>% filter(avis_reinfx == 1) %>% dplyr::select(BQCID) %>% unique()


first_covid_test <- first_covid_test %>% dplyr::select(BQCID, covid19_test_result, first_covid19_test_date) %>% unique()

if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")


###severity final
icu_id <- pheno_raw %>% filter(icu == 1) %>% dplyr::select(BQCID) %>% unique()
hosp_id <- pheno_raw %>% filter(admit == 1) %>% dplyr::select(BQCID) %>% unique()
death_id <- pheno_raw %>% filter(death_dc == 1 | death == 1 | visit_status_spec == 3 | 
                                   exit_reason == 0 | interview_whynot == 0 | covid_severity == 4) %>% dplyr::select(BQCID) %>% unique()
resp_id <- pheno_raw %>% filter(respsupport___2 == 1 | respsupport___3 == 1 | respsupport___4 == 1 |
                                  othersupport___4 == 1 | c_ards == 1 | c_ards_followup == 1 |
                                  covid_severity == 3) %>% dplyr::select(BQCID) %>% unique()


#WHO Covid Severity Scale
first_covid_test <- first_covid_test %>% mutate(hospitalization = case_when(BQCID %in% icu_id$BQCID ~ 1,
                                                                            BQCID %in% hosp_id$BQCID ~ 1,
                                                                            TRUE ~ 0),
                                                death = case_when(BQCID %in% death_id$BQCID ~ 1,
                                                                  TRUE ~ 0),
                                                reinfection = case_when(BQCID %in% reinfection$BQCID ~ 1,
                                                                        TRUE ~ 0)
                                                )


pheno <- pheno_raw %>% mutate(sx_myalgia_anthralgia = case_when(sx_anthralgia == 1 ~ 1,
                                                                  sx_myalgia == 1 ~ 1,
                                                                  sx_anthralgia == 0 ~ 0,
                                                                  sx_myalgia == 0 ~ 0))

pheno <- pheno %>% mutate(sx_any = case_when(asymptomatic == 0 ~ 1,
                                           sx_seizure == 1 ~ 1,
                                           sx_confusion == 1 ~ 1,
                                           sx_nausea == 1 ~ 1,
                                           sx_ear_pain == 1 ~ 1,
                                           sx_anosmia == 1 ~ 1,
                                           sx_apha_dysphagia == 1 ~ 1,
                                           sx_appetite == 1 ~ 1,
                                           sx_diarrhea == 1 ~ 1,
                                           sx_extremity == 1 ~ 1,
                                           sx_dyspnea == 1 ~ 1,
                                           sx_sore_traot == 1 ~ 1,
                                           sx_rhinorrhea == 1 ~ 1,
                                           sx_headache == 1 ~ 1,
                                           sx_cough == 1 ~ 1,
                                           sx_myalgia_anthralgia == 1 ~ 1,
                                           sx_fatigue == 1 ~ 1,
                                           asymptomatic == 1 ~ 0,
                                           sx_seizure == 0 ~ 0,
                                           sx_confusion == 0 ~ 0,
                                           sx_nausea == 0 ~ 0,
                                           sx_ear_pain == 0 ~ 0,
                                           sx_anosmia == 0 ~ 0,
                                           sx_apha_dysphagia == 0 ~ 0,
                                           sx_appetite == 0 ~ 0,
                                           sx_diarrhea == 0 ~ 0,
                                           sx_extremity == 0 ~ 0,
                                           sx_dyspnea == 0 ~ 0,
                                           sx_sore_traot == 0 ~ 0,
                                           sx_rhinorrhea == 0 ~ 0,
                                           sx_headache == 0 ~ 0,
                                           sx_cough == 0 ~ 0,
                                           sx_myalgia_anthralgia == 0 ~ 0,
                                           sx_fatigue == 0 ~ 0
                                           ))



############

phenotypes <- c("sx_any", "sx_seizure", "sx_confusion", "sx_nausea", "sx_ear_pain", "sx_anosmia",
                "sx_apha_dysphagia", "sx_appetite", "sx_diarrhea", "sx_extremity", "sx_dyspnea", 
                "sx_sore_traot", "sx_rhinorrhea", "sx_headache", "sx_cough", "sx_myalgia_anthralgia",
                "sx_fatigue")
  
pheno <- pheno %>% group_by(BQCID) %>%
  mutate_at(.vars=vars(phenotypes), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQCID, .keep_all=TRUE)

pheno <- pheno %>% select(BQCID, phenotypes) 

pheno1 <- pheno %>% mutate_at(.vars=vars(phenotypes), .funs=funs(ifelse(is.infinite(.), NA, .)))

first_covid_test <- first_covid_test %>% left_join(pheno1, by="BQCID")

if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")


pheno <- pheno_raw %>% filter(sx_date != "") %>% filter(redcap_event_name == "0_patient_identifi_arm_1")  %>% select(BQCID, sx_date) %>% unique()
pheno <- pheno[!duplicated(pheno$BQCID),]
pheno %>% filter(BQCID == "BQC17082")##reinfection
first_covid_test <- first_covid_test %>% left_join(pheno, by="BQCID")

if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")


pheno <- pheno_raw %>% filter(interview_date != "") %>% select(-c("sx_seizure", "sx_confusion", "sx_nausea", "sx_ear_pain", "sx_anosmia",
                                                                  "sx_apha_dysphagia", "sx_appetite", "sx_diarrhea", "sx_extremity", "sx_dyspnea", 
                                                                  "sx_sore_traot", "sx_rhinorrhea", "sx_headache", "sx_cough", 
                                                                  "sx_fatigue", "sx_date"))

pheno <- pheno %>% inner_join(first_covid_test, by="BQCID")
pheno <- pheno %>% filter(!is.na(sx_date))

pheno <- pheno %>% mutate(time = as.Date(interview_date) - as.Date(sx_date)) 

pheno <- pheno %>% mutate(m_sx_myalgia_anthralgia = case_when(m_sx_anthralgia == 1 ~ 1,
                                                              m_sx_myalgia == 1 ~ 1,
                                                              m_sx_anthralgia == 0 ~ 0,
                                                              m_sx_myalgia == 0 ~ 0))

pheno <- pheno %>% mutate(sx_seizure_PCS = case_when(sx_seizure == 1 & m_sx_seizure == 1 & time >= 28*2 ~ 1,
                                                     sx_seizure == 1 & m_sx_seizure == 0 & time >= 28*2 ~ 0,
                                                     sx_seizure == 0 ~ 0,
                                                     TRUE ~ -1),
                          sx_confusion_PCS = case_when(sx_confusion == 1 & m_sx_confusion == 1 & time >= 28*2 ~ 1,
                                                       sx_confusion == 1 & m_sx_confusion == 0 & time >= 28*2 ~ 0,
                                                       sx_confusion == 0 ~ 0,
                                                       TRUE ~ -1),
                          sx_nausea_PCS = case_when(sx_nausea == 1 & m_sx_nausea == 1 & time >= 28*2 ~ 1,
                                                    sx_nausea == 1 & m_sx_nausea == 0 & time >= 28*2 ~ 0,
                                                    sx_nausea == 0 ~ 0,
                                                    TRUE ~ -1),
                          sx_ear_pain_PCS = case_when(sx_ear_pain == 1 & m_sx_ear_pain == 1 & time >= 28*2 ~ 1,
                                                     sx_ear_pain == 1 & m_sx_ear_pain == 0 & time >= 28*2 ~ 0,
                                                     sx_ear_pain == 0 ~ 0,
                                                     TRUE ~ -1),
                          sx_anosmia_PCS = case_when(sx_anosmia == 1 & m_sx_anosmia == 1 & time >= 28*2 ~ 1,
                                                     sx_anosmia == 1 & m_sx_anosmia == 0 & time >= 28*2 ~ 0,
                                                     sx_anosmia == 0 ~ 0,
                                                      TRUE ~ -1),
                          sx_apha_dysphagia_PCS = case_when(sx_apha_dysphagia == 1 & m_sx_apha_dysphagia == 1 & time >= 28*2 ~ 1,
                                                            sx_apha_dysphagia == 1 & m_sx_apha_dysphagia == 0 & time >= 28*2 ~ 0,
                                                            sx_apha_dysphagia == 0 ~ 0,
                                                            TRUE ~ -1),
                          sx_appetite_PCS = case_when(sx_appetite == 1 & m_sx_appetite == 1 & time >= 28*2 ~ 1,
                                                      sx_appetite == 1 & m_sx_appetite == 0 & time >= 28*2 ~ 0,
                                                      sx_appetite == 0 ~ 0,
                                                      TRUE ~ -1),
                          sx_diarrhea_PCS = case_when(sx_diarrhea == 1 & m_sx_diarrhea == 1 & time >= 28*2 ~ 1,
                                                      sx_diarrhea == 1 & m_sx_diarrhea == 0 & time >= 28*2 ~ 0,
                                                      sx_diarrhea == 0 ~ 0,
                                                      TRUE ~ -1),
                          sx_extremity_PCS = case_when(sx_extremity == 1 & m_sx_extremity == 1 & time >= 28*2 ~ 1,
                                                       sx_extremity == 1 & m_sx_extremity == 0 & time >= 28*2 ~ 0,
                                                       sx_extremity == 0 ~ 0,
                                                       TRUE ~ -1),
                          sx_dyspnea_PCS = case_when(sx_dyspnea == 1 & m_sx_dyspnea == 1 & time >= 28*2 ~ 1,
                                                     sx_dyspnea == 1 & m_sx_dyspnea == 0 & time >= 28*2 ~ 0,
                                                     sx_dyspnea == 0 ~ 0,
                                                 TRUE ~ -1),
                          sx_sore_traot_PCS = case_when(sx_sore_traot == 1 & m_sx_sore_traot == 1 & time >= 28*2 ~ 1,
                                                        sx_sore_traot == 1 & m_sx_sore_traot == 0 & time >= 28*2 ~ 0,
                                                        sx_sore_traot == 0 ~ 0,
                                                        TRUE ~ -1),
                          sx_rhinorrhea_PCS = case_when(sx_rhinorrhea == 1 & m_sx_rhinorrhea == 1 & time >= 28*2 ~ 1,
                                                        sx_rhinorrhea == 1 & m_sx_rhinorrhea == 0 & time >= 28*2 ~ 0,
                                                        sx_rhinorrhea == 0 ~ 0,
                                                        TRUE ~ -1),
                          sx_headache_PCS = case_when(sx_headache == 1 & m_sx_headache == 1 & time >= 28*2 ~ 1,
                                                      sx_headache == 1 & m_sx_headache == 0 & time >= 28*2 ~ 0,
                                                      sx_headache == 0 ~ 0,
                                                      TRUE ~ -1),
                          sx_cough_PCS = case_when(sx_cough == 1 & m_sx_cough == 1 & time >= 28*2 ~ 1,
                                                   sx_cough == 1 & m_sx_cough == 0 & time >= 28*2 ~ 0,
                                                   sx_cough == 0 ~ 0,
                                                   TRUE ~ -1),
                          sx_myalgia_anthralgia_PCS = case_when(sx_myalgia_anthralgia == 1 & m_sx_myalgia_anthralgia == 1 & time >= 28*2 ~ 1,
                                                                sx_myalgia_anthralgia == 1 & m_sx_myalgia_anthralgia == 0 & time >= 28*2 ~ 0,
                                                                sx_myalgia_anthralgia == 0 ~ 0,
                                                                TRUE ~ -1),
                          sx_fatigue_PCS = case_when(sx_fatigue == 1 & m_sx_fatigue == 1 & time >= 28*2 ~ 1,
                                                     sx_fatigue == 1 & m_sx_fatigue == 0 & time >= 28*2 ~ 0,
                                                     sx_fatigue == 0 ~ 0,
                                                     TRUE ~ -1))

pheno <- pheno %>% mutate(sx_any_PCS = case_when(sx_any == 0 ~ 0,
                                                 sx_seizure_PCS == 1 ~ 1,
                                                 sx_confusion_PCS == 1 ~ 1,
                                                 sx_nausea_PCS == 1 ~ 1,
                                                 sx_ear_pain_PCS == 1 ~ 1,
                                                 sx_anosmia_PCS == 1 ~ 1,
                                                 sx_apha_dysphagia_PCS == 1 ~ 1,
                                                 sx_appetite_PCS == 1 ~ 1,
                                                 sx_diarrhea_PCS == 1 ~ 1,
                                                 sx_extremity_PCS == 1 ~ 1,
                                                 sx_dyspnea_PCS == 1 ~ 1,
                                                 sx_sore_traot_PCS == 1 ~ 1,
                                                 sx_rhinorrhea_PCS == 1 ~ 1,
                                                 sx_headache_PCS == 1 ~ 1,
                                                 sx_cough_PCS == 1 ~ 1,
                                                 sx_myalgia_anthralgia_PCS == 1 ~ 1,
                                                 sx_fatigue_PCS == 1 ~ 1,
                                                 sx_seizure_PCS == 0 ~ 0,
                                                 sx_confusion_PCS == 0 ~ 0,
                                                 sx_nausea_PCS == 0 ~ 0,
                                                 sx_ear_pain_PCS == 0 ~ 0,
                                                 sx_anosmia_PCS == 0 ~ 0,
                                                 sx_apha_dysphagia_PCS == 0 ~ 0,
                                                 sx_appetite_PCS == 0 ~ 0,
                                                 sx_diarrhea_PCS == 0 ~ 0,
                                                 sx_extremity_PCS == 0 ~ 0,
                                                 sx_dyspnea_PCS == 0 ~ 0,
                                                 sx_sore_traot_PCS == 0 ~ 0,
                                                 sx_rhinorrhea_PCS == 0 ~ 0,
                                                 sx_headache_PCS == 0 ~ 0,
                                                 sx_cough_PCS == 0 ~ 0,
                                                 sx_myalgia_anthralgia_PCS == 0 ~ 0,
                                                 sx_fatigue_PCS == 0 ~ 0,
                                                 TRUE ~ -1))

hist(as.numeric(pheno$time))

phenotypes_PCS <- c("sx_any_PCS", "sx_seizure_PCS", "sx_confusion_PCS", "sx_nausea_PCS", "sx_ear_pain_PCS", 
                    "sx_anosmia_PCS", "sx_apha_dysphagia_PCS", "sx_appetite_PCS", "sx_diarrhea_PCS", 
                    "sx_extremity_PCS", "sx_dyspnea_PCS", "sx_sore_traot_PCS", "sx_rhinorrhea_PCS", 
                    "sx_headache_PCS", "sx_cough_PCS", "sx_myalgia_anthralgia_PCS", "sx_fatigue_PCS")


pheno <- pheno %>% group_by(BQCID) %>%
  mutate_at(.vars=vars(phenotypes_PCS), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQCID, .keep_all=TRUE)

pheno <- pheno %>% select(BQCID, phenotypes_PCS) 

pheno1 <- pheno %>% mutate_at(.vars=vars(phenotypes_PCS), .funs=funs(ifelse(is.infinite(.), NA, .)))

first_covid_test <- first_covid_test %>% left_join(pheno1, by="BQCID")
if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")


pheno <- pheno_raw %>% filter(interview_date != "") %>% select(BQCID, interview_date, sx_report)
pheno <- pheno %>% inner_join(first_covid_test, by="BQCID")
pheno <- pheno %>% filter(!is.na(sx_date))

pheno <- pheno %>% mutate(time = as.Date(interview_date) - as.Date(sx_date)) 
pheno <- pheno %>% mutate(longCOVID = case_when(sx_report == 1 & time > 28*2 & covid19_test_result == "Positive" ~ 1,
                                                sx_report == 0 & time > 28*2 & covid19_test_result == "Positive" ~ 0,
                                                TRUE ~ -1))

pheno <- pheno %>% select(BQCID, longCOVID)
pheno <- pheno %>% group_by(BQCID) %>%
  mutate_at(.vars=vars(longCOVID), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQCID, .keep_all=TRUE)

first_covid_test <- first_covid_test %>% left_join(pheno, by="BQCID")
if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")


pheno <- pheno_raw %>% filter(redcap_event_name == "0_patient_identifi_arm_1")

pheno <- pheno %>% mutate(age = age,
                          sex = ifelse(female == 1, "F", "0M"),
                          BMI = bmi,
                          com_asthma = phx_asthma,
                          smoking = case_when(smoking == 0 ~ "0Never",
                                              smoking == 2 ~ "Past",
                                              smoking %in% c(1,3) ~ "Current"),
                          com_COPD = phx_copd,
                          com_diabetes = phx_diabetes,
                          com_cancer = phx_cancer,
                          com_dementia = phx_dementia,
                          com_hypertension = phx_htn,
                          com_autoimmune = phx_rheum
)
pheno <- pheno %>% mutate(age_range = case_when(age < 60 ~ "<60",
                                                age < 70 & age >= 60 ~ "0Age60-70",
                                                age < 80 & age >= 70 ~ "70-80",
                                                age >= 80 ~ ">80"),
                          obesity = case_when(BMI >= 30 ~ 1,
                                              BMI < 30 ~ 0))


pheno1 <- pheno %>% select(BQCID, age, sex, BMI, com_asthma, smoking,
                           com_COPD, com_diabetes, com_cancer, com_dementia, 
                           com_hypertension, com_autoimmune, age_range, obesity)
pheno1 <- pheno1[!duplicated(pheno1$BQCID),]

first_covid_test <- first_covid_test %>% left_join(pheno1, by="BQCID")

if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")

##vaccination
pheno <- pheno_raw %>% select(BQCID, colnames(pheno_raw)[grepl("vaccin", colnames(pheno_raw))])
pheno <- pheno %>% filter(!is.na(vaccinate)) %>%
  select(BQCID, vaccinate, vaccin_date1, vaccin_dosenum, vaccin_date2)
  
first_covid_test <- first_covid_test %>% left_join(pheno, by="BQCID")
if(dim(first_covid_test)[1] != length(unique(first_covid_test$BQCID))) stop("something is wrong.")

first_covid_test <- first_covid_test %>% mutate(sx_any_PCS = ifelse(longCOVID == 1, 1, sx_any_PCS))
first_covid_test <- first_covid_test %>% mutate_at(.vars=vars(phenotypes_PCS, phenotypes), .funs=funs(ifelse(.==-1, NA, .)))
first_covid_test <- first_covid_test %>% mutate(prevaccination = case_when(as.Date(first_covid19_test_date) - as.Date(vaccin_date2) >= 14 ~ 2,
                                                                           as.Date(first_covid19_test_date) - as.Date(vaccin_date1) >= 14 ~ 1,
                                                                           as.Date(first_covid19_test_date) - as.Date(vaccin_date1) < 14 ~ 0,
                                                                           vaccinate == 0 ~ 0
                                                                           ))

first_covid_test %>% filter(covid19_test_result %in% c("Positive", "Negative")) %>% saveRDS("clinical_bqc19.rds")

data <- readRDS("clinical_bqc19.rds")
pheno <- pheno_raw %>% filter(BQCID %in% data$BQCID)
pheno %>% filter(BQCID == "BQC17082") %>% select(BQCID, redcap_event_name)
pheno <- pheno %>% filter(!(BQCID == "BQC17082" & sx_date == "2021-08-31"))
pheno <- pheno %>% filter(!(BQCID == "BQC17082" & redcap_event_name %in% c("m01_arm_1", "m03_arm_1")))
pheno <- pheno %>% filter(!(BQCID == "BQC17082" & redcap_event_name %in% c("m06_arm_1") & interview_date == "2022-02-23"))

pheno <- pheno %>% left_join(data, by="BQCID")
tmp <- pheno %>% group_by(BQCID) %>%
  mutate(time = max(as.Date(interview_date) - as.Date(first_covid19_test_date), na.rm=T)) %>%
           distinct(BQCID, .keep_all=TRUE)
tmp <- tmp %>% mutate(time = ifelse(is.infinite(time), NA, time))

negative <- tmp %>% filter(time < 0)


pheno %>% filter(BQCID %in% negative$BQCID) %>% select(BQCID, first_covid19_test_date, interview_date)

tmp <- tmp %>% mutate(time = ifelse(is.infinite(time) | time < 0, NA, time))
hist(tmp$time)
quantile(tmp$time, na.rm=T)#144

