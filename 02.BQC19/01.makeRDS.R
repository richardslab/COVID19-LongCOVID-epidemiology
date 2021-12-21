setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

data <- read.csv("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/Release5/Clinical_Phenotypic/redcap_clinical_data-2021-12-09.csv", header = T, fileEncoding="latin1", sep="\t")

data$Date.of.earliest.symptom.s..[data$Date.of.earliest.symptom.s.. == ""] <- NA
data <- data %>% group_by(BQC.ID) %>% 
  tidyr::fill(Date.of.earliest.symptom.s.., .direction = "down") %>%
  ungroup()

data$Final.COVID.status.[data$Final.COVID.status. == ""] <- NA
data <- data %>% group_by(BQC.ID) %>% 
  tidyr::fill(Final.COVID.status., .direction = "down") %>%
  ungroup()
data[data$BQC.ID == "BQC12101",c("BQC.ID", "Final.COVID.status.")]

data$Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient.[data$Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == ""] <- NA
data <- data %>% group_by(BQC.ID) %>% 
  tidyr::fill(Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient., .direction = "down") %>%
  ungroup()

data <- data %>% mutate(hospital = case_when(Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == "Hospitalized" ~ 1,
                                             Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == "Outpatient" ~ 0,
                                               TRUE ~ -1),
                        covid19_test = case_when(Final.COVID.status. == "Positive" ~ 1,
                                                   Final.COVID.status. == "Negative" ~ 0,
                                                   TRUE ~ -1))

data <- data %>% mutate(COVIDSx = case_when(Does.the.participant.report.persistent.symptoms.related.to.SARS.CoV.2.infection. == "Yes" ~ 1,
                                            Does.the.participant.report.persistent.symptoms.related.to.SARS.CoV.2.infection. == "No" ~ 0,
                                            TRUE ~ -1))


data <- data %>% mutate(Sx_tremor = case_when(Seizure.. == "Yes" ~ 1,
                                              Seizure.. == "No" ~ 0),
                        Sx_confusion = case_when(Confusion...altered.mental.status.. == "Yes" ~ 1,
                                                 Confusion...altered.mental.status.. == "No" ~ 0),
                        Sx_nausia = case_when(Nausea...vomiting.. == "Yes" ~ 1,
                                              Nausea...vomiting.. == "No" ~ 0),
                        Sx_earpain = case_when(Ear.pain.. == "Yes" ~ 1,
                                               Ear.pain.. == "No" ~ 0),
                        Sx_smell = case_when(Loss.of.taste...lost.of.smell.. == "Yes" ~ 1,
                                             Loss.of.taste...lost.of.smell.. == "No" ~ 0),
                        Sx_hoarseness = case_when(Trouble.speaking..Aphasia...Dysphasia... == "Yes" ~ 1,
                                                  Trouble.speaking..Aphasia...Dysphasia... == "No" ~ 0),
                        Sx_appetite = case_when(Loss.of.appetite.. == "Yes" ~ 1,
                                                Loss.of.appetite.. == "No" ~ 0),
                        Sx_diarrhea = case_when(Diarrhea.. == "Yes" ~ 1,
                                                Diarrhea.. == "No" ~ 0),
                        Sx_weakness = case_when(Extremity.weakness.or.numbness.. == "Yes" ~ 1,
                                                Extremity.weakness.or.numbness.. == "No" ~ 0),
                        Sx_SOB = case_when(Shortness.of.breath..Dyspnea... == "Yes" ~ 1,
                                           Shortness.of.breath..Dyspnea... == "No" ~ 0),
                        Sx_sorethroat = case_when(Sore.throat.. == "Yes" ~ 1,
                                                  Sore.throat.. == "No" ~ 0),
                        Sx_runnynose = case_when(Runny.nose..Rhinorrhea... == "Yes" ~ 1,
                                                 Runny.nose..Rhinorrhea... == "No" ~ 0),
                        Sx_headache = case_when(Headache.. == "Yes" ~ 1,
                                                Headache.. == "No" ~ 0),
                        Sx_cough = case_when(Cough.. == "Yes" ~ 1,
                                             Cough.. == "No" ~ 0),
                        Sx_musclejointpain = case_when(Muscle.aches..Myalgia... == "Yes" ~ 1,
                                                       Joint.pain..Arthralgia... == "Yes" ~ 1,
                                                       Muscle.aches..Myalgia... == "No" ~ 0,
                                                       Joint.pain..Arthralgia... == "No" ~ 0),
                        Sx_fatigue = case_when(Fatigue.. == "Yes" ~ 1,
                                               Fatigue.. == "No" ~ 0))

data <- data %>% mutate(Sx_any = case_when(Asymptomatic. == "No" ~ 1,
                                           Sx_tremor == 1 ~ 1,
                                           Sx_confusion == 1 ~ 1,
                                           Sx_nausia == 1 ~ 1,
                                           Sx_earpain == 1 ~ 1,
                                           Sx_smell == 1 ~ 1,
                                           Sx_hoarseness == 1 ~ 1,
                                           Sx_appetite == 1 ~ 1,
                                           Sx_diarrhea == 1 ~ 1,
                                           Sx_weakness == 1 ~ 1,
                                           Sx_SOB == 1 ~ 1,
                                           Sx_sorethroat == 1 ~ 1,
                                           Sx_runnynose == 1 ~ 1,
                                           Sx_headache == 1 ~ 1,
                                           Sx_cough == 1 ~ 1,
                                           Sx_musclejointpain == 1 ~ 1,
                                           Sx_fatigue == 1 ~ 1,
                                           Asymptomatic. == "Yes" ~ 0,
                                           Sx_tremor == 0 ~ 0,
                                           Sx_confusion == 0 ~ 0,
                                           Sx_nausia == 0 ~ 0,
                                           Sx_earpain == 0 ~ 0,
                                           Sx_smell == 0 ~ 0,
                                           Sx_hoarseness == 0 ~ 0,
                                           Sx_appetite == 0 ~ 0,
                                           Sx_diarrhea == 0 ~ 0,
                                           Sx_weakness == 0 ~ 0,
                                           Sx_SOB == 0 ~ 0,
                                           Sx_sorethroat == 0 ~ 0,
                                           Sx_runnynose == 0 ~ 0,
                                           Sx_headache == 0 ~ 0,
                                           Sx_cough == 0 ~ 0,
                                           Sx_musclejointpain == 0 ~ 0,
                                           Sx_fatigue == 0 ~ 0
                                           ))

final <- data %>% group_by(BQC.ID) %>%
  mutate(COVIDSx = max(COVIDSx, na.rm=T),
         hospital = max(hospital, na.rm=T),
         covid19_test = max(covid19_test, na.rm=T)) %>%
  mutate_at(.vars=vars(colnames(data)[grepl("^Sx_", colnames(data))]), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQC.ID, .keep_all=TRUE)
final <- final %>% mutate_at(.vars=vars(colnames(data)[grepl("^Sx_", colnames(data))]), .funs=funs(ifelse(is.infinite(.), NA, .)))  

tmp <- final %>% select(BQC.ID, colnames(final)[grepl("^Sx_", colnames(final))]) 

data1 <- data %>% filter(Date.of.follow.up. != "") %>% select(-colnames(final)[grepl("^Sx_", colnames(final))])
data1 <- data1 %>% mutate(time =as.Date(Date.of.follow.up.) - as.Date(Date.of.earliest.symptom.s..))

data1 <- data1 %>% merge(tmp, by="BQC.ID")

data1 <- data1 %>% mutate(Sx_tremor_PCS = case_when(Sx_tremor == 1 & Seizure..1 == "Yes" & time >= 28*2 ~ 1,
                                                    Sx_tremor == 1 & Seizure..1 == "Yes" & time < 28*2 ~ 0,
                                                    Sx_tremor == 1 & Seizure..1 == "No"~ 0,
                                                    Sx_tremor == 0 ~ 0,
                                                    TRUE ~ -1),
                          Sx_confusion_PCS = case_when(Sx_confusion == 1 & Confusion...altered.mental.status...1 == "Yes" & time > 28*2 ~ 1,
                                                       Sx_confusion == 1 & Confusion...altered.mental.status...1 == "Yes" & time < 28*2 ~ 0,
                                                       Sx_confusion == 1 & Confusion...altered.mental.status...1 == "No"~ 0,
                                                       Sx_confusion == 0 ~ 0,
                                                       TRUE ~ -1),
                          Sx_nausia_PCS = case_when(Sx_nausia == 1 & Nausea...vomiting...1 == "Yes" & time > 28*2 ~ 1,
                                                    Sx_nausia == 1 & Nausea...vomiting...1 == "Yes" & time < 28*2 ~ 0,
                                                    Sx_nausia == 1 & Nausea...vomiting...1 == "No"~ 0,
                                                    Sx_nausia == 0 ~ 0,
                                                    TRUE ~ -1),
                          Sx_earpain_PCS = case_when(Sx_earpain == 1 & Ear.pain...1 == "Yes" & time > 28*2 ~ 1,
                                                     Sx_earpain == 1 & Ear.pain...1 == "Yes" & time < 28*2 ~ 0,
                                                     Sx_earpain == 1 & Ear.pain...1 == "No"~ 0,
                                                     Sx_earpain == 0 ~ 0,
                                                     TRUE ~ -1),
                          Sx_smell_PCS = case_when(Sx_smell == 1 & Loss.of.taste...lost.of.smell...1 == "Yes" & time > 28*2 ~ 1,
                                                   Sx_smell == 1 & Loss.of.taste...lost.of.smell...1 == "Yes" & time < 28*2 ~ 0,
                                                   Sx_smell == 1 & Loss.of.taste...lost.of.smell...1 == "No"~ 0,
                                                   Sx_smell == 0 ~ 0,
                                                   TRUE ~ -1),
                          Sx_hoarseness_PCS = case_when(Sx_hoarseness == 1 & Trouble.speaking..Aphasia...Dysphasia....1 == "Yes" & time > 28*2 ~ 1,
                                                        Sx_hoarseness == 1 & Trouble.speaking..Aphasia...Dysphasia....1 == "Yes" & time < 28*2 ~ 0,
                                                        Sx_hoarseness == 1 & Trouble.speaking..Aphasia...Dysphasia....1 == "No"~ 0,
                                                        Sx_hoarseness == 0 ~ 0,
                                                        TRUE ~ -1),
                          Sx_appetite_PCS = case_when(Sx_appetite == 1 & Loss.of.appetite...1 == "Yes" & time > 28*2 ~ 1,
                                                      Sx_appetite == 1 & Loss.of.appetite...1 == "Yes" & time < 28*2 ~ 0,
                                                      Sx_appetite == 1 & Loss.of.appetite...1 == "No"~ 0,
                                                      Sx_appetite == 0 ~ 0,
                                                      TRUE ~ -1),
                          Sx_diarrhea_PCS = case_when(Sx_diarrhea == 1 & Diarrhea...1 == "Yes" & time > 28*2 ~ 1,
                                                      Sx_diarrhea == 1 & Diarrhea...1 == "Yes" & time < 28*2 ~ 0,
                                                      Sx_diarrhea == 1 & Diarrhea...1 == "No"~ 0,
                                                      Sx_diarrhea == 0 ~ 0,
                                                      TRUE ~ -1),
                          Sx_weakness_PCS = case_when(Sx_weakness == 1 & Extremity.weakness.or.numbness...1 == "Yes" & time > 28*2 ~ 1,
                                                      Sx_weakness == 1 & Extremity.weakness.or.numbness...1 == "Yes" & time < 28*2 ~ 0,
                                                      Sx_weakness == 1 & Extremity.weakness.or.numbness...1 == "No"~ 0,
                                                      Sx_weakness == 0 ~ 0,
                                                      TRUE ~ -1),
                          Sx_SOB_PCS = case_when(Sx_SOB == 1 & Shortness.of.breath..Dyspnea....1 == "Yes" & time > 28*2  ~ 1,
                                                 Sx_SOB == 1 & Shortness.of.breath..Dyspnea....1 == "Yes" & time < 28*2 ~ 0,
                                                 Sx_SOB == 1 & Shortness.of.breath..Dyspnea....1 == "No"~ 0,
                                                 Sx_SOB == 0 ~ 0,
                                                 TRUE ~ -1),
                          Sx_sorethroat_PCS = case_when(Sx_sorethroat == 1 & Sore.throat...1 == "Yes" & time > 28*2 ~ 1,
                                                        Sx_sorethroat == 1 & Sore.throat...1 == "Yes" & time < 28*2 ~ 0,
                                                        Sx_sorethroat == 1 & Sore.throat...1 == "No"~ 0,
                                                        Sx_sorethroat == 0 ~ 0,
                                                        TRUE ~ -1),
                          Sx_runnynose_PCS = case_when(Sx_runnynose == 1 & Runny.nose..Rhinorrhea....1 == "Yes" & time > 28*2 ~ 1,
                                                       Sx_runnynose == 1 & Runny.nose..Rhinorrhea....1 == "Yes" & time < 28*2 ~ 0,
                                                       Sx_runnynose == 1 & Runny.nose..Rhinorrhea....1 == "No"~ 0,
                                                       Sx_runnynose == 0 ~ 0,
                                                       TRUE ~ -1),
                          Sx_headache_PCS = case_when(Sx_headache == 1 & Headache...1 == "Yes" & time > 28*2 ~ 1,
                                                      Sx_headache == 1 & Headache...1 == "Yes" & time < 28*2 ~ 0,
                                                      Sx_headache == 1 & Headache...1 == "No" ~ 0,
                                                      Sx_headache == 0 ~ 0,
                                                      TRUE ~ -1),
                          Sx_cough_PCS = case_when(Sx_cough == 1 & Cough...1 == "Yes" & time > 28*2 ~ 1,
                                                   Sx_cough == 1 & Cough...1 == "Yes" & time < 28*2 ~ 0,
                                                   Sx_cough == 1 & Cough...1 == "No"~ 0,
                                                   Sx_cough == 0 ~ 0,
                                                   TRUE ~ -1),
                          Sx_musclejointpain_PCS = case_when(Sx_musclejointpain == 1 & (Muscle.aches..Myalgia....1 == "Yes" | Joint.pain..Arthralgia....1 == "Yes") & time > 28*2 ~ 1,
                                                             Sx_musclejointpain == 1 & (Muscle.aches..Myalgia....1 == "Yes" |  Joint.pain..Arthralgia....1 == "Yes") & time < 28*2 ~ 0,
                                                             Sx_musclejointpain == 1 & (Muscle.aches..Myalgia....1 == "No" | Joint.pain..Arthralgia....1 == "No") ~ 0,
                                                             Sx_musclejointpain == 0 ~ 0,
                                                             TRUE ~ -1),
                          Sx_fatigue_PCS = case_when(Sx_fatigue == 1 & Fatigue...1 == "Yes" & time > 28*2 ~ 1,
                                                     Sx_fatigue == 1 & Fatigue...1 == "Yes" & time < 28*2 ~ 0,
                                                     Sx_fatigue == 1 & Fatigue...1 == "No"~ 0,
                                                     Sx_fatigue == 0 ~ 0,
                                                     TRUE ~ -1))

data1 <- data1 %>% mutate(Sx_any_PCS = case_when(Sx_any == 0 ~ 0,
                                                 Sx_tremor_PCS == 1 ~ 1,
                                                 Sx_confusion_PCS == 1 ~ 1,
                                                 Sx_nausia_PCS == 1 ~ 1,
                                                 Sx_earpain_PCS == 1 ~ 1,
                                                 Sx_smell_PCS == 1 ~ 1,
                                                 Sx_hoarseness_PCS == 1 ~ 1,
                                                 Sx_appetite_PCS == 1 ~ 1,
                                                 Sx_diarrhea_PCS == 1 ~ 1,
                                                 Sx_weakness_PCS == 1 ~ 1,
                                                 Sx_SOB_PCS == 1 ~ 1,
                                                 Sx_sorethroat_PCS == 1 ~ 1,
                                                 Sx_runnynose_PCS == 1 ~ 1,
                                                 Sx_headache_PCS == 1 ~ 1,
                                                 Sx_cough_PCS == 1 ~ 1,
                                                 Sx_musclejointpain_PCS == 1 ~ 1,
                                                 Sx_fatigue_PCS == 1 ~ 1,
                                                 Sx_tremor_PCS == 0 ~ 0,
                                                 Sx_confusion_PCS == 0 ~ 0,
                                                 Sx_nausia_PCS == 0 ~ 0,
                                                 Sx_earpain_PCS == 0 ~ 0,
                                                 Sx_smell_PCS == 0 ~ 0,
                                                 Sx_hoarseness_PCS == 0 ~ 0,
                                                 Sx_appetite_PCS == 0 ~ 0,
                                                 Sx_diarrhea_PCS == 0 ~ 0,
                                                 Sx_weakness_PCS == 0 ~ 0,
                                                 Sx_SOB_PCS == 0 ~ 0,
                                                 Sx_sorethroat_PCS == 0 ~ 0,
                                                 Sx_runnynose_PCS == 0 ~ 0,
                                                 Sx_headache_PCS == 0 ~ 0,
                                                 Sx_cough_PCS == 0 ~ 0,
                                                 Sx_musclejointpain_PCS == 0 ~ 0,
                                                 Sx_fatigue_PCS == 0 ~ 0,
                                                 TRUE ~ -1))

data1 <- data1 %>% mutate(longCOVID = case_when(COVIDSx == 1 & time > 28*2 & Final.COVID.status. == "Positive" ~ 1,
                                                Sx_any_PCS == 1 & Final.COVID.status. == "Positive" ~ 1,
                                                TRUE ~ 0))

tmp <- data1 %>% group_by(BQC.ID) %>%
  mutate(longCOVID = max(longCOVID, na.rm=T)) %>%
  mutate_at(.vars=vars(colnames(data1)[grepl("_PCS$", colnames(data1))]), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQC.ID, .keep_all=TRUE) %>% 
  select(BQC.ID, longCOVID, colnames(data1)[grepl("_PCS$", colnames(data1))])

final <- final %>% merge(tmp, by="BQC.ID", all.x=T)

final <- final %>% mutate(age = Age.at.arrival.,
                          sex = ifelse(Sex.at.birth. == "Male", "0M", "F"),
                          BMI = BMI.,
                          com_asthma = ifelse(Asthma.. == "Yes", 1, 0),
                          smoking = case_when(Smoking.status. == "Non-smoker" ~ "0Never",
                                              Smoking.status. == "Former smoker" ~ "Past",
                                              Smoking.status. == "Smoker" ~ "Current"),
                          com_COPD = ifelse(COPD..emphysema..chronic.bronchitis... == "Yes", 1, 0),
                          com_diabetes = ifelse(Diabetes.. == "Yes",1 , 0),
                          com_cancer = ifelse(Malignant.neoplasm.. == "Yes", 1, 0),
                          com_dementia = ifelse(Dementia.. == "Yes", 1, 0),
                          com_hypertension = ifelse(Arterial.Hypertension.. == "Yes", 1, 0),
                          com_autoimmune = ifelse(Rheumatologic.disease.. == "Yes", 1, 0)
)
final <- final %>% mutate(age_range = case_when(age < 60 ~ "<60",
                                                age < 70 & age >= 60 ~ "0Age60-70",
                                                age < 80 & age >= 70 ~ "70-80",
                                                age >= 80 ~ ">80"),
                          obesity = case_when(BMI >= 30 ~ 1,
                                              BMI < 30 ~ 0))

final <- final %>% mutate(Sx_any_PCS = ifelse(Sx_any == 0, 0, Sx_any_PCS),
                          Sx_tremor_PCS = ifelse(Sx_tremor == 0, 0, Sx_tremor_PCS),
                          Sx_confusion_PCS = ifelse(Sx_confusion == 0, 0, Sx_confusion_PCS),
                          Sx_nausia_PCS = ifelse(Sx_nausia == 0, 0, Sx_nausia_PCS),
                          Sx_earpain_PCS = ifelse(Sx_earpain == 0, 0, Sx_earpain_PCS),
                          Sx_smell_PCS = ifelse(Sx_smell == 0, 0, Sx_smell_PCS),
                          Sx_hoarseness_PCS = ifelse(Sx_hoarseness == 0, 0, Sx_hoarseness_PCS),
                          Sx_appetite_PCS = ifelse(Sx_appetite == 0, 0, Sx_appetite_PCS),
                          Sx_diarrhea_PCS = ifelse(Sx_diarrhea == 0, 0, Sx_diarrhea_PCS),
                          Sx_weakness_PCS = ifelse(Sx_weakness == 0, 0, Sx_weakness_PCS),
                          Sx_SOB_PCS = ifelse(Sx_SOB == 0, 0, Sx_SOB_PCS),
                          Sx_sorethroat_PCS = ifelse(Sx_sorethroat == 0, 0, Sx_sorethroat_PCS),
                          Sx_runnynose_PCS = ifelse(Sx_runnynose == 0, 0, Sx_runnynose_PCS),
                          Sx_headache_PCS = ifelse(Sx_headache == 0, 0, Sx_headache_PCS),
                          Sx_cough_PCS = ifelse(Sx_cough == 0, 0, Sx_cough_PCS),
                          Sx_musclejointpain_PCS = ifelse(Sx_musclejointpain == 0, 0, Sx_musclejointpain_PCS),
                          Sx_fatigue_PCS = ifelse(Sx_fatigue == 0, 0, Sx_fatigue_PCS),
                          Sx_tremor_PCS = ifelse(Sx_tremor == 0, 0, Sx_tremor_PCS),
                          Sx_tremor_PCS = ifelse(Sx_tremor == 0, 0, Sx_tremor_PCS))


final %>% saveRDS("clinical_bqc19.rds")

final <- readRDS("clinical_bqc19.rds")
