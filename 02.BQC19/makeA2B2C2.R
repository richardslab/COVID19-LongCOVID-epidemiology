setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

data <- read.csv("/home/richards/tomoko.nakanishi/projects/richards/restricted/BQC19_Release3/BQC19_Release3_Sep_2021_Clinical_Phenotypic/redcap_clinical_data_2021-09-14.csv")

data$Date.of.earliest.symptom.s..[data$Date.of.earliest.symptom.s.. == ""] <- NA
data <- data %>% group_by(BQC.identifier..public.) %>% 
  tidyr::fill(Date.of.earliest.symptom.s.., .direction = "down") %>%
  ungroup()

data$Final.COVID.status.[data$Final.COVID.status. == ""] <- NA
data <- data %>% group_by(BQC.identifier..public.) %>% 
  tidyr::fill(Final.COVID.status., .direction = "down") %>%
  ungroup()

data$ICU.admission.[data$ICU.admission. == ""] <- NA
data <- data %>% group_by(BQC.identifier..public.) %>% 
  tidyr::fill(ICU.admission., .direction = "down") %>%
  ungroup()

data$Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient.[data$Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == ""] <- NA
data <- data %>% group_by(BQC.identifier..public.) %>% 
  tidyr::fill(Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient., .direction = "down") %>%
  ungroup()

data <- data %>% mutate(C2 = case_when(Final.COVID.status. == "Positive" ~ 1,
                                       TRUE ~ 0),
                        C1 = case_when(Final.COVID.status. == "Positive" ~ 1,
                                       Final.COVID.status. == "Negative" ~ 0,
                                       TRUE ~ -1))
data <- data %>% mutate(B2 = case_when(C2 == 1 & Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == "Hospitalized" ~ 1,
                                       TRUE ~ 0),
                        B1 = case_when(C2 == 1 & Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == "Hospitalized" ~ 1,
                                       C2 == 1 ~ 0,
                                       TRUE ~ -1))
data <- data %>% mutate(A2 = case_when(B2 == 1 & Vital.status.at.discharge. == "Deceased" ~ 1,
                                       B2 == 1 & Why.was.this.follow.up.interview.NOT.completed. == "Deseased" ~ 1,
                                       B2 == 1 & Vital.status. == "Deceased" ~ 1,
                                       B2 == 1 & If.yes...choice.High.flow.nasal.cannula..HFNC.. == "Checked" ~ 1,
                                       B2 == 1 & data$If.yes..check.all.that.apply....choice.Non.invasive.support.via.High.flow.nasal.cannula..HFNC.. == "Checked" ~ 1,
                                       B2 == 1 & If.yes...choice.Extracorporeal.membrane.oxygenation..ECMO.. == "Checked" ~ 1,
                                       B2 == 1 & If.yes...choice.Non.invasive.ventilation.CPAP.BPAP. == "Checked" ~ 1,
                                       B2 == 1 & If.yes...choice.Invasive.with.mechanical.ventilation. == "Checked" ~ 1,
                                       B2 == 1 & If.yes..check.all.that.apply....choice.Non.invasive.ventilation.CPAP.BPAP. == "Checked" ~ 1,
                                       B2 == 1 & If.yes..check.all.that.apply....choice.Non.invasive.support.via.High.flow.nasal.cannula..HFNC.. == "Checked" ~ 1,
                                       B2 == 1 & If.yes..check.all.that.apply....choice.Invasive.support.with.mechanical.ventilation. == "Checked" ~ 1,
                                       B2 == 1 & If.yes..check.all.that.apply....choice.Extracorporeal.membrane.oxygenation..ECMO.. == "Checked" ~ 1,
                                       TRUE ~ 0))
data <- data %>% mutate(A1 = case_when(A2 == 1 ~ 1,
                                       C2 == 1 ~ 0,
                                       TRUE ~ -1))

final <- data %>% group_by(BQC.identifier..public.) %>%
  mutate(A2 = max(A2), A1 = max(A1),
         B2 = max(B2), B1 = max(B1),
         C2 = max(C2), C1 = max(C1)) %>% ungroup() %>% distinct(BQC.identifier..public., .keep_all=TRUE) %>% 
  select(BQC.identifier..public., A1, A2, B1, B2, C1, C2)

saveRDS(final, file="/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/BQC19_A2B2C2.rds")


#data <- data %>% filter(Date.of.follow.up. != "")

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
                                           any(colnames(data)[grepl("^Sx_", colnames(data))] == 1) ~ 1,
                                           Asymptomatic. == "Yes" ~ 0,
                                           any(colnames(data)[grepl("^Sx_", colnames(data))] == 0) ~ 0))

final <- data %>% group_by(BQC.identifier..public.) %>%
  mutate(COVIDSx = max(COVIDSx)) %>%
  mutate(A2 = case_when(any(Vital.status.at.discharge. == "")))
mutate_at(.vars=vars(colnames(data)[grepl("^Sx_", colnames(data))]), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQC.identifier..public., .keep_all=TRUE)

tmp <- final %>% select(BQC.identifier..public., colnames(final)[grepl("^Sx_", colnames(final))])
tmp <- tmp %>% filter(!is.infinite(Sx_any))

data1 <- data %>% filter(Date.of.follow.up. != "") %>% select(-colnames(final)[grepl("^Sx_", colnames(final))])
data1 <- data1 %>% mutate(time = as.Date(Date.of.follow.up.) - as.Date(Date.of.earliest.symptom.s..))
data1 <- data1 %>% merge(tmp, by="BQC.identifier..public.")

data1 <- data1 %>% mutate(Sx_tremor_PCS = case_when(Sx_tremor == 1 & Seizure..1 == "Yes" & time > 28*2 & Final.COVID.status. == "Positive" ~ 1,
                                                    TRUE ~ 0),
                          Sx_confusion_PCS = case_when(Sx_confusion == 1 & Confusion...altered.mental.status...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                       TRUE ~ 0),
                          Sx_nausia_PCS = case_when(Sx_nausia == 1 & Nausea...vomiting...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                    TRUE ~ 0),
                          Sx_earpain_PCS = case_when(Sx_earpain == 1 & Ear.pain...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                     TRUE ~ 0),
                          Sx_smell_PCS = case_when(Sx_smell == 1 & Loss.of.taste...lost.of.smell...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                   TRUE ~ 0),
                          Sx_hoarseness_PCS = case_when(Sx_hoarseness == 1 & Trouble.speaking..Aphasia...Dysphasia....1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                        TRUE ~ 0),
                          Sx_appetite_PCS = case_when(Sx_appetite == 1 & Loss.of.appetite...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                      TRUE ~ 0),
                          Sx_diarrhea_PCS = case_when(Sx_diarrhea == 1 & Diarrhea...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                      TRUE ~ 0),
                          Sx_weakness_PCS = case_when(Sx_weakness == 1 & Extremity.weakness.or.numbness...1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                      TRUE ~ 0),
                          Sx_SOB_PCS = case_when(Sx_SOB == 1 & Shortness.of.breath..Dyspnea....1 == "Yes" & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                 TRUE ~ 0),
                          Sx_sorethroat_PCS = case_when(Sx_sorethroat == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                        TRUE ~ 0),
                          Sx_runnynose_PCS = case_when(Sx_runnynose == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                       TRUE ~ 0),
                          Sx_headache_PCS = case_when(Sx_headache == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                      TRUE ~ 0),
                          Sx_cough_PCS = case_when(Sx_cough == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                   TRUE ~ 0),
                          Sx_musclejointpain_PCS = case_when(Sx_musclejointpain == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                             TRUE ~ 0),
                          Sx_fatigue_PCS = case_when(Sx_fatigue == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                     TRUE ~ 0),
                          Sx_any_PCS = case_when(Sx_any == 1 & time > 28*2  & Final.COVID.status. == "Positive" ~ 1,
                                                 TRUE ~ 0)
)

data1 <- data1 %>% mutate(longCOVID = case_when(COVIDSx == 1 & time > 28*2 & Final.COVID.status. == "Positive" ~ 1,
                                                any(colnames(data1)[grepl("_PCS$", colnames(data1))]) == 1 ~ 1,
                                                TRUE ~ 0),
                          hospital = case_when(Has.the.participant.been.hospitalized.or.is.the.participant.seen.on.an.outpatient. == "Hospitalized" ~ 1,
                                               TRUE ~ 0),
                          icu_admit = case_when(ICU.admission. == "Yes" ~ 1,
                                                TRUE ~ 0),
                          covid19_test = case_when(Final.COVID.status. == "Positive" ~ 1,
                                                   Final.COVID.status. == "Negative" ~ 0))

tmp <- data1 %>% group_by(BQC.identifier..public.) %>%
  mutate(longCOVID = max(longCOVID),
         hospital = max(hospital),
         icu_admit = max(icu_admit),
         covid19_test = max(covid19_test)) %>%
  mutate_at(.vars=vars(colnames(data1)[grepl("_PCS$", colnames(data1))]), .funs=funs(max(., na.rm=T))) %>%
  ungroup() %>% distinct(BQC.identifier..public., .keep_all=TRUE) %>% 
  select(BQC.identifier..public., longCOVID, hospital, icu_admit, covid19_test, colnames(data1)[grepl("_PCS$", colnames(data1))])
tmp <- tmp %>% mutate(hospital = case_when(icu_admit == 1 ~ 1,
                                           TRUE ~ hospital))

final <- final %>% merge(tmp, by="BQC.identifier..public.", all.x=T)

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
final <- final %>% mutate(age_range = case_when(age < 60 & age >= 50 ~ "50-60",
                                                age < 70 & age >= 60 ~ "0Age60-70",
                                                age < 80 & age >= 70 ~ "70-80",
                                                age >= 80 ~ ">80",
                                                age < 50  ~ "<50"),
                          obesity = case_when(BMI >= 30 ~ 1,
                                              BMI < 30 ~ 0))

final %>% saveRDS("clinical_bqc19.rds")
final <- readRDS("clinical_bqc19.rds")

cases <- final %>% filter(covid19_test == 1)
LM1 <- glm(longCOVID ~ hospital + age_range + sex + obesity + smoking + com_asthma + com_COPD + 
             com_diabetes + com_cancer + com_dementia + com_hypertension + com_autoimmune, data=final, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "BQC19_C2",
                       covariates = rownames(out))

LM1 <- glm(longCOVID ~ hospital + age_range + sex + obesity + smoking + com_asthma + com_COPD + 
             com_diabetes + com_cancer + com_dementia + com_hypertension + com_autoimmune, data=cases, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out2 <- out %>% mutate(group = "BQC19_C1",
                       covariates = rownames(out))

out <- bind_rows(out1, out2)
write.table(out, file="BQC19_PCS.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

##anySx
cases <- final %>% filter(covid19_test == 1)
LM1 <- glm(Sx_any ~ hospital + age_range + sex + BMI + smoking + com_asthma + com_COPD + 
             com_diabetes + com_cancer + com_dementia + com_hypertension + com_autoimmune, data=cases, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "BQC19_case_anySx",
                       covariates = rownames(out))

controls <- final %>% filter(covid19_test == 0)
LM1 <- glm(worseningSx_final ~ hospital + age_range + sex + BMI + smoking + com_asthma + com_COPD + 
             com_diabetes + com_cancer + com_dementia + com_hypertension + com_autoimmune, data=controls, family="binomial")

summary(LM1)
out <- data.frame(summary(LM1)$coefficients[-1,])
out2 <- out %>% mutate(group = "BQC19_controls_anySx",
                       covariates = rownames(out))

out <- bind_rows(out1, out2)
write.table(out, file="BQC19_anySx.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)


out <- out %>% mutate(OR = exp(Estimate),
                      LL = exp(Estimate - qnorm(0.975)*Std..Error),
                      UL = exp(Estimate + qnorm(0.975)*Std..Error))


out %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + scale_y_log10(breaks=c(0.1, 0.3, 0.5, 1, 2, 6, 10)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        legend.title = element_blank()) +coord_flip() +guides(col = guide_legend(reverse = TRUE))


