setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA")
covid <- readRDS("coviddata.20211007.rds")

###what kind of symptoms
tmp <- covid %>% filter(C2 == 1)
dim(tmp)
table(tmp$hospital)
tmp <- tmp %>% mutate(Sx_fatigue = moderatesevere_FATIG,
                      Sx_weakness = moderatesevere_WEAK,
                      Sx_fatigue_PCS = moderatesevere_FATIG_PCS,
                      Sx_weakness_PCS = moderatesevere_WEAK_PCS,
                      Sx_earpain = moderatesevere_EARP,
                      Sx_earpain_PCS = moderatesevere_EARP_PCS,
                      Sx_sorethroat = moderatesevere_THROAT,
                      Sx_sorethroat_PCS = moderatesevere_THROAT_PCS,
                      Sx_hoarseness = moderatesevere_HOARSE,
                      Sx_hoarseness_PCS = moderatesevere_HOARSE_PCS,
                      Sx_cough = case_when(moderatesevere_DRYCO == 1 ~ 1,
                                           moderatesevere_WETCO == 1 ~ 1,
                                           moderatesevere_DRYCO == 0 ~ 0,
                                           moderatesevere_WETCO == 0 ~ 0),
                      Sx_cough_PCS = case_when(moderatesevere_DRYCO_PCS == 1 ~ 1,
                                           moderatesevere_WETCO_PCS == 1 ~ 1,
                                           moderatesevere_DRYCO_PCS == 0 ~ 0,
                                           moderatesevere_WETCO_PCS == 0 ~ 0),
                      Sx_SOB = moderatesevere_BREATH,
                      Sx_SOB_PCS = moderatesevere_BREATH_PCS,
                      Sx_runnynose = moderatesevere_RSNOSE,
                      Sx_runnynose_PCS = moderatesevere_RSNOSE_PCS,
                      Sx_headache = moderatesevere_HEAD,
                      Sx_headache_PCS = moderatesevere_HEAD_PCS,
                      Sx_musclejointpain = moderatesevere_PAIN,
                      Sx_musclejointpain_PCS = moderatesevere_PAIN_PCS,
                      Sx_appetite = moderatesevere_APPETIT,
                      Sx_appetite_PCS = moderatesevere_APPETIT_PCS,
                      Sx_diarrhea = moderatesevere_DIARR,
                      Sx_diarrhea_PCS = moderatesevere_DIARR_PCS,
                      Sx_nausia = moderatesevere_NAUS,
                      Sx_nausia_PCS = moderatesevere_NAUS_PCS,
                      Sx_smell = moderatesevere_SMELL,
                      Sx_smell_PCS = moderatesevere_SMELL_PCS,
                      Sx_confusion = moderatesevere_CONFU,
                      Sx_confusion_PCS = moderatesevere_CONFU_PCS,
                      Sx_tremor = moderatesevere_TREM,
                      Sx_tremor_PCS = moderatesevere_TREM_PCS
                      )

saveRDS(tmp, file="CLSA_data.rds")


summary <- tmp %>% summarize_at(.vars=vars(colnames(tmp)[grepl("^Sx_", colnames(tmp))]), .funs=funs(sum(., na.rm=T))) 

data <- data.frame(t(summary))
colnames(data) <- "Number"
data$variable = rownames(data)

data <- data[-1,]

data <- data %>% mutate(group = case_when(grepl("PCS", variable) ~ "persistent (>2mo)",
                                          TRUE ~ "acute"),
                        Sx = gsub("_PCS","",variable))

data <- data %>% arrange(group, Number)

data$Sx <- factor(data$Sx, levels = unique(data$Sx))
data <- data %>% mutate(Sx = case_when(Sx == "Sx_any" ~ "Any symptoms",
                                       Sx == "Sx_fatigue" ~ "Fatigue",
                                       Sx == "Sx_musclejointpain" ~ "Muscle/Joint pain",
                                       Sx == "Sx_cough" ~ "Cough",
                                       Sx == "Sx_headache" ~ "Headache",
                                       Sx == "Sx_runnynose" ~ "Rhinorrhea",
                                       Sx == "Sx_sorethroat" ~ "Sore throat",
                                       Sx == "Sx_SOB" ~ "Shortness of breath",
                                       Sx == "Sx_weakness" ~ "Muscle weakness",
                                       Sx == "Sx_diarrhea" ~ "Diarrhea",
                                       Sx == "Sx_appetite" ~ "Loss of Appetite",
                                       Sx == "Sx_hoarseness" ~ "Hoarseness",
                                       Sx == "Sx_smell" ~ "Loss of smell/taste",
                                       Sx == "Sx_earpain" ~ "Ear pain",
                                       Sx == "Sx_nausia" ~ "Nausia",
                                       Sx == "Sx_confusion" ~ "Confusion",
                                       Sx == "Sx_tremor" ~ "Tremor"
                                       ))

data$Sx <- factor(data$Sx, levels = unique(data$Sx))

saveRDS(data, file="COVID_acute_PCS.data.rds")

###any Sx
tmp <- readRDS("CLSA_data.rds")
cases <- tmp %>% filter(C2 == 1)

symptoms <- colnames(tmp)[grepl("^Sx_", colnames(tmp))]
symptoms <- symptoms[c(-1)]
symptoms <- symptoms[!grepl("_PCS$", symptoms)]

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ age_range + sex + hospital + obesity + smoking + 
               com_asthma + com_copd + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family="binomial")
  tmp <- cases[,c(symptoms[i], "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "CLSA",
                         Sx = symptoms[i],
                         casecontrol = "case",
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="CLSA_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

controls <- covid %>% filter(C2 == 0)
controls <- controls %>% mutate(Sx_fatigue = moderatesevere_FATIG,
                      Sx_weakness = moderatesevere_WEAK,
                      Sx_fatigue_PCS = moderatesevere_FATIG_PCS,
                      Sx_weakness_PCS = moderatesevere_WEAK_PCS,
                      Sx_earpain = moderatesevere_EARP,
                      Sx_earpain_PCS = moderatesevere_EARP_PCS,
                      Sx_sorethroat = moderatesevere_THROAT,
                      Sx_sorethroat_PCS = moderatesevere_THROAT_PCS,
                      Sx_hoarseness = moderatesevere_HOARSE,
                      Sx_hoarseness_PCS = moderatesevere_HOARSE_PCS,
                      Sx_cough = case_when(moderatesevere_DRYCO == 1 ~ 1,
                                           moderatesevere_WETCO == 1 ~ 1,
                                           moderatesevere_DRYCO == 0 ~ 0,
                                           moderatesevere_WETCO == 0 ~ 0),
                      Sx_cough_PCS = case_when(moderatesevere_DRYCO_PCS == 1 ~ 1,
                                               moderatesevere_WETCO_PCS == 1 ~ 1,
                                               moderatesevere_DRYCO_PCS == 0 ~ 0,
                                               moderatesevere_WETCO_PCS == 0 ~ 0),
                      Sx_SOB = moderatesevere_BREATH,
                      Sx_SOB_PCS = moderatesevere_BREATH_PCS,
                      Sx_runnynose = moderatesevere_RSNOSE,
                      Sx_runnynose_PCS = moderatesevere_RSNOSE_PCS,
                      Sx_headache = moderatesevere_HEAD,
                      Sx_headache_PCS = moderatesevere_HEAD_PCS,
                      Sx_musclejointpain = moderatesevere_PAIN,
                      Sx_musclejointpain_PCS = moderatesevere_PAIN_PCS,
                      Sx_appetite = moderatesevere_APPETIT,
                      Sx_appetite_PCS = moderatesevere_APPETIT_PCS,
                      Sx_diarrhea = moderatesevere_DIARR,
                      Sx_diarrhea_PCS = moderatesevere_DIARR_PCS,
                      Sx_nausia = moderatesevere_NAUS,
                      Sx_nausia_PCS = moderatesevere_NAUS_PCS,
                      Sx_smell = moderatesevere_SMELL,
                      Sx_smell_PCS = moderatesevere_SMELL_PCS,
                      Sx_confusion = moderatesevere_CONFU,
                      Sx_confusion_PCS = moderatesevere_CONFU_PCS,
                      Sx_tremor = moderatesevere_TREM,
                      Sx_tremor_PCS = moderatesevere_TREM_PCS
)

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ age_range + sex + hospital + obesity + smoking + 
               com_asthma + com_copd + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=controls, family="binomial")
  tmp <- controls[,c(symptoms[i], "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "CLSA",
                         Sx = symptoms[i],
                         casecontrol = "control",
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="CLSA_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

out <- fread("CLSA_summary.logreg.tsv")
colnames(out) <- colnames(out1)
write.table(out, file="CLSA_summary.logreg.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)


##interaction
covid <- readRDS("coviddata.20211007.rds")
covid <- covid %>% mutate(Sx_fatigue = moderatesevere_FATIG,
                      Sx_weakness = moderatesevere_WEAK,
                      Sx_fatigue_PCS = moderatesevere_FATIG_PCS,
                      Sx_weakness_PCS = moderatesevere_WEAK_PCS,
                      Sx_earpain = moderatesevere_EARP,
                      Sx_earpain_PCS = moderatesevere_EARP_PCS,
                      Sx_sorethroat = moderatesevere_THROAT,
                      Sx_sorethroat_PCS = moderatesevere_THROAT_PCS,
                      Sx_hoarseness = moderatesevere_HOARSE,
                      Sx_hoarseness_PCS = moderatesevere_HOARSE_PCS,
                      Sx_cough = case_when(moderatesevere_DRYCO == 1 ~ 1,
                                           moderatesevere_WETCO == 1 ~ 1,
                                           moderatesevere_DRYCO == 0 ~ 0,
                                           moderatesevere_WETCO == 0 ~ 0),
                      Sx_cough_PCS = case_when(moderatesevere_DRYCO_PCS == 1 ~ 1,
                                               moderatesevere_WETCO_PCS == 1 ~ 1,
                                               moderatesevere_DRYCO_PCS == 0 ~ 0,
                                               moderatesevere_WETCO_PCS == 0 ~ 0),
                      Sx_SOB = moderatesevere_BREATH,
                      Sx_SOB_PCS = moderatesevere_BREATH_PCS,
                      Sx_runnynose = moderatesevere_RSNOSE,
                      Sx_runnynose_PCS = moderatesevere_RSNOSE_PCS,
                      Sx_headache = moderatesevere_HEAD,
                      Sx_headache_PCS = moderatesevere_HEAD_PCS,
                      Sx_musclejointpain = moderatesevere_PAIN,
                      Sx_musclejointpain_PCS = moderatesevere_PAIN_PCS,
                      Sx_appetite = moderatesevere_APPETIT,
                      Sx_appetite_PCS = moderatesevere_APPETIT_PCS,
                      Sx_diarrhea = moderatesevere_DIARR,
                      Sx_diarrhea_PCS = moderatesevere_DIARR_PCS,
                      Sx_nausia = moderatesevere_NAUS,
                      Sx_nausia_PCS = moderatesevere_NAUS_PCS,
                      Sx_smell = moderatesevere_SMELL,
                      Sx_smell_PCS = moderatesevere_SMELL_PCS,
                      Sx_confusion = moderatesevere_CONFU,
                      Sx_confusion_PCS = moderatesevere_CONFU_PCS,
                      Sx_tremor = moderatesevere_TREM,
                      Sx_tremor_PCS = moderatesevere_TREM_PCS
)

symptoms <- colnames(covid)[grepl("^Sx_", colnames(covid))]
LM1 <- glm(paste0("Sx_any ~ C2*(age_range + sex + hospital + obesity + smoking + 
               com_asthma + com_copd + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia)"), data=covid, family="binomial")
tmp <- covid[,c("Sx_any", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                     "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "CLSA",
                       Sx = "Sx_any",
                       casecontrol = "covid",
                       covariates = rownames(out),
                       Ncase = sum(tmp[,"Sx_any"]),
                       Ncontrol = dim(tmp)[1] - sum(tmp[,"Sx_any"]))

write.table(out1, file="CLSA_summary.interaction.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)

out1 <- out1 %>% mutate(OR = exp(Estimate),
                        LL = exp(Estimate - qnorm(0.975)*Std..Error),
                        UL = exp(Estimate + qnorm(0.975)*Std..Error))

out1 <- out1 %>% rename(Factors = covariates,
                        Pvalue = Pr...z..,
                        Cohort = group) %>%
  select(Factors, OR, LL, UL, Pvalue, Ncase, Ncontrol)
write.xlsx(out1, "CLSA_summary.interaction.xlsx")

