setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

#missingness pattern
library(finalfit) 
data <- readRDS("clinical_bqc19.rds")
cases <- data[,c("sx_any_PCS","covid19_test_result","death","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
cases <- cases %>% mutate_at(.vars=vars(c("sx_any_PCS", "covid19_test_result", "hospitalization","death", "obesity", colnames(cases)[grepl("^com_", colnames(cases))])),
                             .funs=funs(as.factor(.)))
explanatory = c("covid19_test_result","hospitalization","death","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")
dependent = "sx_any_PCS"

# cases %>% 
#   missing_pattern(dependent, explanatory)
# cases %>% 
#   ff_glimpse(dependent, explanatory)
# cases %>% 
#   missing_plot()
# cases %>%  
#   missing_pairs(dependent, explanatory, position = "fill")
cases %>%  
  missing_compare(dependent, explanatory) %>% write.xlsx("missingcompare.persistentSx.xlsx")

explanatory = c("death","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")

explanatory = "hospitalization"
dependent = "covid19_test_result"
cases %>%  
  missing_compare("asthma", dependent)


#sensitivity1 - only hospitalized
data <- readRDS("clinical_bqc19.rds")
data <- data %>% filter(hospitalization == 1)

library(PredictABEL)
library(pROC)
out <- data.frame(matrix(0, 7, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

#cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "com_asthma")]
cases <- data[,c("sx_any_PCS","covid19_test_result","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))

cases %>% filter(covid19_test_result == "Positive") %>% dim()#3266
cases %>% filter(covid19_test_result == "Negative") %>% dim()#818

cases <- cases[complete.cases(cases),]
cases %>% filter(covid19_test_result == "Positive") %>% dim()#1277
cases %>% filter(covid19_test_result == "Negative") %>% dim()#85

cases %>% filter(sx_any_PCS == 1) %>% dim()
cases %>% filter(sx_any_PCS == 0) %>% dim()

table(cases[,c("covid19_test_result", "sx_any_PCS")])
1277/3266
85/818

LM1 <- glm(paste0("sx_any_PCS ~ covid19_test_result"), data=cases, family=binomial(link="logit"))
#LM2 <- glm(paste0("sx_any_PCS ~ hospitalization"), data=cases, family=binomial(link="logit"))
LM3 <- glm(paste0("sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
#LM4 <- glm(paste0("sx_any_PCS ~ covid19_test_result + hospitalization"), data=cases, family=binomial(link="logit"))
LM5 <- glm(paste0("sx_any_PCS ~ covid19_test_result + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
# LM6 <- glm(paste0("sx_any_PCS ~ hospitalization +  age + sex + obesity + smoking + 
#                com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
#                com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
# LM7 <- glm(paste0("sx_any_PCS ~ covid19_test_result + age + sex + obesity + smoking + 
#                com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
#                com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
cases$pred1 <- predict(LM1, cases)
#cases$pred2 <- predict(LM2, cases)
cases$pred3 <- predict(LM3, cases)
#cases$pred4 <- predict(LM4, cases)
cases$pred5 <- predict(LM5, cases)
#cases$pred6 <- predict(LM6, cases)
#cases$pred7 <- predict(LM7, cases)
cases <- data.frame(cases)
response <- as.numeric(unlist(cases[,"sx_any_PCS"]))
ROC1 <- roc(response=response, predictor=cases$pred1) 
#ROC2 <- roc(response=response, predictor=cases$pred2)
ROC3 <- roc(response=response, predictor=cases$pred3)
#ROC4 <- roc(response=response, predictor=cases$pred4) 
ROC5 <- roc(response=response, predictor=cases$pred5)
#ROC6 <- roc(response=response, predictor=cases$pred6)
#ROC7 <- roc(response=response, predictor=cases$pred7)
out[,1] <- "Persistent symptoms"
out[1,2] <- "A SARS-CoV-2 positive test result"
out[1,3] <- ROC1$auc
out[1,4] <- ci.auc(ROC1, conf.level=0.95)[1]
out[1,5] <- ci.auc(ROC1, conf.level=0.95)[3]
out[1,6] <- AIC(LM1)
out[1,7] <- BIC(LM1)
out[2,2] <- "Hospitalization"
out[2,3] <- ROC2$auc
out[2,4] <- ci.auc(ROC2, conf.level=0.95)[1]
out[2,5] <- ci.auc(ROC2, conf.level=0.95)[3]
out[2,6] <- AIC(LM2)
out[2,7] <- BIC(LM2)
out[3,2] <- "Baseline characteristics + comorbidities"
out[3,3] <- ROC3$auc
out[3,4] <- ci.auc(ROC3, conf.level=0.95)[1]
out[3,5] <- ci.auc(ROC3, conf.level=0.95)[3]
out[3,6] <- AIC(LM3)
out[3,7] <- BIC(LM3)
out[4,2] <- "A SARS-CoV-2 positive test result\n+ hospitalization"
out[4,3] <- ROC4$auc
out[4,4] <- ci.auc(ROC4, conf.level=0.95)[1]
out[4,5] <- ci.auc(ROC4, conf.level=0.95)[3]
out[4,6] <- AIC(LM4)
out[4,7] <- BIC(LM4)
out[5,2] <- "A SARS-CoV-2 positive test result\n+ baseline characteristics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)
out[6,2] <- "Hospitalization\n+ demographics + comorbidities"
out[6,3] <- ROC6$auc
out[6,4] <- ci.auc(ROC6, conf.level=0.95)[1]
out[6,5] <- ci.auc(ROC6, conf.level=0.95)[3]
out[6,6] <- AIC(LM6)
out[6,7] <- BIC(LM6)
out[7,2] <- "A SARS-CoV-2 positive test result\n+ hospitalization\n+ demographics + comorbidities"
out[7,3] <- ROC7$auc
out[7,4] <- ci.auc(ROC7, conf.level=0.95)[1]
out[7,5] <- ci.auc(ROC7, conf.level=0.95)[3]
out[7,6] <- AIC(LM7)
out[7,7] <- BIC(LM7)

plot.roc(ROC1)
plot.roc(ROC7, add=T)
roc.test(ROC1, ROC3, method="delong", alternative="two.sided")$p
roc.test(ROC1, ROC6, method="delong", alternative="two.sided")$p
roc.test(ROC4, ROC5, method="delong", alternative="two.sided")$p

out <- out %>% mutate(AUC = as.numeric(AUC),
                      AUC.LL = as.numeric(AUC.LL),
                      AUC.UL = as.numeric(AUC.UL)
)


write.xlsx(out, file="longCOVID_AUC_hospitalizedonly.xlsx")

#sensitivity2 - imputing death as with having persistent symptoms
data <- readRDS("clinical_bqc19.rds")
data <- data %>% mutate(sx_any_PCS = ifelse(death == 1 & is.na(sx_any_PCS), 1, sx_any_PCS))

#data <- data %>% mutate(smoking = case_when(smoking == "Current" ~ "Ever",
#                                            smoking == "Past" ~ "Ever",
#                                            TRUE ~ smoking))  

symptoms <- colnames(data)[grepl("^sx_", colnames(data))]
symptoms <- symptoms[grepl("_PCS$", symptoms)]

library(Hmisc)
##install.packages("PredictABEL")
library(PredictABEL)
library(pROC)
out <- data.frame(matrix(0, 7, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

#cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "com_asthma")]
cases <- data[,c("sx_any_PCS","covid19_test_result","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))

cases %>% filter(covid19_test_result == "Positive") %>% dim()#3266
cases %>% filter(covid19_test_result == "Negative") %>% dim()#818

cases <- cases[complete.cases(cases),]
cases %>% filter(covid19_test_result == "Positive") %>% dim()#1277
cases %>% filter(covid19_test_result == "Negative") %>% dim()#85

cases %>% filter(sx_any_PCS == 1) %>% dim()
cases %>% filter(sx_any_PCS == 0) %>% dim()

table(cases[,c("covid19_test_result", "sx_any_PCS")])
1277/3266
85/818

LM1 <- glm(paste0("sx_any_PCS ~ covid19_test_result"), data=cases, family=binomial(link="logit"))
LM2 <- glm(paste0("sx_any_PCS ~ hospitalization"), data=cases, family=binomial(link="logit"))
LM3 <- glm(paste0("sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
LM4 <- glm(paste0("sx_any_PCS ~ covid19_test_result + hospitalization"), data=cases, family=binomial(link="logit"))
LM5 <- glm(paste0("sx_any_PCS ~ covid19_test_result + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
LM6 <- glm(paste0("sx_any_PCS ~ hospitalization +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
LM7 <- glm(paste0("sx_any_PCS ~ covid19_test_result +hospitalization + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
cases$pred1 <- predict(LM1, cases)
cases$pred2 <- predict(LM2, cases)
cases$pred3 <- predict(LM3, cases)
cases$pred4 <- predict(LM4, cases)
cases$pred5 <- predict(LM5, cases)
cases$pred6 <- predict(LM6, cases)
cases$pred7 <- predict(LM7, cases)
cases <- data.frame(cases)
response <- as.numeric(unlist(cases[,"sx_any_PCS"]))
ROC1 <- roc(response=response, predictor=cases$pred1) 
ROC2 <- roc(response=response, predictor=cases$pred2)
ROC3 <- roc(response=response, predictor=cases$pred3)
ROC4 <- roc(response=response, predictor=cases$pred4) 
ROC5 <- roc(response=response, predictor=cases$pred5)
ROC6 <- roc(response=response, predictor=cases$pred6)
ROC7 <- roc(response=response, predictor=cases$pred7)
out[,1] <- "Persistent symptoms"
out[1,2] <- "A SARS-CoV-2 positive test result"
out[1,3] <- ROC1$auc
out[1,4] <- ci.auc(ROC1, conf.level=0.95)[1]
out[1,5] <- ci.auc(ROC1, conf.level=0.95)[3]
out[1,6] <- AIC(LM1)
out[1,7] <- BIC(LM1)
out[2,2] <- "Hospitalization"
out[2,3] <- ROC2$auc
out[2,4] <- ci.auc(ROC2, conf.level=0.95)[1]
out[2,5] <- ci.auc(ROC2, conf.level=0.95)[3]
out[2,6] <- AIC(LM2)
out[2,7] <- BIC(LM2)
out[3,2] <- "Baseline characteristics + comorbidities"
out[3,3] <- ROC3$auc
out[3,4] <- ci.auc(ROC3, conf.level=0.95)[1]
out[3,5] <- ci.auc(ROC3, conf.level=0.95)[3]
out[3,6] <- AIC(LM3)
out[3,7] <- BIC(LM3)
out[4,2] <- "A SARS-CoV-2 positive test result\n+ hospitalization"
out[4,3] <- ROC4$auc
out[4,4] <- ci.auc(ROC4, conf.level=0.95)[1]
out[4,5] <- ci.auc(ROC4, conf.level=0.95)[3]
out[4,6] <- AIC(LM4)
out[4,7] <- BIC(LM4)
out[5,2] <- "A SARS-CoV-2 positive test result\n+ baseline characteristics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)
out[6,2] <- "Hospitalization\n+ baseline characteristics + comorbidities"
out[6,3] <- ROC6$auc
out[6,4] <- ci.auc(ROC6, conf.level=0.95)[1]
out[6,5] <- ci.auc(ROC6, conf.level=0.95)[3]
out[6,6] <- AIC(LM6)
out[6,7] <- BIC(LM6)
out[7,2] <- "A SARS-CoV-2 positive test result\n+ hospitalization\n+ baseline characteristics + comorbidities"
out[7,3] <- ROC7$auc
out[7,4] <- ci.auc(ROC7, conf.level=0.95)[1]
out[7,5] <- ci.auc(ROC7, conf.level=0.95)[3]
out[7,6] <- AIC(LM7)
out[7,7] <- BIC(LM7)

plot.roc(ROC1)
plot.roc(ROC7, add=T)
roc.test(ROC1, ROC3, method="delong", alternative="two.sided")$p
roc.test(ROC1, ROC6, method="delong", alternative="two.sided")$p
roc.test(ROC4, ROC5, method="delong", alternative="two.sided")$p

out <- out %>% mutate(AUC = as.numeric(AUC),
                      AUC.LL = as.numeric(AUC.LL),
                      AUC.UL = as.numeric(AUC.UL)
)
write.xlsx(out, file="longCOVID_AUC_deathimpute.xlsx")


##multiple imputation approach
data <- readRDS("clinical_bqc19.rds")
#frequency of long-lasting Sx in non-COVID

data <- data[,c("sx_any_PCS","covid19_test_result","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
data <- data %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
data <- data %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
data <- data %>% drop_na(covid19_test_result)

# cases <- data %>% filter(covid19_test == 1)
# controls <- data %>% filter(covid19_test == 0)
# 
replace_func <- function(x) {
  inds <- is.na(x)
  if (length(x) > 1 & any(inds)) {
    x[inds] <- sample(x[!inds], sum(inds), replace = T)
    x
  } else x
}



replace_pcs <- function(x, p, r) {
  inds <- is.na(x)
  if (length(x) > 1 & any(inds)) {
    x[inds] <- sample(c(0,1), sum(inds), prob=c((1-p*r), p*r), replace = T)
    x
  } else x
}

for(r in c(0, 0.5, 1, 1.5)){
  for(i in seq(1,20)){
    cases <- data %>% filter(covid19_test_result == "Positive")
    controls <- data %>% filter(covid19_test_result == "Negative")
    cases <- cases %>% mutate_at(.vars=vars(c(age, hospitalization, sex, obesity,com_asthma, smoking, com_COPD, com_hypertension,
                             com_diabetes, com_autoimmune, com_cancer, com_dementia)), 
                .funs=funs(replace_func(.)))
    controls <- controls %>% mutate_at(.vars=vars(c(age, hospitalization, sex, obesity,com_asthma, smoking, com_COPD, com_hypertension,
                                              com_diabetes, com_autoimmune, com_cancer, com_dementia)), 
                                 .funs=funs(replace_func(.)))
    cases <- cases %>% mutate_at(.vars=vars(c(sx_any_PCS)), 
                                 .funs=funs(replace_pcs(., 0.539,r)))
    controls <- controls %>% mutate_at(.vars=vars(c(sx_any_PCS)), 
                                 .funs=funs(replace_pcs(., 0.611, r)))
    tmp <- bind_rows(cases, controls)
    out <- data.frame(matrix(0, 7, 7))
    colnames(out) <- c("caseratio","model","AUC","AUC.se", "AIC", "BIC", "controlratio")
    LM1 <- glm(paste0("sx_any_PCS ~ covid19_test_result"), data=tmp, family=binomial(link="logit"))
    LM2 <- glm(paste0("sx_any_PCS ~ hospitalization"), data=tmp, family=binomial(link="logit"))
    LM3 <- glm(paste0("sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
    LM4 <- glm(paste0("sx_any_PCS ~ covid19_test_result + hospitalization"), data=tmp, family=binomial(link="logit"))
    LM5 <- glm(paste0("sx_any_PCS ~ covid19_test_result + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
    LM6 <- glm(paste0("sx_any_PCS ~ hospitalization +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
    LM7 <- glm(paste0("sx_any_PCS ~ covid19_test_result +hospitalization + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
    tmp$pred1 <- predict(LM1, tmp)
    tmp$pred2 <- predict(LM2, tmp)
    tmp$pred3 <- predict(LM3, tmp)
    tmp$pred4 <- predict(LM4, tmp)
    tmp$pred5 <- predict(LM5, tmp)
    tmp$pred6 <- predict(LM6, tmp)
    tmp$pred7 <- predict(LM7, tmp)
    tmp <- data.frame(tmp)
    response <- as.numeric(unlist(tmp[,"sx_any_PCS"]))
    ROC1 <- roc(response=response, predictor=tmp$pred1) 
    ROC2 <- roc(response=response, predictor=tmp$pred2)
    ROC3 <- roc(response=response, predictor=tmp$pred3)
    ROC4 <- roc(response=response, predictor=tmp$pred4) 
    ROC5 <- roc(response=response, predictor=tmp$pred5)
    ROC6 <- roc(response=response, predictor=tmp$pred6)
    ROC7 <- roc(response=response, predictor=tmp$pred7)
    out[,1] <- r
    out[,7] <- r
    out[1,2] <- "A SARS-CoV-2 positive test result"
    out[1,3] <- ROC1$auc
    out[1,4] <- (ci.auc(ROC1, conf.level=0.95)[3] - ci.auc(ROC1, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[1,5] <- AIC(LM1)
    out[1,6] <- BIC(LM1)
    out[2,2] <- "Hospitalization"
    out[2,3] <- ROC2$auc
    out[2,4] <- (ci.auc(ROC2, conf.level=0.95)[3] - ci.auc(ROC2, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[2,5] <- AIC(LM2)
    out[2,6] <- BIC(LM2)
    out[3,2] <- "Demographics + comorbidities"
    out[3,3] <- ROC3$auc
    out[3,4] <- (ci.auc(ROC3, conf.level=0.95)[3] - ci.auc(ROC3, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[3,5] <- AIC(LM3)
    out[3,6] <- BIC(LM3)
    out[4,2] <- "A SARS-CoV-2 positive test result + hospitalization"
    out[4,3] <- ROC4$auc
    out[4,4] <- (ci.auc(ROC4, conf.level=0.95)[3] - ci.auc(ROC4, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[4,5] <- AIC(LM4)
    out[4,6] <- BIC(LM4)
    out[5,2] <- "A SARS-CoV-2 positive test result + baseline characteristics  + comorbidities"
    out[5,3] <- ROC5$auc
    out[5,4] <- (ci.auc(ROC5, conf.level=0.95)[3] - ci.auc(ROC5, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[5,5] <- AIC(LM5)
    out[5,6] <- BIC(LM5)
    out[6,2] <- "Hospitalization + baseline characteristics + comorbidities"
    out[6,3] <- ROC6$auc
    out[6,4] <- (ci.auc(ROC6, conf.level=0.95)[3] - ci.auc(ROC6, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[6,5] <- AIC(LM6)
    out[6,6] <- BIC(LM6)
    out[7,2] <- "A SARS-CoV-2 positive test result + hospitalization + baseline characteristics + comorbidities"
    out[7,3] <- ROC7$auc
    out[7,4] <- (ci.auc(ROC7, conf.level=0.95)[3] - ci.auc(ROC7, conf.level=0.95)[1])/(2*qnorm(0.975))
    out[7,5] <- AIC(LM7)
    out[7,6] <- BIC(LM7)
    write.table(out, file="sensitivity.AUC.imp.tsv", quote=F, col.names = F, row.names = F, append = T, sep="\t")
    }
  }

out <- read.table("sensitivity.AUC.imp.tsv", sep = "\t", header=F)
colnames(out) <- c("caseratio","model","AUC","AUC.se", "AIC", "BIC","ratio")
modellist <- out$model[1:7]
write.table(out, file="sensitivity.AUC.imp.tsv", quote=F, col.names = T, row.names = F, append = F, sep="\t")
library(psfmi)
out <- data.frame(matrix(0, 7, 9))
colnames(out) <- c("model","AUC","AUC.LL", "AUC.UL", "AIC.mean","AIC.mean","BIC.mean","BIC.sd","ratio")

out[1:7,1] <- modellist

data <- fread("sensitivity.AUC.imp.tsv")
for(r in c(0, 0.5, 1, 1.5)){
  for(i in seq(1,7)){
    tmp <- data %>% filter(model == modellist[i] & ratio ==r)
    out[i,2] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[2]
    out[i,3] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[1]
    out[i,4] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[3]
    out[i,5] <- mean(tmp$AIC)
    out[i,6] <- sd(tmp$BIC) 
    out[i,7] <- mean(tmp$AIC) 
    out[i,8] <- sd(tmp$BIC) 
    out[i,9] <- r
  }
  write.table(out, file="sensitivity.AUC.imp.res.tsv", quote=F, col.names = F, row.names = F, append = T, sep="\t")
  }


out <- fread("sensitivity.AUC.imp.res.tsv")
colnames(out) <- c("model","AUC","AUC.LL", "AUC.UL", "AIC.mean","AIC.sd","BIC.mean","BIC.sd","ratio")
write.table(out, file="sensitivity.AUC.imp.res.tsv", quote=F, col.names = T, row.names = F, append = F, sep="\t")

tmp <- out %>% group_by(ratio) %>%
  filter(AUC[model == "Hospitalization + baseline characteristics + comorbidities"] < AUC[model == "A SARS-CoV-2 positive test result"]) 

