setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

library(data.table)
library(tidyr)
library(dplyr)
data <- readRDS("clinical_bqc19.rds")
table(data[,c("covid19_test_result", "sx_any_PCS")])
66/(66+42)
911/(911+778)
#data <- data %>% mutate(smoking = case_when(smoking == "Current" ~ "Ever",
#                                            smoking == "Past" ~ "Ever",
#                                            TRUE ~ smoking))  

symptoms <- colnames(data)[grepl("^sx_", colnames(data)) & !grepl("sx_date", colnames(data))]
symptoms <- symptoms[!grepl("_PCS$", symptoms)]

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
out[3,2] <- "Basic characteristics + comorbidities"
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
out[5,2] <- "A SARS-CoV-2 positive test result\n+ basic characteristics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)
out[6,2] <- "Hospitalization\n+ basic characteristics + comorbidities"
out[6,3] <- ROC6$auc
out[6,4] <- ci.auc(ROC6, conf.level=0.95)[1]
out[6,5] <- ci.auc(ROC6, conf.level=0.95)[3]
out[6,6] <- AIC(LM6)
out[6,7] <- BIC(LM6)
out[7,2] <- "A SARS-CoV-2 positive test result\n+ hospitalization\n+ basic characteristics + comorbidities"
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
write.xlsx(out, file="longCOVID_AUC.xlsx")

out <- read.xlsx("longCOVID_AUC.xlsx")
out$model <- factor(out$model, levels = out$model)

png("/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/Fig2.AUC.png",width=500, height=600)
p1 <- ggplot(out, aes(x=model, y=AUC, ymin=AUC.LL, ymax=AUC.UL, fill=model)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0.5,0.75)) + 
  xlab("") + ylab("AUC (95% Confidence Interval)") + geom_text(aes(y=AUC.UL+0.03, label=round(AUC,2), hjust = 0.5,vjust=0),size=6, position=position_dodge(.9))+
  theme_classic() + ggtitle("") +
  scale_fill_brewer(palette = "Set3")  +
  theme(text = element_text(size=15, face="bold"),legend.position = "none",
        axis.text.x = element_text(angle = 70,hjust = 1, size=15)) + guides(color = guide_legend(legend.position = "None"))

p1
dev.off()

##################################

library(dplyr)

explanatory_factor <- c("covid19_test_result","hospitalization","sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                        "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")

cases <- data[,c("sx_any_PCS","covid19_test_result","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))

cases_new <- cases %>% 
  mutate_at(.vars=vars(c(explanatory_factor)),
            .funs=funs(forcats::fct_explicit_na(as.factor(.)))) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm=T), age))
cases_new <- cases_new[complete.cases(cases_new),]

out <- data.frame(matrix(0, 5, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

LM1 <- glm(paste0("sx_any_PCS ~ covid19_test_result"), data=cases_new, family=binomial(link="logit"))
LM2 <- glm(paste0("sx_any_PCS ~ hospitalization"), data=cases_new, family=binomial(link="logit"))
LM3 <- glm(paste0("sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
LM4 <- glm(paste0("sx_any_PCS ~ hospitalization +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
LM5 <- glm(paste0("sx_any_PCS ~ covid19_test_result +hospitalization + age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
cases_new$pred1 <- predict(LM1, cases_new)
cases_new$pred2 <- predict(LM2, cases_new)
cases_new$pred3 <- predict(LM3, cases_new)
cases_new$pred4 <- predict(LM4, cases_new)
cases_new$pred5 <- predict(LM5, cases_new)
cases_new <- data.frame(cases_new)
response <- as.numeric(unlist(cases_new[,"sx_any_PCS"]))
ROC1 <- roc(response=response, predictor=cases_new$pred1) 
ROC2 <- roc(response=response, predictor=cases_new$pred2)
ROC3 <- roc(response=response, predictor=cases_new$pred3)
ROC4 <- roc(response=response, predictor=cases_new$pred4) 
ROC5 <- roc(response=response, predictor=cases_new$pred5)

out[,1] <- "Persistent symptoms"
out[1,2] <- "SARS-CoV-2 test positivity"
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
out[3,2] <- "Demographics + comorbidities"
out[3,3] <- ROC3$auc
out[3,4] <- ci.auc(ROC3, conf.level=0.95)[1]
out[3,5] <- ci.auc(ROC3, conf.level=0.95)[3]
out[3,6] <- AIC(LM3)
out[3,7] <- BIC(LM3)
out[4,2] <- "Hospitalization\n+ demographics + comorbidities"
out[4,3] <- ROC4$auc
out[4,4] <- ci.auc(ROC4, conf.level=0.95)[1]
out[4,5] <- ci.auc(ROC4, conf.level=0.95)[3]
out[4,6] <- AIC(LM4)
out[4,7] <- BIC(LM4)
out[5,2] <- "SARS-CoV-2 test positivity + hospitalization\n+ demographics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)

plot.roc(ROC1)
plot.roc(ROC5, add=T)
roc.test(ROC1, ROC3, method="delong", alternative="two.sided")$p
roc.test(ROC1, ROC4, method="delong", alternative="two.sided")$p
roc.test(ROC4, ROC5, method="delong", alternative="two.sided")$p

write.xlsx(out, file="longCOVID.missing.xlsx")

################################


out <- data.frame(matrix(0, 5, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

#cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "com_asthma")]
cases <- data[,c("sx_any_PCS","covid19_test_result","hospitalization","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
library(mice)

#cases <- cases %>% mutate(sex = ifelse(sex == "F", 1, 0),
#                          Current = case_when(smoking == "Current" ~ 1, 
#                                              smoking == "Past" ~ 0, 
#                                              smoking == "0Never" ~ 0),
#                          Ever = case_when(smoking == "Current" ~ 1, 
#                                           smoking == "Past" ~ 1, 
#                                           smoking == "0Never" ~ 0)) %>% select(-smoking)
  
library("tidyverse")

cases <- cases %>% mutate_at(.vars=vars(c("sx_any_PCS", "covid19_test_result", "hospitalization", "obesity", colnames(cases)[grepl("^com_", colnames(cases))])),
                             .funs=funs(as.factor(.)))
imp1 <- mice(cases, m = 20)
imp_tot2 <- complete(imp1, "long", inc = TRUE)
for(i in seq(1,20)){
  out <- data.frame(matrix(0, 5, 6))
  colnames(out) <- c("imp","model","AUC","AUC.se", "AIC", "BIC")
  tmp <- imp_tot2 %>% filter(.imp == i)
  LM1 <- glm(paste0("sx_any_PCS ~ covid19_test"), data=tmp, family=binomial(link="logit"))
  LM2 <- glm(paste0("sx_any_PCS ~ hospital"), data=tmp, family=binomial(link="logit"))
  LM3 <- glm(paste0("sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  LM4 <- glm(paste0("Sx_any_PCS ~ hospital +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  LM5 <- glm(paste0("Sx_any_PCS ~ covid19_test +hospital+ age + sex + obesity + smoking +  
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  tmp$pred1 <- predict(LM1, tmp)
  tmp$pred2 <- predict(LM2, tmp)
  tmp$pred3 <- predict(LM3, tmp)
  tmp$pred4 <- predict(LM4, tmp)
  tmp$pred5 <- predict(LM5, tmp)
  tmp <- data.frame(tmp)
  response <- as.numeric(unlist(tmp[,"Sx_any_PCS"]))
  ROC1 <- roc(response=response, predictor=tmp$pred1) 
  ROC2 <- roc(response=response, predictor=tmp$pred2)
  ROC3 <- roc(response=response, predictor=tmp$pred3)
  ROC4 <- roc(response=response, predictor=tmp$pred4) 
  ROC5 <- roc(response=response, predictor=tmp$pred5)
  
  out[,1] <- i
  out[1,2] <- "SARS-CoV-2 test positivity"
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
  out[4,2] <- "Hospitalization + demographics + comorbidities"
  out[4,3] <- ROC4$auc
  out[4,4] <- (ci.auc(ROC4, conf.level=0.95)[3] - ci.auc(ROC4, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[4,5] <- AIC(LM4)
  out[4,6] <- BIC(LM4)
  out[5,2] <- "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities"
  out[5,3] <- ROC5$auc
  out[5,4] <- (ci.auc(ROC5, conf.level=0.95)[3] - ci.auc(ROC5, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[5,5] <- AIC(LM5)
  out[5,6] <- BIC(LM5)
  write.table(out, file="AUC.imp.tsv", quote=F, col.names = F, row.names = F, append = T, sep="\t")
}
out <- fread("AUC.imp.tsv")
colnames(out) <- c("imp","model","AUC","AUC.se", "AIC", "BIC")
write.table(out, file="AUC.imp.tsv", quote=F, col.names = T, row.names = F, append = F, sep="\t")
library(psfmi)
out <- data.frame(matrix(0, 5, 4))
colnames(out) <- c("model","AUC","AUC.LL", "AUC.UL")

out[1:5,1] <- c("SARS-CoV-2 test positivity", "Hospitalization", "Demographics + comorbidities", 
                "Hospitalization + demographics + comorbidities", 
                "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities")
data <- fread("AUC.imp.tsv")
modellist <- c("SARS-CoV-2 test positivity", "Hospitalization", "Demographics + comorbidities", 
               "Hospitalization + demographics + comorbidities", 
               "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities")
for(i in seq(1,5)){
  tmp <- data %>% filter(model == modellist[i])
  out[i,2] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[2]
  out[i,3] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[1]
  out[i,4] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[3]
}

out %>% write.xlsx("imputation.AUC.xlsx")


