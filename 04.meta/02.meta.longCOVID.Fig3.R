setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/")

out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_summary.longCOVID.tsv") 
out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.longCOVID.tsv") 
out3 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/UKB_PCS.summary.tsv") %>% filter(group == "UKB_C1") %>%
  mutate(group = "UKB") %>% mutate(Sx = "Sx_any_PCS")

OUT <- bind_rows(out1, out2, out3)

OUT <- OUT %>% mutate(covariates = case_when(covariates == "com_COPD" ~ "com_copd",
                                             covariates == "age_range1<60" ~ "age_range<60",
                                             covariates == "age_range50-60" ~ "age_range<60",
                                             covariates == "age_range1>70" ~ "age_range70-80",
                                             covariates == "hospital" ~ "hospitalization",
                                             TRUE ~ covariates),
                      Sx = case_when(Sx == "sx_any_PCS" ~ "Sx_any_PCS",
                                     TRUE ~ Sx))

sx <- unique(OUT$Sx)
sx <- sx[!is.na(sx)]
cov <- unique(OUT$covariates)
table(OUT[,c("covariates", "group")])

OUT1 <- OUT %>% filter(Sx == "Sx_any_PCS") %>% 
  mutate(OR = exp(Estimate),
                      LL = exp(Estimate - qnorm(0.975)*Std..Error),
                      UL = exp(Estimate + qnorm(0.975)*Std..Error)) %>%
  rename(Cohort = group, 
         Pvalue = Pr...z..,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Ncase, Ncontrol, Cohort)

write.xlsx(OUT1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/longCOVID_individual.xlsx")

data <- data.frame(matrix(0, length(cov)*2, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "sx")
data$sx <- "longCOVID"
library(meta)
for(j in c(1:length(cov))){
    tmp <- OUT %>% filter(covariates == cov[j] & Sx == "Sx_any_PCS") 
    m1 <- metagen(Estimate,
                Std..Error,
                data=tmp,
                studlab=paste(group),
                comb.fixed = TRUE,
                comb.random = TRUE,
                prediction = FALSE,
                sm="OR")
  data$covariates[(2*j-1):(2*j)] <- cov[j]
  data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
  data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
  data[(2*j-1):(2*j),5] <- c("fixed", "random")
  }
write.table(data, file="meta_summary.longCOVID.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=F)

data <- fread("meta_summary.longCOVID.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta", "sx")
write.table(data, file="meta_summary.longCOVID.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

data <- data %>% mutate(OR = exp(beta),
                         LL = exp(beta - qnorm(0.975)*se),
                         UL = exp(beta + qnorm(0.975)*se))
data1 <- data %>%
  rename(Model = meta, 
         Pvalue = pval,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Model)

write.xlsx(data1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.longCOVID.xlsx")
data1 <- data %>% filter(meta == "fixed" & sx == "longCOVID")

data1$covariates <- rep(c("Hospitalization", "70-79","<60", "≥80", "Female", "BMI≥30", 
                      "Ever smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune Disease",
                      "Cancer", "Dementia"))


tmp <- data1[1,]
tmp$covariates <- rep(c("60-69 (ref)"))
tmp$beta <- 0
tmp$se <- 0
tmp$pval <- 1
tmp$meta <- "fixed"
tmp$OR <- 1
tmp$LL <- 1
tmp$UL <- 1

data1 <- bind_rows(data1, tmp)

data1 <- data1 %>% mutate(OR = as.numeric(OR),
                          LL = as.numeric(LL),
                          UL = as.numeric(UL)) 

data1$covariates <- factor(data1$covariates, levels = rev(c("Hospitalization", "<60", "60-69 (ref)",  "70-79","≥80",
                                                        "Female", "BMI≥30", 
                                                        "Ever smoker","Asthma", "COPD", "Autoimmune Disease","Hypertension", "Diabetes", 
                                                        "Cancer", "Dementia")))

data1 <- data1 %>% mutate(sig = case_when(as.numeric(pval) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(pval) > 0.05 ~ 21,
                                          as.numeric(pval) <= 0.05 & OR > 1 ~ 24,
                                          as.numeric(pval) <= 0.05 & OR < 1 ~ 25))

data1 <- data1 %>% mutate(group = case_when(covariates %in% c("Hospitalization") ~ "1",
                                            covariates %in% c("<60", "70-79", "≥80", "60-69 (ref)") ~ "2",
                                            covariates %in% c("Female", "BMI≥30", "Ever smoker") ~ "3",
                                            covariates %in% c("Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune Disease", "Cancer", "Dementia") ~ "4"
                                            ))
png("/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/Fig3.longCOVID.png", width = 700, height = 600)
data1 %>% 
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=covariates, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + 
  scale_shape_identity() +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  #scale_color_manual(values=c("#FF8000","#00994C"), label = c("non-COVID","COVID")) + 
  #scale_fill_manual(values=c("#FF8000","#00994C")) + 
  scale_y_continuous(breaks=c(0.2, 0.5, 1.0, 1.5, 2, 3), limit=c(0.16, 3.9)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=20,face="bold"),
        axis.text.y=element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=20,face="bold"),
        legend.title = element_blank(),
        legend.text = element_blank()) +coord_flip() + guides(col = FALSE, fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE) 
dev.off()


###nonhospitalized
out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_summary.longCOVID.mild.tsv") 
out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.longCOVID.mild.tsv") 
out3 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/UKB_PCS.summary.mild.tsv") %>% filter(group == "UKB_C1") %>%
  mutate(group = "UKB") %>% mutate(Sx = "Sx_any_PCS")

OUT <- bind_rows(out1, out2, out3)

OUT <- OUT %>% mutate(covariates = case_when(covariates == "com_COPD" ~ "com_copd",
                                             covariates == "age_range1<60" ~ "age_range<60",
                                             covariates == "age_range50-60" ~ "age_range<60",
                                             covariates == "age_range1>70" ~ "age_range70-80",
                                             covariates == "hospital" ~ "hospitalization",
                                             TRUE ~ covariates),
                      Sx = case_when(Sx == "sx_any_PCS" ~ "Sx_any_PCS",
                                     TRUE ~ Sx))

sx <- unique(OUT$Sx)
sx <- sx[!is.na(sx)]
cov <- unique(OUT$covariates)
table(OUT[,c("covariates", "group")])

OUT1 <- OUT %>% filter(Sx == "Sx_any_PCS") %>% 
  mutate(OR = exp(Estimate),
         LL = exp(Estimate - qnorm(0.975)*Std..Error),
         UL = exp(Estimate + qnorm(0.975)*Std..Error)) %>%
  rename(Cohort = group, 
         Pvalue = Pr...z..,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Ncase, Ncontrol, Cohort)

write.xlsx(OUT1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/longCOVID_individual.mild.xlsx")

data <- data.frame(matrix(0, length(cov)*2, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "sx")
data$sx <- "longCOVID"
library(meta)
for(j in c(1:length(cov))){
  tmp <- OUT %>% filter(covariates == cov[j] & Sx == "Sx_any_PCS") 
  m1 <- metagen(Estimate,
                Std..Error,
                data=tmp,
                studlab=paste(group),
                comb.fixed = TRUE,
                comb.random = TRUE,
                prediction = FALSE,
                sm="OR")
  data$covariates[(2*j-1):(2*j)] <- cov[j]
  data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
  data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
  data[(2*j-1):(2*j),5] <- c("fixed", "random")
}
write.table(data, file="meta_summary.longCOVID.mild.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=F)

data <- fread("meta_summary.longCOVID.mild.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta", "sx")
write.table(data, file="meta_summary.longCOVID.mild.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))
data1 <- data %>%
  rename(Model = meta, 
         Pvalue = pval,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Model)

write.xlsx(data1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.longCOVID.mild.xlsx")


###vaccination
out1 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.longCOVID.vaccination.tsv") 
out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/UKB_PCS.summary.vaccination.tsv") %>% filter(group == "UKB_C1") %>%
  mutate(group = "UKB") %>% mutate(Sx = "Sx_any_PCS")

OUT <- bind_rows(out1, out2)

OUT <- OUT %>% mutate(covariates = case_when(covariates == "com_COPD" ~ "com_copd",
                                             covariates == "age_range1<60" ~ "age_range<60",
                                             covariates == "age_range50-60" ~ "age_range<60",
                                             covariates == "age_range1>70" ~ "age_range70-80",
                                             covariates == "hospital" ~ "hospitalization",
                                             TRUE ~ covariates),
                      Sx = case_when(Sx == "sx_any_PCS" ~ "Sx_any_PCS",
                                     TRUE ~ Sx))

sx <- unique(OUT$Sx)
sx <- sx[!is.na(sx)]
cov <- unique(OUT$covariates)
table(OUT[,c("covariates", "group")])

OUT1 <- OUT %>% filter(Sx == "Sx_any_PCS") %>% 
  mutate(OR = exp(Estimate),
         LL = exp(Estimate - qnorm(0.975)*Std..Error),
         UL = exp(Estimate + qnorm(0.975)*Std..Error)) %>%
  rename(Cohort = group, 
         Pvalue = Pr...z..,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Ncase, Ncontrol, Cohort)

write.xlsx(OUT1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/longCOVID_individual.vaccination.xlsx")

data <- data.frame(matrix(0, length(cov)*2, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "sx")
data$sx <- "longCOVID"
library(meta)
for(j in c(1:length(cov))){
  tmp <- OUT %>% filter(covariates == cov[j] & Sx == "Sx_any_PCS") 
  m1 <- metagen(Estimate,
                Std..Error,
                data=tmp,
                studlab=paste(group),
                comb.fixed = TRUE,
                comb.random = TRUE,
                prediction = FALSE,
                sm="OR")
  data$covariates[(2*j-1):(2*j)] <- cov[j]
  data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
  data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
  data[(2*j-1):(2*j),5] <- c("fixed", "random")
}
write.table(data, file="meta_summary.longCOVID.vaccination.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=F)

data <- fread("meta_summary.longCOVID.vaccination.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta", "sx")
write.table(data, file="meta_summary.longCOVID.vaccination.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))
data1 <- data %>%
  rename(Model = meta, 
         Pvalue = pval,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Model)

write.xlsx(data1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.longCOVID.vaccination.xlsx")

###vaccination nonhospital
out1 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.longCOVID.vaccination.mild.tsv") 
out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/UKB_PCS.summary.vaccination.mild.tsv") %>% filter(group == "UKB_C1") %>%
  mutate(group = "UKB") %>% mutate(Sx = "Sx_any_PCS")

OUT <- bind_rows(out1, out2)

OUT <- OUT %>% mutate(covariates = case_when(covariates == "com_COPD" ~ "com_copd",
                                             covariates == "age_range1<60" ~ "age_range<60",
                                             covariates == "age_range50-60" ~ "age_range<60",
                                             covariates == "age_range1>70" ~ "age_range70-80",
                                             covariates == "hospital" ~ "hospitalization",
                                             TRUE ~ covariates),
                      Sx = case_when(Sx == "sx_any_PCS" ~ "Sx_any_PCS",
                                     TRUE ~ Sx))

sx <- unique(OUT$Sx)
sx <- sx[!is.na(sx)]
cov <- unique(OUT$covariates)
table(OUT[,c("covariates", "group")])

OUT1 <- OUT %>% filter(Sx == "Sx_any_PCS") %>% 
  mutate(OR = exp(Estimate),
         LL = exp(Estimate - qnorm(0.975)*Std..Error),
         UL = exp(Estimate + qnorm(0.975)*Std..Error)) %>%
  rename(Cohort = group, 
         Pvalue = Pr...z..,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Ncase, Ncontrol, Cohort)

write.xlsx(OUT1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/longCOVID_individual.vaccination.mild.xlsx")

data <- data.frame(matrix(0, length(cov)*2, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "sx")
data$sx <- "longCOVID"
library(meta)
for(j in c(1:length(cov))){
  tmp <- OUT %>% filter(covariates == cov[j] & Sx == "Sx_any_PCS") 
  m1 <- metagen(Estimate,
                Std..Error,
                data=tmp,
                studlab=paste(group),
                comb.fixed = TRUE,
                comb.random = TRUE,
                prediction = FALSE,
                sm="OR")
  data$covariates[(2*j-1):(2*j)] <- cov[j]
  data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
  data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
  data[(2*j-1):(2*j),5] <- c("fixed", "random")
}
write.table(data, file="meta_summary.longCOVID.vaccination.mild.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=F)

data <- fread("meta_summary.longCOVID.vaccination.mild.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta", "sx")
write.table(data, file="meta_summary.longCOVID.vaccination.mild.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))
data1 <- data %>%
  rename(Model = meta, 
         Pvalue = pval,
         RiskFactor = covariates) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Model)

write.xlsx(data1, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.longCOVID.vaccination.mild.xlsx")


