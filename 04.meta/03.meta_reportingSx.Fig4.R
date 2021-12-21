setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/")

out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_summary.logreg.tsv")
out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.logreg.tsv")

OUT <- bind_rows(out1, out2)
OUT$covariates[OUT$covariates == "smokingCurrent"] <- "smokingcurrent"
OUT$covariates[OUT$covariates == "smokingPast"] <- "smokingpast"
OUT$covariates[OUT$covariates == "com_COPD"] <- "com_copd"
table(OUT[,c("covariates", "group")])
library(meta)

sx <- unique(OUT$Sx)
cov <- unique(OUT$covariates)
for(i in c("case", "control")){
  for(l in seq(1, length(sx))){
    data <- data.frame(matrix(0, length(cov)*2, 7))
    colnames(data) <- c("covariates","beta", "se", "pval", "meta", "sx","group")
    data$group <- i
    data$sx <- sx[l]
    for(j in seq(1, length(cov))){
      tmp <- OUT %>% filter(casecontrol == i & covariates == cov[j] & Sx == sx[l]) 
      m1 <- metagen(Estimate,
                    Std..Error,
                    data=tmp,
                    studlab=paste(group),
                    comb.fixed = TRUE,
                    comb.random = TRUE,
                    prediction=FALSE,
                    sm="OR")
      data$covariates[(2*j-1):(2*j)] <- cov[j]
      data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
      data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
      data[(2*j-1):(2*j),5] <- c("fixed", "random")
    }
    write.table(data, file="meta_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
  }
}

data <- fread("meta_summary.logreg.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta", "sx","group")
write.table(data, file="meta_summary.logreg.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))

data <- data %>% filter(sx == "Sx_any") %>% 
  rename(Model = meta, 
         Pvalue = pval,
         RiskFactor = covariates,
         COVID19Status = group) %>% 
  select(RiskFactor, OR, LL, UL, Pvalue, Model, COVID19Status)

write.xlsx(data, file = "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.reportingSx.xlsx")

data <- fread("meta_summary.logreg.tsv")
data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))

data1 <- data %>% filter(meta == "fixed")

data1$covariates <- rep(c( "70-80","<60", ">80", "Female", "Hospitalization","BMI≥30", 
                          "Current smoker", "Past smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                          "Cancer", "Dementia"), 34)

tmp <- data1[1:4,]
tmp$covariates <- rep(c("60-70 (ref)", "Never smoker (ref)"), 2)
tmp$beta <- 0
tmp$se <- 0
tmp$pval <- 1
tmp$meta <- "fixed"
tmp$group <- c(rep("case",2), rep("control", 2))
tmp$OR <- 1
tmp$LL <- 1
tmp$UL <- 1

data1 <- bind_rows(data1, tmp)

data1 <- data1 %>% mutate(OR = as.numeric(OR),
                          LL = as.numeric(LL),
                          UL = as.numeric(UL)) 

data1$covariates <- factor(data1$covariates, levels = c("Hospitalization","<60", "60-70 (ref)",  "70-80",">80",
                                                        "Female", "BMI≥30", "Never smoker (ref)",
                                                        "Current smoker", "Past smoker", "Asthma", "COPD", "Autoimmune","Hypertension", "Diabetes", 
                                                        "Cancer", "Dementia"))

data1$group <- factor(data1$group, levels = c("control", "case"))


data1 <- data1 %>% mutate(sig = case_when(as.numeric(pval) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(pval) > 0.05 ~ 21,
                                            as.numeric(pval) <= 0.05 & OR > 1 ~ 24,
                                            as.numeric(pval) <= 0.05 & OR < 1 ~ 25))

#data1 <- data1 %>% filter(covariates %in% c("Hospitalization", "Female", "BMI≥30", "Never smoker (ref)",
#                                            "Current smoker", "Past smoker", "Asthma", "COPD", "Autoimmune","Hypertension", "Diabetes", 
#                                            "Cancer", "Dementia"))

data1 %>% filter(sx == "Sx_any") %>% filter(covariates == "Autoimmune")
#breaks=c(0.1, 0.3, 0.5, 1, 2, 6, 10)
png("/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/Fig.3.anySx_casecontrol_fixed.png", width = 700, height = 800)
data1 %>% filter(sx == "Sx_any") %>% 
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  scale_shape_identity() +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  scale_color_manual(values=c("#FF8000","#00994C"), label = c("non-COVID","COVID")) + 
  scale_fill_manual(values=c("#FF8000","#00994C")) + scale_y_log10(breaks=c(0.5,1.5,2,3)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=20,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=20,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=20,face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=20)) +coord_flip() + guides(col = guide_legend(reverse = TRUE),
                                                                    fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE) 
dev.off()

OUT <- OUT %>% filter(Sx == "Sx_any")

data2 <- data1 %>% filter(sx != "Sx_any")

library(corrplot)
M <- matrix(0, 34, 16)
p <- matrix(0, 34, 16)
lci <- matrix(0, 34, 16)
hci <- matrix(0, 34, 16)
colnames(M) <- c("Sx_cough","Sx_fatigue", "Sx_SOB","Sx_headache", "Sx_musclejointpain",  "Sx_diarrhea" ,
                 "Sx_smell" , "Sx_appetite", "Sx_sorethroat","Sx_nausia" ,  "Sx_runnynose",
                 "Sx_weakness" ,  "Sx_confusion" , "Sx_earpain", "Sx_hoarseness" , "Sx_tremor" )
colnames(p) <- c("Sx_cough","Sx_fatigue", "Sx_SOB","Sx_headache", "Sx_musclejointpain",  "Sx_diarrhea" ,
                 "Sx_smell" , "Sx_appetite", "Sx_sorethroat","Sx_nausia" ,  "Sx_runnynose",
                 "Sx_weakness" ,  "Sx_confusion" , "Sx_earpain", "Sx_hoarseness" , "Sx_tremor" )
colnames(lci) <- c("Sx_cough","Sx_fatigue", "Sx_SOB","Sx_headache", "Sx_musclejointpain",  "Sx_diarrhea" ,
                   "Sx_smell" , "Sx_appetite", "Sx_sorethroat","Sx_nausia" ,  "Sx_runnynose",
                   "Sx_weakness" ,  "Sx_confusion" , "Sx_earpain", "Sx_hoarseness" , "Sx_tremor" )
colnames(hci) <- c("Sx_cough","Sx_fatigue", "Sx_SOB","Sx_headache", "Sx_musclejointpain",  "Sx_diarrhea" ,
                   "Sx_smell" , "Sx_appetite", "Sx_sorethroat","Sx_nausia" ,  "Sx_runnynose",
                   "Sx_weakness" ,  "Sx_confusion" , "Sx_earpain", "Sx_hoarseness" , "Sx_tremor" )
rownames(M) <- rep(c("COVID", "non-COVID"),17)
rownames(p) <- rep(c("COVID", "non-COVID"),17)
tmp <- c(rep("<50", 2), rep("50-60", 2), rep("60-70 (ref)", 2), rep("70-80", 2), rep(">80", 2), 
                 rep("Female", 2), rep("BMI≥30", 2), rep("Never smoker (ref)", 2), rep("Current smoker", 2), 
                 rep("Past smoker", 2), rep("Asthma", 2), rep("COPD", 2), rep("Autoimmune", 2), rep("Hypertension", 2), 
                 rep("Diabetes", 2), rep("Cancer", 2), rep("Dementia", 2))
rownames(lci) <- c(rep("<50", 2), rep("50-60", 2), rep("60-70 (ref)", 2), rep("70-80", 2), rep(">80", 2), 
                 rep("Female", 2), rep("BMI≥30", 2), rep("Never smoker (ref)", 2), rep("Current smoker", 2), 
                 rep("Past smoker", 2), rep("Asthma", 2), rep("COPD", 2), rep("Autoimmune", 2), rep("Hypertension", 2), 
                 rep("Diabetes", 2), rep("Cancer", 2), rep("Dementia", 2))
rownames(hci) <- c(rep("<50", 2), rep("50-60", 2), rep("60-70 (ref)", 2), rep("70-80", 2), rep(">80", 2), 
                 rep("Female", 2), rep("BMI≥30", 2), rep("Never smoker (ref)", 2), rep("Current smoker", 2), 
                 rep("Past smoker", 2), rep("Asthma", 2), rep("COPD", 2), rep("Autoimmune", 2), rep("Hypertension", 2), 
                 rep("Diabetes", 2), rep("Cancer", 2), rep("Dementia", 2))

for(j in c(1:length(unique(tmp)))){
  for(i in seq(1, length(colnames(M)))){
    M[(2*j-1):(2*j),colnames(M)[i]] <- unlist(data1 %>% filter(covariates == unique(tmp)[j] & sx == colnames(M)[i]) %>% select(beta))[1:2]
    p[(2*j-1):(2*j),colnames(M)[i]] <- unlist(data1 %>% filter(covariates == unique(tmp)[j] & sx == colnames(M)[i]) %>% select(pval))[1:2]
    lci[(2*j-1):(2*j),colnames(M)[i]] <- unlist(data1 %>% filter(covariates == unique(tmp)[j] & sx == colnames(M)[i]) %>% select(LL))[1:2]
    hci[(2*j-1):(2*j),colnames(M)[i]] <- unlist(data1 %>% filter(covariates == unique(tmp)[j] & sx == colnames(M)[i]) %>% select(UL))[1:2]
  }  
}

M[is.na(M)] <- 0
p[is.na(p)] <- 0
M[p > 0.05] <- 0

col2 = colorRampPalette(c('#005BB6', 'white', '#C70000'))
colnames(M) <- c("Cough", "Fatigue", "Shortness of breath", "Headache", "Muscle/Joint pain", "Diarrhea", "Loss of smell/taste",
                 "Loss of appetite", "Sore thrat", "Nausia", "Rhinorrhea", "Muscle weakness", "Confusion", "Ear pain", 
                 "Hoarseness", "Tremor")
png("Fig2.corplot.png", height = 600, width = 1000)
corrplot(M, p.mat=p, is.corr = FALSE, sig.level = 0.05, insig='blank', method = 'square', tl.col = "black", col = col2(100), tl.srt = 45)
dev.off()

#Appendicular lean mass
tmp <- SNP %>% filter(grepl("Appendicular lean mass", Trait))
tmp <- tmp %>% mutate(Z = qnorm(`Lead.Variant.P-value`,lower.tail=FALSE))
tmp <- tmp %>% mutate(Z = ifelse(Beta > 0, Z, -1*Z))
tmp$Z <- -1*tmp$Z
M[1,"rs16837903"] <- tmp$Z[1]
M[1,"rs7307726"] <- tmp$Z[2]

