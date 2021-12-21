setwd("/scratch/richards/tomoko.nakanishi/09.COVID19/08.LongCOVID_epi/")

tmp1 <- readRDS("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/COVID_acute_PCS.data.rds") %>% mutate(cohort = "CLSA\nN=1,293 (Hospitalized N=39)")
tmp2 <- readRDS("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/COVID_acute_PCS.data.rds") %>% mutate(cohort = "BQC19\nN=2,832 (Hospitalized N=1,748)")

data <- bind_rows(tmp2, tmp1)
data <- data %>% mutate(group = ifelse(group == "acute", "all duration", "persistent (≥2mo)"))
data$Sx <- factor(data$Sx, levels = unique(data$Sx))

png("/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/Fig2.symptomoloty.png",width=1100, height=500)
ggplot(data=data, aes(x=Sx, y=Number, fill=group)) + 
  geom_bar(stat="identity",position=position_dodge(width = 0)) + coord_flip() +
  facet_wrap(~cohort,strip.position="top",nrow=1,scales = "free") + xlab("Symptoms") +
  scale_fill_manual(values =c("#a6d8f0","#f9b282")) + theme_minimal() +
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y = element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        strip.text = element_text(size=15,face="bold"),
        legend.text = element_text(size=15)) +coord_flip() 
dev.off()

#CLSA 
data %>% filter(variable == "Sx_any")
2283/2545
734/1293
data %>% filter(variable == "Sx_any_PCS")
635/2545
55/1293


