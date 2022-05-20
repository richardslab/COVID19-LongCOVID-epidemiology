setwd("/scratch/richards/tomoko.nakanishi/09.COVID19/08.LongCOVID_epi/")

tmp1 <- readRDS("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/COVID_acute_PCS.data.rds") %>% mutate(cohort = "CLSA\nN=1,293 (Hospitalized N=39)")
tmp2 <- readRDS("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/COVID_acute_PCS.data.rds") %>% mutate(cohort = "BQC19\nN=3,266 (Hospitalized N=1,426)")


data <- bind_rows(tmp2, tmp1)
data$Sx <- as.character(data$Sx)
data <- data %>% mutate(Sx = case_when(Sx == "Tremor" ~ "Seizure",
                                       Sx == "Anosmia" ~ "Loss of smell/taste",
                                       Sx %in% c("Aphasia/Dysphagia", "Hoarseness") ~ "Aphasia/Dysphagia/Hoarseness",
                                       Sx == "Muscle weakness" ~ "Extremity weakness/numbness",
                                       Sx == "Nausia" ~ "Nausea",
                                       TRUE ~ Sx))

data <- data %>% mutate(group = ifelse(group == "acute", "Any time duration", "Persistent (≥2mo)"))
data$Sx <- factor(data$Sx, levels = unique(data$Sx))

png("/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/Fig1.symptomoloty.png",width=1100, height=500)
ggplot(data=data, aes(x=Sx, y=Number, fill=group)) + 
  geom_bar(stat="identity",position=position_dodge(width = 0)) + coord_flip() +
  facet_wrap(~cohort,strip.position="top",nrow=1,scales = "free") + xlab("Symptoms") +
  scale_fill_manual(values =c("#a6d8f0","#f9b282")) + theme_minimal() +
  guides(fill=guide_legend(title=NULL)) +
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y = element_text(size=15,face="bold"),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        strip.text = element_text(size=15,face="bold"),
        legend.text = element_text(size=15), legend.title=element_blank()) +coord_flip() 
dev.off()

#CLSA 
data %>% filter(variable == "Sx_any")
729/1293
data %>% filter(variable == "Sx_any_PCS")
53/1293
#BQC
data %>% filter(variable == "sx_any")
2990/3266
data %>% filter(variable == "sx_any_PCS")
911/3266


