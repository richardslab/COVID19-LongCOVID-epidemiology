setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

tmp <- readRDS("clinical_bqc19.rds")

tmp <- tmp %>% filter(covid19_test_result == "Positive")
dim(tmp)#3266
table(tmp$hospitalization)#1426
tmp <- tmp %>% mutate_at(.vars=vars(colnames(tmp)[grepl("^sx_", colnames(tmp))]), .funs=funs(ifelse(is.infinite(.), NA, .)))
tmp <- tmp %>% mutate_at(.vars=vars(colnames(tmp)[grepl("^sx_", colnames(tmp))]), .funs=funs(ifelse(.==-1, NA, .)))
summary <- tmp %>% summarize_at(.vars=vars(colnames(tmp)[grepl("^sx_", colnames(tmp)) & !(grepl("sx_date", colnames(tmp)))]), .funs=funs(sum(., na.rm=T))) 

data <- data.frame(t(summary))
colnames(data) <- "Number"
data$variable = rownames(data)

data <- data %>% mutate(group = case_when(grepl("PCS", variable) ~ "persistent (>2mo)",
                                          TRUE ~ "acute"),
                        Sx = gsub("_PCS","",variable))

data <- data %>% arrange(group, Number)

data$Sx <- factor(data$Sx, levels = unique(data$Sx))
data <- data %>% mutate(Sx = case_when(Sx == "sx_any" ~ "Any symptoms",
                                       Sx == "sx_fatigue" ~ "Fatigue",
                                       Sx == "sx_myalgia_anthralgia" ~ "Muscle/Joint pain",
                                       Sx == "sx_cough" ~ "Cough",
                                       Sx == "sx_headache" ~ "Headache",
                                       Sx == "sx_rhinorrhea" ~ "Rhinorrhea",
                                       Sx == "sx_sore_traot" ~ "Sore throat",
                                       Sx == "sx_dyspnea" ~ "Shortness of breath",
                                       Sx == "sx_extremity" ~ "Extremity weakness/numbness",
                                       Sx == "sx_diarrhea" ~ "Diarrhea",
                                       Sx == "sx_appetite" ~ "Loss of Appetite",
                                       Sx == "sx_apha_dysphagia" ~ "Aphasia/Dysphagia",
                                       Sx == "sx_anosmia" ~ "Anosmia",
                                       Sx == "sx_ear_pain" ~ "Ear pain",
                                       Sx == "sx_nausea" ~ "Nausea",
                                       Sx == "sx_confusion" ~ "Confusion",
                                       Sx == "sx_seizure" ~ "Seizure"
))

data$Sx <- factor(data$Sx, levels = unique(data$Sx))

saveRDS(data, file="COVID_acute_PCS.data.rds")



