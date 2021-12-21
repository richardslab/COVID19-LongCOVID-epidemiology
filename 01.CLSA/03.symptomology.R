setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA")
##longCOVID
###any Sx
tmp <- readRDS("CLSA_data.rds")
cases <- tmp %>% filter(C2 == 1)

dim(tmp)#1258
table(tmp$hospital)#39
tmp <- tmp %>% mutate_at(.vars=vars(colnames(tmp)[grepl("^Sx_", colnames(tmp))]), .funs=funs(ifelse(is.infinite(.), NA, .)))
tmp <- tmp %>% mutate_at(.vars=vars(colnames(tmp)[grepl("^Sx_", colnames(tmp))]), .funs=funs(ifelse(.==-1, NA, .)))
summary <- tmp %>% summarize_at(.vars=vars(colnames(tmp)[grepl("^Sx_", colnames(tmp))]), .funs=funs(sum(., na.rm=T))) 
summary <- summary[-1]
data <- data.frame(t(summary))
colnames(data) <- "Number"
data$variable = rownames(data)

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
