setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA")

covid <- read.csv("/home/richards/tomoko.nakanishi/projects/richards/restricted/clsa/covid19_may2021/21CON001_McMasterU_PRaina_COVID-19/21CON001_McMasterU_PRaina_COVID-19_Combined_v1.csv")

covid <- covid %>% mutate(covid19_test = case_when(SYM_TESTPOS_COVID == 1 ~ 1,
                                                   SYM_TESTPOS_1_COVB == 1 ~ 1,
                                                   SYM_TESTPOS_2_COVB == 1 ~ 1,
                                                   SYM_TESTPOS_1_COVW == 1 ~ 1,
                                                   SYM_TESTPOS_2_COVW == 1 ~ 1,
                                                   SYM_TESTPOS_3_COVW == 1 ~ 1,
                                                   SYM_TESTPOS_4_COVW == 1 ~ 1,
                                                   SYM_TESTPOS_1_COVM == 1 ~ 1,
                                                   SYM_TESTPOS_2_COVM == 1 ~ 1,
                                                   SYM_TESTPOS_3_COVM == 1 ~ 1,
                                                   SYM_TESTPOS_COVX == 1 ~ 1,
                                                   SYM_TESTPOS_COVID == 2 ~ 0,
                                                   SYM_TESTPOS_1_COVB == 2 ~ 0,
                                                   SYM_TESTPOS_2_COVB == 2 ~ 0,
                                                   SYM_TESTPOS_1_COVW == 2 ~ 0,
                                                   SYM_TESTPOS_2_COVW == 2 ~ 0,
                                                   SYM_TESTPOS_3_COVW == 2 ~ 0,
                                                   SYM_TESTPOS_4_COVW == 2 ~ 0,
                                                   SYM_TESTPOS_1_COVM == 2 ~ 0,
                                                   SYM_TESTPOS_2_COVM == 2 ~ 0,
                                                   SYM_TESTPOS_3_COVM == 2 ~ 0,
                                                   SYM_TESTPOS_COVX == 2 ~ 0))

covid <- covid %>% mutate(covid19_test_date = case_when((SYM_TESTPOS_COVID == 1 | SYM_NTCONF_COVID == 1) ~ as.Date(start_datetime_COVID),
                                                        (SYM_TESTPOS_1_COVB == 1 | SYM_NTCONF_1_COVB == 1) ~ as.Date(start_datetime_1_COVB, format = "%m/%d/%Y"),
                                                        (SYM_TESTPOS_2_COVB == 1 | SYM_NTCONF_2_COVB == 1) ~ as.Date(start_datetime_2_COVB, format = "%m/%d/%Y"),
                                                        (SYM_TESTPOS_1_COVW == 1 | SYM_NTCONF_1_COVW == 1) ~ as.Date(str_split(start_datetime_1_COVW, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d"),
                                                        (SYM_TESTPOS_2_COVW == 1 | SYM_NTCONF_2_COVW == 1) ~ as.Date(str_split(start_datetime_2_COVW, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d"),
                                                        (SYM_TESTPOS_3_COVW == 1 | SYM_NTCONF_3_COVW == 1) ~ as.Date(str_split(start_datetime_3_COVW, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d"),
                                                        (SYM_TESTPOS_4_COVW == 1 | SYM_NTCONF_4_COVW == 1) ~ as.Date(str_split(start_datetime_4_COVW, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d"),
                                                        (SYM_TESTPOS_1_COVM == 1 | SYM_NTCONF_1_COVM == 1) ~ as.Date(start_datetime_1_COVM, format = "%m/%d/%Y %H:%M"),
                                                        (SYM_TESTPOS_2_COVM == 1 | SYM_NTCONF_2_COVM == 1) ~ as.Date(start_datetime_2_COVM, format = "%m/%d/%Y %H:%M"),
                                                        (SYM_TESTPOS_3_COVM == 1 | SYM_NTCONF_3_COVM == 1) ~ as.Date(start_datetime_3_COVM, format = "%m/%d/%Y %H:%M"),
                                                        (SYM_TESTPOS_COVX == 1 | SYM_NTCONF_COVX == 1 | GEN_INFECT_COVX %in% c(1,2)) ~ as.Date(str_split(start_datetime_COVX, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d")))

covid <- covid %>% mutate(C2 = case_when(covid19_test == 1 ~ 1,
                                         SYM_NTCONF_COVID == 1 ~ 1,
                                         SYM_NTCONF_1_COVB == 1 ~ 1,
                                         SYM_NTCONF_2_COVB == 1 ~ 1,
                                         SYM_NTCONF_1_COVW == 1 ~ 1,
                                         SYM_NTCONF_2_COVW == 1 ~ 1,
                                         SYM_NTCONF_3_COVW == 1 ~ 1,
                                         SYM_NTCONF_4_COVW == 1 ~ 1,
                                         SYM_NTCONF_1_COVM == 1 ~ 1,
                                         SYM_NTCONF_2_COVM == 1 ~ 1,
                                         SYM_NTCONF_3_COVM == 1 ~ 1,
                                         SYM_NTCONF_COVX == 1 ~ 1,
                                         GEN_INFECT_COVX %in% c(1,2) ~ 1,
                                         TRUE ~ 0))

covid <- covid %>% mutate(start_datetime_COVX = as.Date(str_split(start_datetime_COVX, pattern="T", simplify = TRUE)[,1], format="%Y-%m-%d"))
covid <- covid %>% drop_na(start_datetime_COVX)

covid <- covid %>% mutate(moderatesevere_RSNOSE = case_when(SYM_RSNOSE_COVX %in% c(3,4) ~ 1,
                                                           SYM_RSNOSE_COVX %in% c(1,2) ~ 0,
                                                           TRUE  ~ -1),
                          moderatesevere_SINUS = case_when(SYM_SINUS_COVX %in% c(3,4) ~ 1,
                                                            SYM_SINUS_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_EARP = case_when(SYM_EARP_COVX %in% c(3,4) ~ 1,
                                                            SYM_EARP_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_THROAT = case_when(SYM_THROAT_COVX %in% c(3,4) ~ 1,
                                                            SYM_THROAT_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_HOARSE = case_when(SYM_HOARSE_COVX %in% c(3,4) ~ 1,
                                                            SYM_HOARSE_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_DRYCO = case_when(SYM_DRYCO_COVX %in% c(3,4) ~ 1,
                                                            SYM_DRYCO_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_WETCO = case_when(SYM_WETCO_COVX %in% c(3,4) ~ 1,
                                                            SYM_WETCO_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_BREATH = case_when(SYM_BREATH_COVX %in% c(3,4) ~ 1,
                                                            SYM_BREATH_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_HEAD = case_when(SYM_HEAD_COVX %in% c(3,4) ~ 1,
                                                           SYM_HEAD_COVX %in% c(1,2) ~ 0,
                                                           TRUE  ~ -1),
                         moderatesevere_PAIN = case_when(SYM_PAIN_COVX %in% c(3,4) ~ 1,
                                                            SYM_PAIN_COVX %in% c(1,2) ~ 0,
                                                            TRUE  ~ -1),
                          moderatesevere_FATIG = case_when(SYM_FATIG_COVX %in% c(3,4) ~ 1,
                                                             SYM_FATIG_COVX %in% c(1,2) ~ 0,
                                                             TRUE  ~ -1),
                          moderatesevere_APPETIT = case_when(SYM_APPETIT_COVX %in% c(3,4) ~ 1,
                                                             SYM_APPETIT_COVX %in% c(1,2) ~ 0,
                                                             TRUE  ~ -1),
                          moderatesevere_DIARR = case_when(SYM_DIARR_COVX %in% c(3,4) ~ 1,
                                                          SYM_DIARR_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1),
                          moderatesevere_NAUS = case_when(SYM_NAUS_COVX %in% c(3,4) ~ 1,
                                                          SYM_NAUS_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1),
                          moderatesevere_SMELL = case_when(SYM_SMELL_COVX %in% c(3,4) ~ 1,
                                                          SYM_SMELL_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1),
                          moderatesevere_CONFU = case_when(SYM_CONFU_COVX %in% c(3,4) ~ 1,
                                                          SYM_CONFU_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1),
                          moderatesevere_WEAK = case_when(SYM_WEAK_COVX %in% c(3,4) ~ 1,
                                                          SYM_WEAK_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1),
                          moderatesevere_TREM = case_when(SYM_TREM_COVX %in% c(3,4) ~ 1,
                                                          SYM_TREM_COVX %in% c(1,2) ~ 0,
                                                          TRUE  ~ -1))
covid <- covid %>% mutate_at(.vars=vars(c(colnames(covid)[grepl("^moderatesevere_", colnames(covid))])), .funs=funs(ifelse(.==-1, NA, .)))
covid <- covid %>% mutate(Sx_num = rowSums(.[910:927], na.rm=T))

covid <- covid %>% mutate(Sx_any = case_when(Sx_num > 0 ~ 1,
                                             Sx_num == 0 ~ 0))

covid <- covid %>% mutate(moderatesevere_RSNOSE_PCS = case_when(moderatesevere_RSNOSE == 1 & SYM_RNOSEDR_COVX %in% c(4,5) ~ 1,
                                                                moderatesevere_RSNOSE == 1 & SYM_RNOSEDR_COVX %in% c(1,2,3) ~ 0,
                                                                moderatesevere_RSNOSE == 0 ~ 0,
                                                                TRUE ~ -1),
                          moderatesevere_SINUS_PCS = case_when(moderatesevere_SINUS == 1 & SYM_SINUSDR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_SINUS == 1 & SYM_SINUSDR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_SINUS == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_EARP_PCS = case_when(moderatesevere_EARP == 1 & SYM_EARPDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_EARP == 1 & SYM_EARPDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_EARP == 0 ~ 0,
                                                              TRUE ~ -1),
                          moderatesevere_THROAT_PCS = case_when(moderatesevere_THROAT == 1 & SYM_THROATDR_COVX %in% c(4,5) ~ 1,
                                                                moderatesevere_THROAT == 1 & SYM_THROATDR_COVX %in% c(1,2,3) ~ 0,
                                                                moderatesevere_THROAT == 0 ~ 0,
                                                                TRUE ~ -1),
                          moderatesevere_HOARSE_PCS = case_when(moderatesevere_HOARSE == 1 & SYM_HOARSEDR_COVX %in% c(4,5) ~ 1,
                                                                moderatesevere_HOARSE == 1 & SYM_HOARSEDR_COVX %in% c(1,2,3) ~ 0,
                                                                moderatesevere_HOARSE == 0 ~ 0,
                                                                TRUE ~ -1),
                          moderatesevere_DRYCO_PCS = case_when(moderatesevere_DRYCO == 1 & SYM_DRYCODR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_DRYCO == 1 & SYM_DRYCODR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_DRYCO == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_WETCO_PCS = case_when(moderatesevere_WETCO == 1 & SYM_WETCODR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_WETCO == 1 & SYM_WETCODR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_WETCO == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_BREATH_PCS = case_when(moderatesevere_BREATH == 1 & SYM_BREATHDR_COVX %in% c(4,5) ~ 1,
                                                                moderatesevere_BREATH == 1 & SYM_BREATHDR_COVX %in% c(1,2,3) ~ 0,
                                                                moderatesevere_BREATH == 0 ~ 0,
                                                                TRUE ~ -1),
                          moderatesevere_HEAD_PCS = case_when(moderatesevere_HEAD == 1 & SYM_HEADDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_HEAD == 1 & SYM_HEADDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_HEAD == 0 ~ 0,
                                                              TRUE ~ -1),
                          moderatesevere_PAIN_PCS = case_when(moderatesevere_PAIN == 1 & SYM_PAINDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_PAIN == 1 & SYM_PAINDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_PAIN == 0 ~ 0,
                                                              TRUE ~ -1),
                          moderatesevere_FATIG_PCS = case_when(moderatesevere_FATIG == 1 & SYM_FATIGDR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_FATIG == 1 & SYM_FATIGDR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_FATIG == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_APPETIT_PCS = case_when(moderatesevere_APPETIT == 1 & SYM_APPETITDR_COVX %in% c(4,5) ~ 1,
                                                                 moderatesevere_APPETIT == 1 & SYM_APPETITDR_COVX %in% c(1,2,3) ~ 0,
                                                                 moderatesevere_APPETIT == 0 ~ 0,
                                                                 TRUE ~ -1),
                          moderatesevere_DIARR_PCS = case_when(moderatesevere_DIARR == 1 & SYM_DIARRDR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_DIARR == 1 & SYM_DIARRDR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_DIARR == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_NAUS_PCS = case_when(moderatesevere_NAUS == 1 & SYM_NAUSDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_NAUS == 1 & SYM_NAUSDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_NAUS == 0 ~ 0,
                                                              TRUE ~ -1),
                          moderatesevere_SMELL_PCS = case_when(moderatesevere_SMELL == 1 & SYM_SMELLDR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_SMELL == 1 & SYM_SMELLDR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_SMELL == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_CONFU_PCS = case_when(moderatesevere_CONFU == 1 & SYM_CONFUDR_COVX %in% c(4,5) ~ 1,
                                                               moderatesevere_CONFU == 1 & SYM_CONFUDR_COVX %in% c(1,2,3) ~ 0,
                                                               moderatesevere_CONFU == 0 ~ 0,
                                                               TRUE ~ -1),
                          moderatesevere_WEAK_PCS = case_when(moderatesevere_WEAK == 1 & SYM_WEAKDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_WEAK == 1 & SYM_WEAKDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_WEAK == 0 ~ 0,
                                                              TRUE ~ -1),
                          moderatesevere_TREM_PCS = case_when(moderatesevere_TREM == 1 & SYM_TREMDR_COVX %in% c(4,5) ~ 1,
                                                              moderatesevere_TREM == 1 & SYM_TREMDR_COVX %in% c(1,2,3) ~ 0,
                                                              moderatesevere_TREM == 0 ~ 0,
                                                              TRUE ~ -1))

####
covid <- covid %>% mutate(Sx_any_PCS = case_when(moderatesevere_RSNOSE_PCS == 1 ~ 1,
                                                            moderatesevere_SINUS_PCS == 1 ~ 1,
                                                            moderatesevere_EARP_PCS == 1 ~ 1,
                                                            moderatesevere_THROAT_PCS == 1 ~ 1,
                                                            moderatesevere_HOARSE_PCS == 1 ~ 1,
                                                            moderatesevere_DRYCO_PCS == 1 ~ 1,
                                                            moderatesevere_WETCO_PCS == 1 ~ 1,
                                                            moderatesevere_BREATH_PCS == 1 ~ 1,
                                                            moderatesevere_HEAD_PCS == 1 ~ 1,
                                                            moderatesevere_PAIN_PCS == 1 ~ 1,
                                                            moderatesevere_FATIG_PCS == 1 ~ 1,
                                                            moderatesevere_APPETIT_PCS == 1 ~ 1,
                                                            moderatesevere_DIARR_PCS == 1 ~ 1,
                                                            moderatesevere_NAUS_PCS == 1 ~ 1,
                                                            moderatesevere_SMELL_PCS == 1 ~ 1,
                                                            moderatesevere_CONFU_PCS == 1 ~ 1,
                                                            moderatesevere_WEAK_PCS == 1 ~ 1,
                                                            moderatesevere_TREM_PCS == 1 ~ 1,
                                                            moderatesevere_RSNOSE_PCS == 0 ~ 0,
                                                            moderatesevere_SINUS_PCS == 0 ~ 0,
                                                            moderatesevere_EARP_PCS == 0 ~ 0,
                                                            moderatesevere_THROAT_PCS == 0 ~ 0,
                                                            moderatesevere_HOARSE_PCS == 0 ~ 0,
                                                            moderatesevere_DRYCO_PCS == 0 ~ 0,
                                                            moderatesevere_WETCO_PCS == 0 ~ 0,
                                                            moderatesevere_BREATH_PCS == 0 ~ 0,
                                                            moderatesevere_HEAD_PCS == 0 ~ 0,
                                                            moderatesevere_PAIN_PCS == 0 ~ 0,
                                                            moderatesevere_FATIG_PCS == 0 ~ 0,
                                                            moderatesevere_APPETIT_PCS == 0 ~ 0,
                                                            moderatesevere_DIARR_PCS == 0 ~ 0,
                                                            moderatesevere_NAUS_PCS == 0 ~ 0,
                                                            moderatesevere_SMELL_PCS == 0 ~ 0,
                                                            moderatesevere_CONFU_PCS == 0 ~ 0,
                                                            moderatesevere_WEAK_PCS == 0 ~ 0,
                                                            moderatesevere_TREM_PCS == 0 ~ 0,
                                                            TRUE ~ -1
                          ))

covid <- covid %>% mutate(hospital = case_when(SYM_HOSP_COVID == 1 ~ 1,
                                               SYM_HOSP_1_COVB == 1 ~ 1,
                                               SYM_HOSP_2_COVB == 1 ~ 1,
                                               SYM_HOSP_1_COVW == 1 ~ 1,
                                               SYM_HOSP_2_COVW == 1 ~ 1,
                                               SYM_HOSP_3_COVW == 1 ~ 1,
                                               SYM_HOSP_4_COVW == 1 ~ 1,
                                               SYM_HOSP_1_COVM == 1 ~ 1,
                                               SYM_HOSP_2_COVM == 1 ~ 1,
                                               SYM_HOSP_3_COVM == 1 ~ 1,
                                               SYM_CARE_IP_COVX == 1 ~ 1,
                                               TRUE ~ 0
                                               ))
covid <- covid %>% mutate(COVID19hospital = case_when(SYM_CARE_IP_COVX == 1 ~ 1,
                                                      TRUE ~ 0
))

library(tableone)

catVars <- c(colnames(covid)[grepl("^moderatesevere_", colnames(covid))])

tableOne <- CreateTableOne(vars = catVars , strata = c("C2"), data = covid,
                           factorVars = catVars )

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
write.csv(tab3Mat, file = "myTable1.csv")

covid <- covid %>% mutate_at(.vars=vars(colnames(covid)[grepl("^Sx_", colnames(covid))]), 
                             .funs=funs(ifelse(.==-1,NA,.)))

covid <- covid %>% mutate(DEP_CESD10_COVX = ifelse(DEP_CESD10_COVX <0, NA, DEP_CESD10_COVX))
covid <- covid %>% mutate(GAD_DSCORE_COVX = ifelse(GAD_DSCORE_COVX <0, NA, GAD_DSCORE_COVX))

covid <- covid %>% mutate(DEP_CESD10_COVID = ifelse(DEP_CESD10_COVID <0, NA, DEP_CESD10_COVID))
covid <- covid %>% mutate(GAD_DSCORE_COVID = ifelse(GAD_DSCORE_COVID <0, NA, GAD_DSCORE_COVID))

tmp <- covid %>% filter(SYM_NTCONF_COVID != 1 & SYM_TESTPOS_COVID != 1)
t.test(tmp$DEP_CESD10_COVX[tmp$C2 == 1], tmp$DEP_CESD10_COVID[tmp$C2 == 1])
t.test(tmp$GAD_DSCORE_COVX[tmp$C2 == 1], tmp$GAD_DSCORE_COVID[tmp$C2 == 1])

covid <- covid %>% mutate_at(.vars=vars(colnames(covid)[grepl("^CCC_LTC_", colnames(covid))]), 
                             .funs=funs(ifelse(.==1,1,0)))

baseline <- read.csv("/home/richards/tomoko.nakanishi/projects/richards/restricted/clsa/covid19_may2021/21CON001_McMasterU_PRaina_Baseline/21CON001_McMasterU_PRaina_CoP5_2_Baseline.csv")

baseline <- baseline %>% rename(genomeID = ADM_GWAS3_COM)
baseline <- baseline %>% mutate(fatigue_at_baseline = case_when(K10_TIRED_MCQ %in% c(1:2) ~ 1,
                                                                K10_TIRED_MCQ %in% c(3:5) ~ 0))

baseline <- baseline %>% mutate(SOB_at_baseline = case_when(CAO_SOBFLAT_COM == 1 ~ 1,
                                                            TRUE ~ 0))

baseline <- baseline %>% mutate(cough_at_baseline = case_when(CAO_COFPY_COM == 1 ~ 1,
                                                            TRUE ~ 0))

baseline <- baseline %>% mutate(CESD10_at_baseline = ifelse(DEP_CESD10_COM <0, NA, DEP_CESD10_COM))
baseline <- baseline %>% mutate(BMI_at_baseline = ifelse(HWT_DBMI_COM >999, NA, HWT_DBMI_COM))
#ICQ_SMOKE_COM

baseline <- baseline %>% mutate_at(.vars=vars(c(CCC_ASTHM_COM, CCC_COPD_COM, DIA_DIAB_COM,
                                                CCC_HBP_COM, CCC_HEART_COM, CCC_CANC_COM,
                                                CCC_KIDN_COM, CCC_RA_COM)),
                                   .funs=funs(ifelse(.==1, 1, 0)))

covid <- covid %>% merge(baseline, by="entity_id", all.x=T)

followup <- read.csv("/home/richards/tomoko.nakanishi/projects/richards/restricted/clsa/covid19_may2021/21CON001_McMasterU_PRaina_FU1/21CON001_McMasterU_PRaina_CoP3_FU1.csv")

followup <- followup %>% mutate(fatigue_at_followup = case_when(K10_TIRED_COF1 %in% c(1:2) ~ 1,
                                                                K10_TIRED_COF1 %in% c(3:5) ~ 0))

followup <- followup %>% mutate(SOB_at_followup = case_when(CAO_SOBFLAT_COF1 == 1 ~ 1,
                                                            TRUE ~ 0))

followup <- followup %>% mutate(cough_at_followup = case_when(CAO_COFPY_COF1 == 1 ~ 1,
                                                              TRUE ~ 0))

followup <- followup %>% mutate(CESD10_at_followup = ifelse(DEP_CESD10_COF1 <0, NA, DEP_CESD10_COF1))
followup <- followup %>% mutate(BMI_at_followup = ifelse(HWT_DBMI_COF1 >999, NA, HWT_DBMI_COF1))

#ICQ_SMOKE_COM

followup <- followup %>% mutate_at(.vars=vars(c(CCC_ASTHM_COF1, CCC_COPD_COF1, DIA_DIAB_COF1,
                                                CCC_HBP_COF1, CCC_HEART_COF1, CCC_CANC_COF1,
                                                CCC_KIDN_COF1, CCC_RA_COF1)),
                                   .funs=funs(ifelse(.==1, 1, 0)))

covid <- covid %>% merge(followup, by="entity_id", all.x=T)

covid <- covid %>% rename(age = AGE_NMBR_COVID)

covid <- covid %>% mutate(sex = ifelse(SEX_CLSA == "M", "0M", "F"))

covid <- covid %>% mutate(smoking = case_when(SMK_CURRCG_COVID %in% c(1,2) ~ "current",
                                              SMK_CURRCG_COVID == 3 & (ICQ_SMOKE_COM %in% c(1,3) | ICQ_SMOKE_COF1 %in% c(1,3)) ~ "past",
                                              (ICQ_SMOKE_COM == 2 | ICQ_SMOKE_COF1 == 2 ) ~ "0never"),
                          com_asthma = case_when(CCC_LTC_ASTHM_COVID == 1 ~ 1,
                                                 CCC_ASTHM_COM == 1 ~ 1,
                                                 CCC_ASTHM_COF1 == 1 ~ 1,
                                                 TRUE ~ 0),
                          com_copd = case_when(CCC_LTC_COPD_COVID == 1 ~ 1,
                                               CCC_COPD_COM == 1 ~ 1,
                                               CCC_COPD_COF1 == 1 ~ 1,
                                               TRUE ~ 0),
                          com_hypertension = case_when(CCC_LTC_HBP_COVID == 1 ~ 1,
                                                       CCC_HBP_COM == 1 ~ 1,
                                                       CCC_HBP_COF1 == 1 ~ 1,
                                               TRUE ~ 0),
                          com_diabetes = case_when(CCC_LTC_DIA_COVID == 1 ~ 1,
                                                   DIA_DIAB_COM == 1 ~ 1,
                                                   DIA_DIAB_COF1 == 1 ~ 1,
                                                   TRUE ~ 0),
                          com_autoimmune = case_when(CCC_LTC_AUTOIMD_COVID == 1 ~ 1,
                                                     CCC_RA_COM == 1 ~ 1,
                                                     CCC_RA_COF1 == 1 ~ 1,
                                                     TRUE ~ 0),
                          com_cancer = case_when(CCC_LTC_CANC_COVID == 1 ~ 1,
                                                 CCC_CANC_COM == 1 ~ 1,
                                                 CCC_CANC_COF1 == 1 ~ 1,
                                                 TRUE ~ 0),
                          com_dementia = case_when(CCC_LTC_CANC_COVID == 1 ~ 1,
                                                   CCC_ALZH_COM == 1 ~ 1,
                                                   CCC_ALZH_COF1 == 1 ~ 1,
                                                   TRUE ~ 0)
                          )

covid <- covid %>% mutate(ethnicity = case_when(SDC_CULT_WH_COM == 1 ~ "0White",
                                                SDC_CULT_ZH_COM == 1 ~ "EastAsia",
                                                SDC_CULT_SA_COM == 1 ~ "SouthAsia",
                                                SDC_CULT_BL_COM == 1 ~ "Black",
                                                SDC_CULT_FP_COM == 1 ~ "SouthAsia",
                                                SDC_CULT_LA_COM == 1 ~ "LatinAmerica",
                                                SDC_CULT_SE_COM == 1 ~ "SouthAsia",
                                                SDC_CULT_AR_COM == 1 ~ "Others",
                                                SDC_CULT_WA_COM == 1 ~ "Others",
                                                SDC_CULT_JA_COM == 1 ~ "EastAsia",
                                                SDC_CULT_KO_COM == 1 ~ "EastAsia",
                                                TRUE ~ "Others"))
covid <- covid %>% mutate(ethnicity1 = case_when(SDC_CULT_WH_COM == 1 ~ "0White",
                                                TRUE ~ "Non-white"))

covid <- covid %>% mutate(obesity = case_when(BMI_at_followup >= 30 ~ 1,
                                              BMI_at_followup < 30 ~ 0,
                                              BMI_at_baseline >= 30 ~ 1,
                                              BMI_at_baseline < 30 ~ 0
                                              ))

covid <- covid %>% mutate(age_range = case_when(age >= 60 & age < 70 ~ "060-70",
                                                age < 60 ~ "<60",
                                               age >= 70 & age < 80 ~ "70-80",
                                               age >= 80 ~ ">80"
                                               ))

covid <- covid %>% mutate(income_at_baseline = case_when(INC_TOT_COM %in% c(1,2) ~ "<50000",
                                                         INC_TOT_COM %in% c(3) ~ "050000-100000",
                                                         INC_TOT_COM %in% c(4) ~ "100000-150000",
                                                         INC_TOT_COM %in% c(5) ~ ">150000",
                                                         ))

saveRDS(covid, file="coviddata.20211007.rds")

