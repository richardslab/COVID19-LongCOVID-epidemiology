setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

library(data.table)
infile <- "~/scratch/09.COVID19/data/01.UKBB/ukb27449_20688_fetch_data.tab.gz"
t <- fread(infile, sep="\t")
d <- data.frame(t$f.eid, t$f.31.0.0, t$f.54.0.0, t$f.53.0.0, t$f.21003.0.0, t$f.22000.0.0)
d$GTARRAY <- NA
d$GTARRAY[d$t.f.22000.0.0 < 0] <- "UKBiLEVE"
d$GTARRAY[d$t.f.22000.0.0 > 0] <- "UKAxiom"
d$GTARRAY <- ifelse(d$GTARRAY == "UKAxiom", 1, 0)
d1 <- d[-6]
colnames(d1)[1:5] <- c("ID", "SEX", "CENTRE","1stDATE","AGE")

#withdrawal
w <- fread("/scratch/richards/tomoko.nakanishi/09.COVID19/data/01.UKBB/w27449_20210809.csv")
colnames(w) <- "ID"
d1$withdrawal <- 0
d1$withdrawal[d1$ID %in% w$ID] <- 1
d2 <- d1[d1$withdrawal == 0,]#486123
d3 <- d2 %>% select(c("ID", "SEX", "CENTRE", "1stDATE", "AGE", "GTARRAY"))


t <- d3
t$PCS <- 0

#longCOVID: PCS 
gp <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_tpp_gp_clinical_20210520.txt.gz", sep="\t")
max(as.Date(gp$event_dt[as.Date(gp$event_dt, format="%d/%m/%Y") < as.Date("2021-12-01")], format="%d/%m/%Y"))#"2021-04-23"
#Y2b87	Post-COVID-19 syndrome
#Y2b88	Signposting to Your COVID Recovery
#Y2b89	Referral to post-COVID assessment clinic
#Y2b8a	Referral to Your COVID Recovery rehabilitation platform
id1 <- gp %>% filter(code_type == 1 & code %in% c("Y2b87", "Y2b89", "Y2b88","Y2b8a"))
t <- t %>% mutate(PCS = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PCS))

emis <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_emis_gp_clinical_20210810.txt.gz", sep="\t")
max(as.Date(emis$event_dt[as.Date(emis$event_dt, format="%d/%m/%Y") < as.Date("2021-12-01")], format="%d/%m/%Y"))#"2021-07-25"
#Data-field 7689 #local EMIS code
#data <- data %>% mutate(case_hesin = ifelse(ID %in% unique(id1), 1, case_hesin))
#code_type == 2 > SNOMED CT from https://confluence.ihtsdotools.org/display/snomed/SNOMED+CT+COVID-19+Related+Content
snomedct <- 
  c("1325161000000102", "1325181000000106", "1325021000000106", "1325031000000108", "1325041000000104",
    "1325051000000101", "1325061000000103", "1325071000000105", "1325081000000107", "1325091000000109",
    "1325101000000101", "1325121000000105", "1325131000000107", "1325141000000103", "1325151000000100")
id1 <- emis %>% filter(code_type == 2 & code %in% snomedct)
t <- t %>% mutate(PCS = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PCS))
sum(t$PCS)

##PVF
t$PVF <- 0

#G933
infile <- "/home/richards/tomoko.nakanishi/scratch/UKBBspirometry/1.0.phenotype/ukb27449_20688_fetch_disease.tab.gz"
d <- fread(infile, sep="\t", na.strings = c("NA"))
colnames(d)[1] <- "ID"

##Non-cancer Illness
#1482	chronic fatigue syndrome
f <- names(d)[names(d) %like% "f.20002."]
d1 <- d[, f.20002.x.x := do.call(paste0,list(c(.SD, "NA"), sep=",", collapse="")), by=ID, .SDcols=f]
f.20002 <- d1[,c("ID", "f.20002.x.x")]
code <- merge(t, f.20002, by="ID")
t$PVF[grepl("1482", code$f.20002.x.x)] <- 1#2154

# Diagnoses - main ICD10
f <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/hesin_diag_20210620.txt.gz")
date <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/hesin_20210620.txt.gz")
#G933	G93.3 Postviral fatigue syndrome
f1 <- f[grepl("G933", f$diag_icd10),]
f1 <- f1 %>% merge(date, by=c("eid", "ins_index"))
f1 <- f1 %>% filter(as.Date(epistart, format="%d/%m/%Y") < as.Date("2020/01/31") | as.Date(elecdate, format="%d/%m/%Y") < as.Date("2020/01/31"))
list <- unique(f1$eid)
for(i in seq(1,length(list))){
  t$PVF[t$ID == list[i]] <- 1
}

f1 <- f[grepl("7807", f$diag_icd9),]
f1 <- f1 %>% merge(date, by=c("eid", "ins_index"))
f1 <- f1 %>% filter(as.Date(admidate, format="%d/%m/%Y") < as.Date("2020/01/31"))
list <- unique(f1$eid)
for(i in seq(1,length(list))){
  t$PVF[t$ID == list[i]] <- 1
}
sum(t$PVF)

##TPP
#CTV3
#Xa01F	Chronic fatigue syndrome
#XaPeC	Activity management for chronic fatigue syndrome
#XaPom	Mild chronic fatigue syndrome
#XaPon	Moderate chronic fatigue syndrome
#XaPoo	Severe chronic fatigue syndrome
#XaR7C	Referral to chronic fatigue syndrome specialist team
#XaRAz	Referral for chronic fatigue syndrome activity management

id1 <- gp %>% filter(code_type == 0 & code %in% c("Xa01F", "XaPeC", "XaPom","XaPon","XaPoo","XaR7C","XaRAz"))
id1 <- id1 %>% filter(as.Date(event_dt, format="%d/%m/%Y") < as.Date("2020/01/31"))

t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PVF))
#1684.	(Malaise/lethargy) or (C/O: [debility - malaise] or [overwork] or [postviral syndrome])
#E2B0.	Postviral depression
#R0074	[D]Postviral (asthenic) syndrome
#X0081	Postviral excessive daytime sleepiness
#XE0uP	(Malaise: [/lethargy] or [debility]) or (overwork) or (postviral syndrome)
#XM0Cg	C/O - postviral syndrome

id1 <- gp %>% filter(code_type == 0 & code %in% c("1684.", "E2B0.", "R0074","X0081","XE0uP","XM0Cg"))
id1 <- id1 %>% filter(as.Date(event_dt, format="%d/%m/%Y") < as.Date("2020/01/31"))
t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PVF))

#TPP
#Y8580	Postviral syndr.(asthenic) [D]
id1 <- gp %>% filter(code_type == 1 & code %in% c("Y8580"))
id1 <- id1 %>% filter(as.Date(event_dt, format="%d/%m/%Y") < as.Date("2020/01/31"))
t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PVF))

#fatigue
#Y7549	Malaise [D]
#Y7550	Malaise and fatigue [D]
#Y7551	Malaise and fatigue NOS [D]
#Y0e5e	Fatigue management advice
#Y4050	Fatigues
#Y6753	Fatigue [D]
#Y11f5	PHQ-9 Question 4 Score: Feeling tired or having little energy

#id1 <- gp %>% filter(code_type == 1 & code %in% c("Y7549", "Y7550", "Y7551", "Y0e5e", "Y4050", "Y6753", "Y11f5"))
#t <- t %>% mutate(fatigue = case_when(ID %in% id1$eid ~ 1,
#                                      TRUE ~ fatigue))

emis <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/covid19_emis_gp_clinical_20210810.txt.gz", sep="\t")

#EMISNQRE561	Reasn fr referrl:Chronic Fatigue Syndrome/Myalgic Encephalopathy
#OLTR-AAA_GP_CFS	Test Request : GP Chronic Fatigue Syndrome
#OLTR-CFPV	Test Request : Chronic Fatigue/Post Viral Syndome
#OLTR-CFSS	Test Request : Chronic Fatigue /ME  Referral Screen
#id1 <- emis %>% filter(code_type == 3 & code %in% c("EMISNQRE561", "OLTR-AAA_GP_CFS", "OLTR-CFPV", "OLTR-CFSS"))

#t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
#                                  TRUE ~ PVF))

#CFS
id1 <- emis %>% filter(code_type == 2 & code %in% c("52702003", "377181000000104", "377161000000108", "377171000000101",
                                                    "372601000000101", "520511000000101", "522991000000100", "473341005",
                                                    "377181000000104"))
id1 <- id1 %>% filter(as.Date(event_dt, format="%d/%m/%Y") < as.Date("2020/01/31"))
t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PVF))

#PVF 
#51771007
#266226000
#192079006
#445241004
#230491004
id1 <- emis %>% filter(code_type == 2 & code %in% c("51771007", "266226000", "192079006", "445241004",
                                                    "230491004"))
dim(id1)
id1 <- id1 %>% filter(as.Date(event_dt, format="%d/%m/%Y") < as.Date("2020/01/31"))
dim(id1)
t <- t %>% mutate(PVF = case_when(ID %in% id1$eid ~ 1,
                                  TRUE ~ PVF))

saveRDS(t, "LongCOVID.rds")
