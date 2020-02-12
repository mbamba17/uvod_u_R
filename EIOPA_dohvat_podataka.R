library(tidyverse)
library(lubridate)
library(readxl)
library(ecb)
library(rio)
library(scales)

# Kopiranje dataframea u excel

skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# 0. Definiranje regija ####
reg = data.frame(ctry=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE","IT","LV","LI","LT","LU","MT","NL","NO","PL","PT","RO","SK","SI","ES","SE","UK"),country=c("AUSTRIA","BELGIUM","BULGARIA","CROATIA","CYPRUS","CZECH REPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HUNGARY","ICELAND","IRELAND","ITALY","LATVIA","LIECHTENSTEIN","LITHUANIA","LUXEMBOURG","MALTA","NETHERLANDS","NORWAY","POLAND","PORTUGAL","ROMANIA","SLOVAKIA","SLOVENIA","SPAIN","SWEDEN","UNITED KINGDOM"),regija=c("Other EU","Other EU","CEE","HR","Other EU","CEE","Other EU","CEE","Other EU","Other EU","Other EU","Other EU","CEE","Other EEA","Other EU","Other EU","CEE","Other EEA","CEE","Other EU","Other EU","Other EU","Other EEA","CEE","Other EU","CEE","CEE","CEE","Other EU","Other EU","Other EU"))


# 1. Mirovinci ####

# Link na agregatni excel: https://eiopa.europa.eu/Publications/Surveys/EIOPA_BoS-18-508_EEA_pension%20statistics%202004-2017_update_12-12-2018.xls


#### 2. Insurance ####

# Link na stranicu sa statistikom od osiguranja: https://eiopa.europa.eu/Pages/Financial-stability-and-crisis-prevention/Insurance-Statistics.aspx

# 2.0. Dohvat tečaja sa EUR/HRK (s ECB-a) - to nam treba da iznose u EUR preračunamo u HRK
pom0 <- get_data("EXR.Q.HRK.EUR.SP00.E") %>% select(obstime,tecaj_eur=obsvalue) %>% mutate(datum = make_date(ifelse(substr(obstime,7,7)=="4",as.numeric(substr(obstime,1,4))+1,as.numeric(substr(obstime,1,4))), ifelse(substr(obstime,7,7)=="4",1,as.numeric(substr(obstime,7,7))*3+1), 1)-1) %>% select(-obstime)

# 2.1. Balance sheet - u mil. EUR

pom1 <- rio::import(file="https://register.eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Balance_Sheet.csv") %>% select(`Reporting country`,`Reference period`,`Item name`,`Item code`,`Value`)
colnames(pom1) <- c("country","period","razina","tag","iznos_eur")
pom1 <- pom1 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="bilanca") %>% select(-period)

# 2.2. Own Funds - u mil. EUR

pom2 <- rio::import(file="https://register.eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Own_Funds.csv") %>% select(`Reporting country`,`Reference period`,`Item name`,`Item code`,`Value`)
colnames(pom2) <- c("country","period","razina","tag","iznos_eur")
pom2 <- pom2 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="kapital") %>% select(-period)

# 2.3. Premiums, claims and expenses - u mil. EUR
pom3 <- rio::import(file="https://register.eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Premiums_Claims_Expenses.csv") %>% select(`Reporting country`,`Reference period`,`Item`,`Item code`,`Value`)
colnames(pom3) <- c("country","period","razina","tag","iznos_eur")
pom3 <- pom3 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="premije") %>% select(-period)

# 2.4. Sklapanje tablica u 1 tablicu: osiguranja
osiguranja <- rbind(pom1,pom2,pom3) %>% left_join(pom0,by="datum") %>% left_join(reg,by="country") %>% mutate(iznos=iznos_eur*tecaj_eur)
rm(pom1,pom2,pom3)
save(osiguranja,file="osiguranja.Rda")

# 2.5. Asset exposures - u mil. EUR
izlozenost_os <- rio::import(file="https://register.eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Exposures.xlsx",which="Raw data (CSV)") %>% select(`Reference period`,`NCA_ISO_CODE`,`Reporting country`,`Undertaking type`,`CIC main category`,`CIC sub-category`,`Portfolio type`,`Location of investment`,`Real estate exposure`,`Type of real estate exposure`,`Value (euro millions)`)
colnames(izlozenost_os) <- c("period","ctry","country","vrsta_osiguranja","razina1","razina2","portfelj","drzava_izlozenosti","nekretnina_izlozenost","vrsta_nekretnine","iznos_eur")
izlozenost_os <- izlozenost_os %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% select(-period,-ctry) %>% left_join(pom0,by="datum") %>% left_join(reg,by="country") %>% mutate(iznos=iznos_eur*tecaj_eur)
save(izlozenost_os,file="izlozenost_osiguranja.Rda")
rm(pom0)

# Slika 29. Struktura izloženosti osiguranja po instrumentu ####
pom <- izlozenost_os %>% group_by(datum,regija,razina1) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% mutate(razina1=ifelse(razina1=="CIC 1 Government bonds","Državne obveznice",ifelse(razina1=="CIC 2 Corporate bonds","Korporativne obveznice",ifelse(razina1=="CIC 3 Equity","Dionice",ifelse(razina1=="CIC 9 Property","Nekretnine",ifelse(razina1=="CIC 8 Mortgages and loans","Krediti",ifelse(razina1=="CIC 4 Collective Investment Undertakings","UCITS fondovi","Ostalo")) ))))) %>% group_by(datum,regija,razina1) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% ungroup() %>% filter(regija!="Other EEA") %>% mutate(regija=ifelse(regija=="CEE","Zemlje SIE",ifelse(regija=="Other EU","Ostale zemlje EU-a","HR")))  %>% mutate(datum=floor_date(datum,unit = "month")) %>% arrange(razina1)
pom$razina1 <- factor(pom$razina1,levels=c("Dionice","Državne obveznice","Korporativne obveznice","Krediti","Nekretnine","UCITS fondovi","Ostalo"))
pom$datum=as.Date(pom$datum)
ggplot(pom,aes(x=datum,y=iznos,fill=razina1)) + geom_col(position="fill") + facet_wrap(~regija) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="") + scale_x_date(breaks = "6 months",labels = date_format("%m/%y.")) + scale_fill_manual(values=c("khaki","tomato2","turquoise3","palegreen","firebrick","skyblue2","cornsilk3")) + scale_y_continuous(labels = scales::percent)

# Slika 30. Struktura izloženosti po rezidentnosti ####
pom <- izlozenost_os %>% mutate(rezidentnost=ifelse(country==drzava_izlozenosti,"Domaća","Strana")) %>% group_by(datum,regija,rezidentnost) %>% summarise(iznos=sum(iznos,na.rm = T)) %>% ungroup() %>% filter(regija!="Other EEA") %>% mutate(regija=ifelse(regija=="CEE","Zemlje SIE",ifelse(regija=="Other EU","Ostale zemlje EU-a","HR"))) %>% mutate(datum=floor_date(datum,unit = "month"))
pom$datum=as.Date(pom$datum)
ggplot(pom,aes(x=datum,y=iznos,fill=rezidentnost)) + geom_col(position="fill") + facet_wrap(~regija) + theme(legend.position = "top",legend.title = element_blank()) + labs(x="",y="") + scale_x_date(breaks = "6 months",labels = date_format("%m/%y.")) + scale_y_continuous(labels = scales::percent)

# Slika 32. Troškovna efikasnost osiguranja ####

# prosjek 2016-2019
pom <- osiguranja %>% group_by(datum,ctry,regija) %>% filter(tablica=="premije" & tag %in% c("Z0002")) %>% summarise(iznos=sum(iznos_eur,na.rm=T)) %>% group_by(ctry,regija) %>% summarise(iznos=mean(iznos,na.rm=T)*100) %>% filter(regija!="Ostale EEA zemlje" & ctry %in% reg$ctry) %>% mutate(regija=ifelse(regija=="CEE","Zemlje SIE",ifelse(regija=="Other EU","Ostale zemlje EU-a","HR")))
ggplot(pom,aes(x=reorder(ctry,-iznos),y=iznos)) + geom_col(aes(fill=regija)) + geom_hline(aes(yintercept=mean(pom$iznos))) + geom_text(aes(25,mean(pom$iznos),label = round(mean(pom$iznos),digits = 2), vjust = -1)) + labs(x="",y="Kvota troškova (2017.-6/2019.,%)") + theme(legend.position = "top",legend.title = element_blank())


