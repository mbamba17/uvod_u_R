library(tidyverse)
library(lubridate)
library(readxl)
library(ecb)

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
download.file("https://eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Balance_Sheet.csv", "bilanca.csv", method = "auto", quiet = FALSE, mode = "w",cacheOK = TRUE, extra = getOption("download.file.extra"))
pom1 <- read.csv(file="bilanca.csv", header=TRUE, sep=",") %>% select(`Reporting.country`,`Reference.period`,`Item.name`,`Item.code`,`Value`)
colnames(pom1) <- c("country","period","razina","tag","iznos_eur")
unlink("bilanca.csv")
pom1 <- pom1 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="bilanca") %>% select(-period)

# 2.2. Own Funds - u mil. EUR
download.file("https://eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Own_Funds.csv", "kapital.csv", method = "auto", quiet = FALSE, mode = "w",cacheOK = TRUE, extra = getOption("download.file.extra"))
pom2 <- read.csv(file="kapital.csv", header=TRUE, sep=",") %>% select(`Reporting.country`,`Reference.period`,`Item.name`,`Item.code`,`Value`)
colnames(pom2) <- c("country","period","razina","tag","iznos_eur")
unlink("kapital.csv")
pom2 <- pom2 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="kapital") %>% select(-period)

# 2.3. Premiums, claims and expenses - u mil. EUR
download.file("https://eiopa.europa.eu/Publications/Insurance%20Statistics/SQ_Premiums_Claims_Expenses.csv", "premije.csv", method = "auto", quiet = FALSE, mode = "w",cacheOK = TRUE, extra = getOption("download.file.extra"))
pom3 <- read.csv(file="premije.csv", header=TRUE, sep=",") %>% select(`Reporting.country`,`Reference.period`,`Item`,`Item.code`,`Value`)
colnames(pom3) <- c("country","period","razina","tag","iznos_eur")
unlink("premije.csv")
pom3 <- pom3 %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% mutate(tablica="premije") %>% select(-period)

# 2.4. Sklapanje tablica u 1 tablicu: osiguranja
osiguranja <- rbind(pom1,pom2,pom3) %>% left_join(pom0,by="datum") %>% left_join(reg,by="country") %>% mutate(iznos=iznos_eur*tecaj_eur)
rm(pom1,pom2,pom3)
save(osiguranja,file="osiguranja.Rda")

# 2.5. Asset exposures - u mil. EUR
# Iz nekog razloga direktno skidanje ovog excela ne funkcionira, tako da ga trebamo ručno skinuti i spremiti na: D:\mbamba\DATA\International\EIOPA\Primjeri excela pod imenom SQ_Exposures.xlsx
izlozenost_os <- read_excel(path = "D:/mbamba/DATA/International/EIOPA/Primjeri excela/SQ_Exposures.xlsx",sheet="Raw data (CSV)") %>% select(`Reference period`,`NCA_ISO_CODE`,`Reporting country`,`Undertaking type`,`CIC main category`,`CIC sub-category`,`Portfolio type`,`Location of investment`,`Real estate exposure`,`Type of real estate exposure`,`Value (euro millions)`)
colnames(izlozenost_os) <- c("period","ctry","country","vrsta_osiguranja","razina1","razina2","portfelj","drzava_izlozenosti","nekretnina_izlozenost","vrsta_nekretnine","iznos_eur")
izlozenost_os <- izlozenost_os %>% mutate(datum = make_date(ifelse(substr(period,7,7)=="4",as.numeric(substr(period,1,4))+1,as.numeric(substr(period,1,4))), ifelse(substr(period,7,7)=="4",1,as.numeric(substr(period,7,7))*3+1), 1)-1) %>% select(-period,-ctry) %>% left_join(pom0,by="datum") %>% left_join(reg,by="country") %>% mutate(iznos=iznos_eur*tecaj_eur)
save(izlozenost_os,file="izlozenost_osiguranja.Rda")
rm(reg,pom0)

# 3. Kopiranje u excel ####
load("D:/mbamba/DATA/International/EIOPA/osiguranja.Rda")
load("D:/mbamba/DATA/International/EIOPA/izlozenost_osiguranja.Rda")
write.csv(x = osiguranja,file = "osiguranja.csv")
write.csv(x = izlozenost_os,file = "izlozenost_os.csv")

# 4. Izloženost domaći vs. strani ####

# prekogranična izloženost
pom1 <- izlozenost_os %>% group_by(datum,ctry) %>% filter(country!=drzava_izlozenosti) %>% summarise(prekogranicni=sum(iznos,na.rm=T))
pom2 <- izlozenost_os %>% group_by(datum,ctry) %>% summarise(ukupno_izlozenost=sum(iznos,na.rm=T))
pom <- left_join(pom1,pom2,by=c("ctry","datum")) %>% left_join(reg,by="ctry") %>% group_by(regija,datum) %>% summarise(prekogranicni=sum(prekogranicni,na.rm=T),ukupno_izlozenost=sum(ukupno_izlozenost,na.rm=T)) %>% mutate(udio_prekogranicni=prekogranicni/ukupno_izlozenost*100) %>% select(datum,regija,udio_prekogranicni) %>% spread(regija,udio_prekogranicni)
skopiraj(pom)
# ggplot(pom,aes(x=datum,y=udio_prekogranicni)) + geom_line() + facet_wrap(~regija, scales="free")


