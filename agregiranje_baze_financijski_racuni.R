library(XLConnect)
library(tidyverse)
library(lubridate)
library(xlsx)
library(openxlsx)
rm(list=ls())

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

options(java.parameters = "-Xmx8g") ## memory set to 4 GB

# Daj popis svih excela u folderu ####
financijski_racuni = data.frame(protustrana_puni_naziv=array(),vrsta_imovine=array(),iznos=array(),razina=array(),kvartal=array(),vrsta_iznosa=array(),sektor=array())[-1,]

putanja <- "D:/mbamba/DATA/Financijski_racuni/Fin_rac_input"
direktoriji <- list.dirs(putanja, full.names = TRUE,recursive = FALSE)
direktoriji
for (d in 1:length(direktoriji)){
  temp_dir = direktoriji[d]
  fileovi <- list.files(path=temp_dir,pattern = "\\.xlsx$")
  fileovi = fileovi[fileovi!="DELTA.xlsx"]
  for (f in 1:length(fileovi)){
    temp_file = paste(direktoriji[d],fileovi[f],sep="/")
    sheetovi = getSheetNames(temp_file)
    sheetovi = sheetovi[sheetovi!="TOTAL 2"]
    for (s in 1:length(sheetovi)){
      # Assets
      podaci <- read.xlsx(temp_file, sheet = sheetovi[s], startRow = 3, colNames = TRUE,rows = 3:22,cols = 2:28)
      podaci = gather(podaci,"vrsta_imovine","iznos",2:ncol(podaci))
      colnames(podaci)[1]<-"protustrana_puni_naziv"
      podaci = data.frame(podaci,razina=rep("Assets",nrow(podaci)),kvartal=rep(substr(temp_dir,nchar(temp_dir)-5,nchar(temp_dir)),nrow(podaci)),vrsta_iznosa=rep(substr(fileovi[f],1,nchar(fileovi[f])-5)),sektor=rep(sheetovi[s],nrow(podaci)))
      financijski_racuni = rbind(financijski_racuni,podaci)
      # Liabilities
      podaci <- read.xlsx(temp_file, sheet = sheetovi[s], startRow = 26, colNames = TRUE,rows = 26:45,cols = 2:28)
      podaci = gather(podaci,"vrsta_imovine","iznos",2:ncol(podaci))
      colnames(podaci)[1]<-"protustrana_puni_naziv"
      podaci = data.frame(podaci,razina=rep("Liabilities",nrow(podaci)),kvartal=rep(substr(temp_dir,nchar(temp_dir)-5,nchar(temp_dir)),nrow(podaci)),vrsta_iznosa=rep(substr(fileovi[f],1,nchar(fileovi[f])-5)),sektor=rep(sheetovi[s],nrow(podaci)))
      financijski_racuni = rbind(financijski_racuni,podaci)
      
    }
  }
}
rm(d,direktoriji,f,fileovi,putanja,s,sheetovi,temp_dir,temp_file)

# kreiranje naziva sektora i protustrana da budu konzistentni
kratice_sektor <- tibble(sektor_puni_naziv=c("Non financial corporations","Non operating banks","Central bank","Deposit taking corporations","Money markets funds","Non-MMF Investment funds","Other financial corporations","Financial auxiliaries","Capitve financial institutions","Insurance corporations","Pension funds","Central government","Local government","Social security funds","Households","Non profit institutions serving households","Rest of the world","Not allocated","Total") , sektor = c("NFC","NoB","CB","DTC","MMF","IVF","OFI","FA","CFI","IC","PF","CG","LG","SSF","HH","NPISH", "RoW","NA","TOTAL") , sektor_mb=c("Poduzeća","Ostale FI","Centralna banka","Kreditne institucije","Ostale FI","Investicijski fondovi","Ostale FI","Ostale FI","Ostale FI","Osiguranja","Mirovinski fondovi","Država","Država","Država","Stanovništvo","Stanovništvo","Inozemstvo","NA","Total"))
kratice_protustrana <- tibble(protustrana_puni_naziv=c("Non financial corporations","Non operating banks","Central bank","Deposit taking corporations","Money markets funds","Non-MMF Investment funds","Other financial corporations","Financial auxiliaries","Capitve financial institutions","Insurance corporations","Pension funds","Central government","Local government","Social security funds","Households","Non profit institutions serving households","Rest of the world","Not allocated","Total") , protustrana = c("NFC","NoB","CB","DTC","MMF","IVF","OFI","FA","CFI","IC","PF","CG","LG","SSF","HH","NPISH", "RoW","NA","TOTAL") , protustrana_mb=c("Poduzeća","Ostale FI","Centralna banka","Kreditne institucije","Ostale FI","Investicijski fondovi","Ostale FI","Ostale FI","Ostale FI","Osiguranja","Mirovinski fondovi","Država","Država","Država","Stanovništvo","Stanovništvo","Inozemstvo","NA","Total"))
financijski_racuni <- financijski_racuni %>% left_join(kratice_sektor,by="sektor") %>% left_join(kratice_protustrana,by="protustrana_puni_naziv")
rm(kratice_protustrana,kratice_sektor)

# kreiranje datuma da bude prikazan kao u DWH
financijski_racuni <- financijski_racuni %>% mutate(datum = make_date(ifelse(substr(kvartal,6,6)=="4",as.numeric(substr(kvartal,1,4))+1,as.numeric(substr(kvartal,1,4))), ifelse(substr(kvartal,6,6)=="4",1,as.numeric(substr(kvartal,6,6))*3+1), 1)-1) %>% select(-kvartal)

# spremanje dataframea
save(financijski_racuni,file = "FinRacuni.Rda")

# micanje baze iz workspacea
rm(financijski_racuni)
