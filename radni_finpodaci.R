library(tidyverse)
library(lubridate)
library(readxl)

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

load("finpodaci.Rda")

# 0. Popis obrazaca, izvještaja, redaka i stupaca ####
#popis <- finpodaci %>% select(sektor,izvjestaj,obrazac,oznaka_obrasca,oznaka_retka,redak,stupac,stupac_naziv) %>% distinct()

# 1. Osiguranja - bilanca ####
os_bilanca <- finpodaci %>% filter(izvjestaj %in% c("TIDO","TIDRE","TIDO-RE") & oznaka_obrasca=="Standardno" & obrazac %in% c("Bilanca","IFP") & stupac_naziv %in% c("TIDO - Bilanca - Tekuća godina - Život","TIDO - Bilanca - Tekuća godina - Neživot ","TIDRE - Bilanca - Tekuća godina - Život","TIDRE - Bilanca - Tekuća godina - Neživot ","TIDO-RE - IFP - Tekuća godina - Neživot ","TIDO-RE - IFP - Tekuća godina - Život")) %>% unite("obrazac",izvjestaj,obrazac,sep="-") %>% unite("redak",oznaka_retka,redak,sep=".") %>% select(datum,vrsta=stupac,subjekt,obrazac,redak,iznos) %>% mutate(vrsta=str_trim(vrsta),redak=str_trim(redak))
# uvoz razina iz excela
razine <- read_excel("Mapiranje obrazaca - osiguranja.xlsx",sheet = "za_izvoz_bilanca")
os_bilanca <- full_join(os_bilanca,razine,by=c("obrazac","redak")) %>% na.omit()

# Struktura imovine
pom <- os_bilanca %>% group_by(datum,razina2) %>% filter(datum>="2011-09-30" & razina1=="Imovina") %>% summarise(iznos=sum(iznos,na.rm=T)/1000000000)
ggplot(pom,aes(x=datum,y=iznos,fill=razina2)) + geom_area() + ggtitle("Imovina") + theme(legend.position = "top") + theme(legend.position = "top") + labs(x="",y="mlrd. HRK")
# Struktura ulaganja
pom <- os_bilanca %>% group_by(datum,razina3) %>% filter(datum>="2011-09-30" & razina2=="Ulaganja") %>% summarise(iznos=sum(iznos,na.rm=T)/1000000000)
ggplot(pom,aes(x=datum,y=iznos,fill=razina3)) + geom_area() + ggtitle("Ulaganja") + theme(legend.position = "top") + theme(legend.position = "top") + labs(x="",y="mlrd. HRK")
# Struktura obveza
pom <- os_bilanca %>% group_by(datum,razina2) %>% filter(datum>="2011-09-30" & razina1=="Obveze") %>% summarise(iznos=sum(iznos,na.rm=T)/1000000000)
ggplot(pom,aes(x=datum,y=iznos,fill=razina2)) + geom_area() + ggtitle("Obveze") + theme(legend.position = "top") + theme(legend.position = "top") + labs(x="",y="mlrd. HRK")
# Struktura tehničkih pričuva
pom <- os_bilanca %>% group_by(datum,razina3) %>% filter(datum>="2011-09-30" & razina2=="Tehničke pričuve") %>% summarise(iznos=sum(iznos,na.rm=T)/1000000000)
ggplot(pom,aes(x=datum,y=iznos,fill=razina3)) + geom_area() + ggtitle("Tehničke pričuve") + theme(legend.position = "top") + labs(x="",y="mlrd. HRK")


