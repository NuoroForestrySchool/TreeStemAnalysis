library("googlesheets")
library('sqldf')
suppressMessages(library(dplyr))

AdF_Pinus.pinaster_Pattada <- gs_title("AdF_Pinus-pinaster_Pattada")
print(t(t(AdF_Pinus.pinaster_Pattada$ws$ws_title)))

DBstructure <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="DBstructure")

CODICI <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="CODICI")
Campagne <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Campagne")
UnitaTopografiche <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="UnitaTopografiche")
Fusti <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Fusti")
Sezioni <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Sezioni")
Raggi   <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Raggi")
Palchi  <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Palchi")
PalchiDaElimEtInserire <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="PalchiDaElimEtInserire")

tab_err <- data.frame()
for (i in 1:nrow(DBstructure)) {
  tab <- DBstructure$Table[i]
  PK <- DBstructure$PK[i]
  dk <- sqldf(paste('select count(*)  from', tab)) - 
        sqldf(paste('select n from (select distinct', PK, ', count(*) as n from', tab, ')'))
  if (dk!=0) rbind(tab_err, data.frame(Table=tab, NumDuplicateKeys=dk))
  }
if (nrow(tab_err)>0) {
  print("DB error: duplicate keys!")
  print(tab_err)
  stop("Stopping!")
}
source("Fusti.R")
source("Sezioni.R")
source("Palchi.R")
source("CongruenzaAnelliPalchi.R")

save(list=AdF_Pinus.pinaster_Pattada$ws$ws_title, 
     file="AdF_Pinus.pinaster_Pattada.Rdata")
