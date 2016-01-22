# Analizza lo sviluppo in altezza

source("ReadMeasurements.R")

## primo grafico di prova (h_sez ed Età_h_sez sono in realtà da ricalcolare)
library(lattice)
xyplot(h_sez~Età_h_sez|Id_Fusto, Sezioni)
##  id_sez_palco:= se !=c(0.0, 1.3, NA) allora la sezione è sotto il palco 'id_palco'='id_sez_palco'

## Stima di h_sez. (già realizzata nel tabellone ma è necessario riprenderla, si rischiano incoerenze)
## h_sez non è stata misurata in bosco, avendo già misurato con cura le altezze dei palchi 
##                         ed avendo prelevato le sezioni negli interpalchi
##                         (sotto ogni terzo palco a partre dalla cima).
## Si tratta quindi di assegnare ad ogni sezione una altezza in base alla logica del rilievo
## Per procedere occorre interpretare i campi prog_sezione ed id_sez_palco
##   (le righe con id_sez_palco=999 sono state aggiunte per inserire la lunghezza totale nelle serie)
##                        a metà tra i palchi 'id_palco' e 'id_palco'-1!
Sezioni0 <- Sezioni[Sezioni$id_sez_palco!=999,c("Id_Fusto","prog_Sezione","id_sez_palco","Numero_anelli")]
Palchi0 <- Palchi[,2:10]
Sezioni01 <- merge(all.x=T, Sezioni0,Palchi0[,1:3],by.x=c("Id_Fusto","id_sez_palco"), by.y=c("id_fusto","id_palco"))
Sezioni01 <- merge(all.x=T, data.frame(Sezioni01,id_palco_succ=Sezioni01$id_sez_palco-1),
                   data.frame(Palchi0[,1:2],h_palco_succ=Palchi0$h_palco),
                   by.x=c("Id_Fusto","id_palco_succ"), by.y=c("id_fusto","id_palco"))
Sezioni01$h_sez <- with(Sezioni01, (h_palco+h_palco_succ)/2)
Sezioni01$h_sez[Sezioni01$prog_Sezione==1]<-0
Sezioni01$h_sez[Sezioni01$id_sez_palco==1.3]<-130
Sezioni1 <- Sezioni01[,c("Id_Fusto","prog_Sezione","Numero_anelli","id_sez_palco","h_sez" )]
Sezioni1 <- rbind(Sezioni1, 
                  with(Fusti[!is.na(Fusti$id_fusto),], 
                       data.frame(Id_Fusto=id_fusto, prog_Sezione=999, 
                                  Numero_anelli=0, id_sez_palco=NA, 
                                  h_sez=lunghezza*100, stringsAsFactors=F)))
Sezioni1 <- Sezioni1[order(Sezioni1$Id_Fusto, Sezioni1$prog_Sezione),]
Sezioni1 <- merge(Sezioni1, 
                  with(Sezioni1[Sezioni1$prog_Sezione==1,],
                       data.frame(Id_Fusto, Eta_h_sez=Numero_anelli)))
Sezioni1$Eta_h_sez <- Sezioni1$Eta_h_sez - Sezioni1$Numero_anelli + .5
Sezioni1$Eta_h_sez[Sezioni1$prog_Sezione==1] <- 0
Sezioni1$Eta_h_sez[Sezioni1$prog_Sezione==999] <- floor(Sezioni1$Eta_h_sez[Sezioni1$prog_Sezione==999])
## Quando sezioni successive presentano lo stesso numero di anelli, l'approssimazione del mezzo anno non si applica bene
##   spalmiano
library(sqldf)
multiple.rows <- sqldf('select Id_Fusto, prog_Sezione, Eta_h_sez, count(*) as n, min(prog_Sezione) as pSmin from Sezioni1 group by Id_Fusto, Eta_h_sez having n>1')
updated.rows  <- sqldf('select Id_Fusto, A.prog_Sezione, 
                       (A.prog_Sezione-pSmin+1)/(n+1)+floor(Eta_h_sez) as Eta_h_sez2 
                       from Sezioni1 as A join "multiple.rows" using (Id_Fusto, Eta_h_sez)')
## ho provato ad inserire i valori ricalcolati al loro posto sia con UPDATE, sia con REPLACE, senza soddisfazione!

Sezioni2 <- merge(Sezioni1, updated.rows, all.x=T)
Sezioni2$Eta_h_sez[!is.na(Sezioni2$Eta_h_sez2)] <- Sezioni2$Eta_h_sez2[!is.na(Sezioni2$Eta_h_sez2)]
Sezioni2 <- merge(Sezioni2, Fusti[,c("id_fusto","cond_concorrenza", "dim")], by.x="Id_Fusto", by.y="id_fusto")
xyplot(h_sez~Eta_h_sez|dim*cond_concorrenza, Sezioni2, type="o", grid=T)

