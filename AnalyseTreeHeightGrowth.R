# Analizza lo sviluppo in altezza
rm(list=ls())

library(sqldf)
library(latticeExtra)

# get the data
giturl <- "https://raw.githubusercontent.com/NuoroForestrySchool/TreeStemAnalysis/master/"
filen <- "ReadMeasurements.R"
sf <- paste(ifelse(file.exists(filen),"", giturl), filen, sep="") 
source(sf)

## Stima di h_sez.
## h_sez non è stata misurata in bosco, avendo già misurato con cura le altezze dei palchi 
##                         ed avendo prelevato le sezioni negli interpalchi
##                         (sotto ogni terzo palco a partre dalla cima).
## Si tratta quindi di assegnare ad ogni sezione una altezza in base alla logica del rilievo
## (per procedere occorre interpretare i campi ProgSezione ed IdSezPalco:
##  * le sezioni sono identificate con un numero progressivo, a partire da 1 per la base
##  * IdSezPalco riporta (in genere) l'identificativo del palco posto subito sopra la sezione 
##     salvo nei casi "IdSezPalco %in% c(0.0, 130, 999)" 
##       che indicano rispettivamente 'base', 'sez.130', 'lunghezza tot'
##    le righe con IdSezPalco=999 vengono aggiunte per inserire la lunghezza totale nelle serie)
##  * h_sez è quindi la metà tra le altezze dei palchi 'Idpalco' e 'Idpalco'-1!

## Invece di questo procedimento, decisamente complesso e non standard
## -----------
## Sezioni01 <- merge(all.x=T, Sezioni, Palchi, by=c("IdFusto","IdSezPalco"), by.y=c("Idfusto","Idpalco"))
## Sezioni01 <- merge(all.x=T, data.frame(Sezioni01,Idpalco_succ=Sezioni01$IdSezPalco-1),
##                   data.frame(Palchi0[,1:2],h_palco_succ=Palchi0$h_palco),
##                   by.x=c("IdFusto","Idpalco_succ"), by.y=c("Idfusto","Idpalco"))
## Sezioni01$h_sez <- with(Sezioni01, (h_palco+h_palco_succ)/2)
## Sezioni1 <- rbind(Sezioni1, 
##                  with(Fusti[!is.na(Fusti$Idfusto),], 
##                       data.frame(IdFusto=Idfusto, ProgSezione=999, 
##                                  NumeroAnelli=0, IdSezPalco=NA, 
##                                  h_sez=lunghezza*100, stringsAsFactors=F)))
## Sezioni1 <- merge(Sezioni1, 
##                  with(Sezioni1[Sezioni1$ProgSezione==1,],
##                       data.frame(IdFusto, Eta_h_sez=NumeroAnelli)))
## Sezioni1$Eta_h_sez <- Sezioni1$Eta_h_sez - Sezioni1$NumeroAnelli + .5
## ---- ho deciso di procedere sfruttando la standard SQL

### Sviluppo in altezza in base agli anelli contati sulle sezioni
source("Sezioni.R")
xyplot(h_sez~Eta_h_sez|IdFusto, Sezioni1, type="o", grid=T, layout=c(3,3))

###
# Conteggio dei palchi, a partire dalla base, escludendo quelli NON primaverili
source("Palchi.R")

### Sviluppo in altezza - Confronto tra NumeroAnelli, IdPalco ed EtaPalco
H_growth <- sqldf('select IdFusto, "Eta.sez" as G, 1.0*Eta_h_Sez as E_oppure_IdPalco, 1.0*h_sez as H from Sezioni1 
             UNION select IdFusto, "Id.palco",     1.0*IdPalco,                       1.0*h_palco from Palchi1 
             UNION select IdFusto, "Eta.palco",    1.0*Eta,                           1.0*h_palco from PalchiPrim')

sbs <- unique(Fusti$IdFusto)[3]
xyplot(H~E_oppure_IdPalco|IdFusto, groups=G, data=H_growth, subset=IdFusto %in% sbs, grid=T,
       type=c("p","p","p"), par.settings=simpleTheme(
         pch=c(4,1,5), cex=c(1, 1.5, .3)), auto.key=list(columns=3)) +
  as.layer(xyplot(H~E_oppure_IdPalco|IdFusto, data=H_growth[H_growth$G=="Eta.palco",], subset=IdFusto %in% sbs, grid=T, t="l"))
       
# Aggiungi i descrittori per il grafico
H_growth1 <- sqldf('select A.*, CondConcorrenza, dim as Dimensioni 
                  from H_growth A natural join Fusti1')   # where IdFusto="A3"')
xyplot(H~E_oppure_IdPalco|CondConcorrenza+Dimensioni, groups=G, data=H_growth1, grid=T,
       type=c("p","p","p"), par.settings=simpleTheme(
         pch=c(4,1,5), cex=c(1, 1.5, .3)), auto.key=list(columns=3)) +
  as.layer(xyplot(H~E_oppure_IdPalco|CondConcorrenza+Dimensioni, data=H_growth1[H_growth1$G=="Eta.palco",], t="l"))

