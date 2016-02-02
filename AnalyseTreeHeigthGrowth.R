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
##     salvo nei casi "IdSezPalco %in% c(0.0, 1.3, 999)" 
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
# Calcola h_sez in base ad h_palco ed aggiungi le cime come sezioni 999
Sezioni01 <- sqldf('
            select A.*, 
           1.0*(CASE WHEN ProgSezione = 1 THEN 0
                     WHEN IdSezPalco = 1.3 THEN 130
                     ELSE (B1.h_palco + B2.h_palco)/2
                END) AS h_sez
              from (Sezioni A left outer join Palchi B1 
                    on A.IdFusto = B1.IdFusto and A.IdSezPalco = B1.IdPalco)
                    left outer join Palchi B2 
                    on A.IdFusto = B2.IdFusto and A.IdSezPalco-1 = B2.IdPalco
            UNION 
            select IdFusto, 999 as ProgSezione, 999 as IdSezPalco, 
                   0 as NumeroAnelli, lunghezza*100 as h_sez from Fusti
                   ')

# Calcola Eta_h_sez (prima approssimazione)
Sezioni02 <- sqldf('select A.*,
           1.0*(CASE WHEN ProgSezione = 1 THEN 0
                WHEN ProgSezione = 999 THEN Eta
                ELSE Eta-NumeroAnelli+.5
           END) AS Eta_h_sez
       from Sezioni01 A left natural join 
         (select IdFusto, NumeroAnelli as Eta from Sezioni where ProgSezione=1)
       ')

# Calcola Eta_h_sez (seconda approssimazione)
## Quando sezioni successive presentano lo stesso numero di anelli, 
##  l'approssimazione del mezzo anno non si applica bene, spalmiano!
multiple.rows <- sqldf('select IdFusto, ProgSezione, Eta_h_sez, count(*) as n, min(ProgSezione) as pSmin 
                       from Sezioni02 group by IdFusto, Eta_h_sez having n>1')
updated.rows  <- sqldf('select IdFusto, A.ProgSezione, 
                       1.0*(A.ProgSezione-pSmin+1)/(n+1)+floor(Eta_h_sez) as Eta_h_sez2 
                       from Sezioni02 as A join "multiple.rows" using (IdFusto, Eta_h_sez)')
Sezioni03 <- sqldf('select IdFusto, ProgSezione, IdSezPalco, NumeroAnelli, h_sez, 
          CASE WHEN Eta_h_sez2 IS NOT NULL
               THEN Eta_h_sez2
               ELSE Eta_h_sez
          END AS Eta_h_sez
       from Sezioni02 left natural join "updated.rows"')

Sezioni1 <- Sezioni03[order(Sezioni03$IdFusto, Sezioni03$ProgSezione),]
xyplot(h_sez~Eta_h_sez|IdFusto, Sezioni1, type="o", grid=T, layout=c(3,3))

### Sviluppo in altezza - Confronto tra NumeroAnelli, IdPalco ed EtaPalco
H_growth <- sqldf('select IdFusto, "Eta.sez" as G, Eta_h_Sez as E_oppure_IdPalco, h_sez as H from Sezioni1 
             UNION select IdFusto, "Id.palco",     IdPalco,                       h_palco from Palchi1 
             UNION select IdFusto, "Eta.palco",    Eta,                           h_palco from PalchiPrim')

sbs <- unique(Fusti$IdFusto)[2]
xyplot(H~E_oppure_IdPalco|IdFusto, groups=G, data=H_growth, subset=IdFusto %in% sbs, grid=T,
       type=c("p","p","p"), par.settings=simpleTheme(
         pch=c(4,1,5), cex=c(1, 1.5, .3)), auto.key=list(columns=3)) +
  as.layer(xyplot(H~E_oppure_IdPalco|IdFusto, data=H_growth[H_growth$G=="Eta.palco",], subset=IdFusto %in% sbs, grid=T, t="l"))
       
# Aggiungi i descrittori per il grafico
H_growth1 <- sqldf('select A.*, CondConcorrenza, dim as Dimensioni 
                  from H_growth A natural join Fusti')
xyplot(H~E_oppure_IdPalco|CondConcorrenza+Dimensioni, groups=G, data=H_growth1, grid=T,
       type=c("p","p","p"), par.settings=simpleTheme(
         pch=c(4,1,5), cex=c(1, 1.5, .3)), auto.key=list(columns=3)) +
  as.layer(xyplot(H~E_oppure_IdPalco|CondConcorrenza+Dimensioni, data=H_growth1[H_growth1$G=="Eta.palco",], t="l"))
