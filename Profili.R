# Analizza lo sviluppo in altezza
rm(list=ls())

library(sqldf)
library(latticeExtra)

# get the data
giturl <- "https://raw.githubusercontent.com/NuoroForestrySchool/TreeStemAnalysis/master/"
filen <- "ReadMeasurements.R"
sf <- paste(ifelse(file.exists(filen),"", giturl), filen, sep="") 
source(sf)

## ================= Inizio - sezione da spostare in ReadMeasurements.R
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

### Sviluppo in altezza in base agli anelli contati sulle sezioni
# Calcola h_sez in base ad h_palco ed aggiungi le cime come sezioni 999
Sezioni01 <- sqldf('
                   select A.*, 
                   1.0*(CASE WHEN ProgSezione = 1 THEN 0
                   WHEN IdSezPalco = 130 THEN 130
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
## ================= fine

# verifica che i campi chiave siano inivoci
if (nrow(Raggi)!=
      with(Raggi,
    nrow(unique(data.frame(IdFusto,IdSezPalco,IdRaggio,EtaCambiale))))) 
    stop("ERROR: duplicate keys in Raggi!")

## Verifica grafica dei rilievi sulle rotelle
# recupera h_sez da Sezioni elaborate
Raggi01 <- sqldf('select A.*, h_sez from Raggi A natural join Sezioni1')
Raggi01 <- with(Raggi01, Raggi01[order(IdFusto, IdRaggio, EtaCambiale, h_sez),])
# distingue profili basati su una progressione crescente di letture da quelli accessori
tmp1 <- sqldf('select IdFusto, IdRaggio, EtaCambiale, count(*) as nl 
              from Raggi group by IdFusto, IdRaggio, EtaCambiale')
ProfiliConfrontabili <- sqldf('select A.* from tmp1 A join tmp1 B 
              on A.IdFusto=B.IdFusto and A.IdRaggio=B.IdRaggio 
              and A.EtaCambiale>=B.EtaCambiale 
              group by A.IdFusto, A.IdRaggio, A.EtaCambiale 
              having A.nl >= max(B.nl)')
RaggiConfrontabili <- sqldf('select A.* from Raggi01 A natural join ProfiliConfrontabili')
LettureAccessorie <- sqldf('select * from Raggi01 EXCEPT selecT * from RaggiConfrontabili')
i <- unique(Fusti$IdFusto)[2]
for (i in unique(Fusti$IdFusto)) {
  p <- xyplot(h_sez~ifelse(IdRaggio=="a",-1,1)*Lettura|IdFusto, 
               xlab="raggio [cm]", ylab="altezza [cm]", grid=T,
               group=paste(EtaCambiale,IdRaggio), 
               data=RaggiConfrontabili, subset=IdFusto %in% i, t="o", pch=0, lwd=2) +
    as.layer(xyplot(h_sez~ifelse(IdRaggio=="a",-1,1)*Lettura|IdFusto, 
                    group=paste(EtaCambiale,IdRaggio), 
                    data=LettureAccessorie, subset=IdFusto %in% i, t="o", pch=4, lty=2))
  trellis.device("svg", width=6.3, height=8.6, filename=paste(i,"svg",sep="."))
  print(p)
  dev.off()
}

