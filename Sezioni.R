## Completamento e pulizia tabella Sezioni
require(sqldf)

Sezioni00 <- Sezioni
if (!is.na(i<-pmatch("Note",names(Sezioni00)))) Sezioni00 <- Sezioni00[, -i]

# Controlla incongruenze tra NumeroAnelli e Letture
# PassoCampTemporale # Passo utilizzato nella maggior parte dei casi ma non sempre!
# in particolare su sez130 e nella parte in nterna di diverse sez Passo = 1
NumeroAnelliERR <- sqldf('select IdFusto, PassoCampTemporale, IdSezPalco, 
                         EtaAbbattimento - (NumeroAnelli + min(EtaCambiale) -1) as test 
                         from Raggi natural join Sezioni00 natural join Fusti1 
                         group by IdFusto, IdSezPalco 
                         having test >0 or test <= -PassoCampTemporale')
if (nrow(NumeroAnelliERR)>0) {
  print(NumeroAnelliERR); 
  stop("ATTENZIONE - NumeroAnelli incongruente con Letture, se test>0, aggiungere Test anelli")
}

# Calcola h_sez in base ad h_palco ed aggiungi le cime come sezioni 999
Sezioni01 <- sqldf('
            select A.*, 
                   1.0*(CASE WHEN ProgSezione = 1 THEN 0
                   WHEN IdSezPalco = 130 THEN 130
                   ELSE (B1.h_palco + B2.h_palco)/2.0
                   END) AS h_sez
                   from (Sezioni00 A left outer join Palchi B1 
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
                   (select IdFusto, NumeroAnelli as Eta from Sezioni00 where ProgSezione=1)
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

library('lattice')
Sezioni1 <- Sezioni03[order(Sezioni03$IdFusto, Sezioni03$ProgSezione),]
xyplot(h_sez~Eta_h_sez|IdFusto, Sezioni1, type="o", grid=T, layout=c(3,3))
