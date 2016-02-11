## Completamento e pulizia tabella Palchi 
##    (condizionamento da tabella PalchiDaElimEtInserire)
require(sqldf)

Palchi1 <- Palchi
if (!is.na(i<-pmatch("Note",names(Palchi1)))) 
  Palchi1 <- Palchi1[, -i]
PalchiDaElimEtInserire1 <- PalchiDaElimEtInserire
if (!is.na(i<-pmatch("Note",names(PalchiDaElimEtInserire1)))) 
  PalchiDaElimEtInserire1 <- PalchiDaElimEtInserire1[, -i]

# Estendi i 'palchi' ad includere la cima del fusto
Palchi00 <- sqldf('select IdFusto, IdPalco, h_palco from Palchi1
                  UNION
                   select IdFusto, IdCima, lunghezza*100 from Fusti natural join 
                     (select IdFusto, max(IdPalco)+1 as IdCima from Palchi1 group by IdFusto)')

### Adattamento della serie dei palchi alla sequenza dei conteggi degli anelli per sezione
### (Definizione della serie dei Palchi Primaverili compatibile con gli anelli rilevati su ciascuna sezione)
## Eliminazione plachi in eccesso rispetto alla differenza tra i conteggi degli anelli alle sezioni più vicine
PalchiPrim01 <- sqldf('select IdFusto, IdPalco, h_palco from Palchi00
                       natural join 
                         (select IdFusto, IdPalco from Palchi00
                          EXCEPT select IdFusto, IdPalco from PalchiDaElimEtInserire
                              where NumPalchiDaInserireSopra is NULL)')
## Inserimento palchi mancanti rispetto alla differenza nei conteggi degli anelli sulle sezioni più vicine
PalchiDaIns0 <- sqldf('select A.IdFusto IdFusto, A.IdPalco IdPalco_sotto, NumPalchiDaInserireSopra,
                              A.h_palco as h_palco_sotto, B.h_palco as h_palco_sopra
      from (Palchi00 A join Palchi00 B on A.IdFusto=B.IdFusto and A.IdPalco+1=B.IdPalco)
      natural join 
      (select * from PalchiDaElimEtInserire where NumPalchiDaInserireSopra is not NULL)')
if (max(PalchiDaElimEtInserire$NumPalchiDaInserireSopra, na.rm=T)>1)
  stop("Palchi da inserire > 1 - OPZIONE ancora da implementare!")
PalchiPrim02 <- sqldf('select * from PalchiPrim01 
                     UNION select IdFusto, 10000+100*IdPalco_sotto+1 as IdPalco,
                     (h_palco_sopra+h_palco_sotto)/2 as h_palco from PalchiDaIns0')
PalchiPrim02 <- PalchiPrim02[order(PalchiPrim02$IdFusto, PalchiPrim02$h_palco),]

# Conteggio dei palchi (primaverili), a partire dalla base
PalchiPrim <- sqldf('select A.*, count(*) as Eta 
                    from PalchiPrim02 A join PalchiPrim02 B 
                    on A.IdFusto=B.IdFusto and A.h_palco >= B.h_palco 
                    group by A.IdFusto, A.h_palco')


