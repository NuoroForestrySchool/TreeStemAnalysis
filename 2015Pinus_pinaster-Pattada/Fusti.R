## Completamento e pulizia tabella Fusti
require(sqldf)

Fusti1 <- Fusti
Fusti1 <- Fusti1[!is.na(Fusti1$IdFusto),]
if (!is.na(i<-pmatch("Note",names(Fusti1)))) Fusti1 <- Fusti1[, -i]

# codifica del disegno sperimentale all'origine dei rilievi
# in CondizioniDiConcorrenza distinte su 3 livelli
# analizzare un fusto per categoria: piccoli, medi e grandi

Fusti1$CondConcorrenza<-factor(Fusti$CondConcorrenza,ordered=T)

d <- c("piccolo","medio","grande")
for(i in levels(Fusti1$CondConcorrenza)) 
  Fusti1$dim[Fusti1$CondConcorrenza==i] <- 
  d[rank(Fusti1$d_130[Fusti1$CondConcorrenza==i], ties.method="first")]
Fusti1$dim <- factor(Fusti1$dim, levels=d, ordered=T)

# completa Fusti con EtaAbbattimento
Fusti1 <- sqldf('select A.*, EtaAbbattimento 
                 from Fusti1 A natural join (select IdFusto, NumeroAnelli as EtaAbbattimento
                                            from Sezioni where IdSezPalco=0) ')

# verifica congruenza logica Fusti.lunghezza - max(h_palco)
error <- sqldf('select IdFusto, lunghezza*100 as l, max_h_palco as cim
                from Fusti join (select IdFusto, max(h_palco) as max_h_palco
                                 from Palchi group by IdFusto) 
                     using (IdFusto) where l < max_h_palco')
if (nrow(error)>0) {print(error); stop("errore: h_palco > lunghezza")}
