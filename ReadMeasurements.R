# Get the data
filen <- "AdF_Pinus.pinaster_Pattada.Rdata"
if (file.exists(filen)) DB <- filen else 
  {
  library(RCurl)
  library(foreign)
  giturl <- "https://github.com/NuoroForestrySchool/TreeStemAnalysis/raw/master/"
  ourl <- paste(giturl, filen, sep="")
  if (!url.exists(ourl)) stop("ERROR: DATI non trovati!")
  DB <- url(ourl)
}
load(DB)

# completa Fusti con EtaAbbattimento
Fusti1 <- sqldf('select A.*, EtaAbbattimento 
                from Fusti A natural join (select IdFusto, NumeroAnelli as EtaAbbattimento
                from Sezioni where IdSezPalco=0) ')
# verifica congruenza logica Fusti.lunghezza - max(h_palco)
error <- sqldf('select IdFusto, lunghezza*100 as l, max_h_palco as cim 
                from Fusti join 
                   (select IdFusto, max(h_palco) as max_h_palco 
                    from Palchi group by IdFusto) using (IdFusto) 
                    where l < max_h_palco')
if (nrow(error)>0) {print(error); stop("errore: h_palco > lunghezza")}

# Estendi i 'palchi' ad includere la cima del fusto
Palchi0 <- sqldf('select A.*, PNP as PalcoNonPrimaverile from Palchi A left natural join
                 (select *, 1 as PNP from PalchiNonPrimaverili)')
Palchi1 <- sqldf('select IdFusto, NumPalchi+1 as IdPalco, lunghezza*100 as h_palco, 
                  NULL as PalcoNonPrimaverile, EtaAbbattimento from Fusti1 natural join 
                 (select IdFusto, count(*) as NumPalchi from Palchi0 group by IdFusto)
                 UNION select IdFusto, IdPalco, h_palco, PalcoNonPrimaverile, NULL from Palchi0')

# Conteggio dei palchi, a partire dalla base, escludendo quelli NON primaverili
PalchiPrim0 <- Palchi1[is.na(Palchi1$PalcoNonPrimaverile),]
PalchiPrim <- sqldf('select A.IdFusto IdFusto, A.IdPalco IdPalco, A.h_palco h_palco
                             , count(*) as Eta 
      from PalchiPrim0 A join PalchiPrim0 B 
      on A.IdFusto=B.IdFusto and A.h_palco >= B.h_palco 
      group by A.IdFusto, A.h_palco')
error <- sqldf('select IdFusto, EtaAbbattimento, n as NumPalchiPrimaverili from Fusti1 natural join 
      (select IdFusto, count(*) as n from PalchiPrim group by IdFusto) where EtaAbbattimento <> NumPalchiPrimaverili')
if (nrow(error)>0) {
  cat("===================================================================== 
      ATTENZIONE: nei seguenti casi EtaAbbattimento <> NumPalchiPrimaverili \n")
  print(error)
  cat("===================================================================== \n")
}

