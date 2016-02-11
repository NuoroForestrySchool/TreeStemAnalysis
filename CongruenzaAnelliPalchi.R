## controlli preliminari per comporre la tabella PalchiDaElimEtInserire
# Il numero di palchi presenti tra due sezioni successive deve evidentemete coincidere 
# con la differenza nel numero di anelli rilevati (e quindi di Età_palco) tra le due Sezioni

ToppiTraSezioni <- sqldf('select A.IdFusto, A.Eta_h_sez as Eta_h_sez_sotto, A.ProgSezione as ProgSezione_sotto, 
              A.h_sez as h_sez_sotto, min(B.h_sez) as h_sez_sopra, 
              floor(min(B.Eta_h_sez)) - floor(A.Eta_h_sez) as DiffEta,
              A.NumeroAnelli - max(B.NumeroAnelli) as DiffNumAn 
              from Sezioni1 A join Sezioni1 B
              on A.IdFusto=B.IdFusto and A.h_sez<B.h_sez
              group by A.IdFusto, A.h_sez')
IncongruenzeInDB <- sqldf('select A.IdFusto, A.ProgSezione_sotto, A.Eta_h_sez_sotto, 
                      DiffEta, count(*) as NumPalchi, 
                      count(*) - DiffEta as NumPlcInEcc, 
                      count(*) - DiffNumAn as NumPlcInEcc2  
               from ToppiTraSezioni A join Palchi1 B
               on A.IdFusto=B.IdFusto and h_palco > h_sez_sotto 
               and h_palco <= h_sez_sopra
               group by A.IdFusto, h_sez_sotto')
IncongruenzeResidue <- 
               sqldf('select A.IdFusto, A.ProgSezione_sotto, A.Eta_h_sez_sotto, 
                      DiffEta, count(*) as NumPalchi, 
                      count(*) - DiffEta as NumPlcInEcc, 
                      count(*) - DiffNumAn as NumPlcInEcc2  
               from ToppiTraSezioni A join PalchiPrim B
               on A.IdFusto=B.IdFusto and h_palco > h_sez_sotto 
               and h_palco <= h_sez_sopra
               group by A.IdFusto, h_sez_sotto having NumPlcInEcc<>0')
if (nrow(IncongruenzeResidue)>0) {
  cat("ERROR:  incongruenze tra numero di anelli e numero di palchi al di sopra delle seguenti Sezioni\n")
  print(IncongruenzeResidue)
  #stop("-------- stopping!!")
}
error <- sqldf('select IdFusto, EtaAbbattimento, n as NumPalchiPrimaverili from Fusti1 natural join 
               (select IdFusto, count(*) as n from PalchiPrim group by IdFusto) where EtaAbbattimento <> NumPalchiPrimaverili')
if (nrow(error)>0) {
  cat("===================================================================== 
      ATTENZIONE: nei seguenti casi EtaAbbattimento <> NumPalchiPrimaverili \n")
  print(error)
  cat("===================================================================== \n")
}