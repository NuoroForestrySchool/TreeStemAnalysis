# Analizza i profili deifusti, calcola volumi ed incrementi
rm(list=ls())

library(sqldf)
library(latticeExtra)

# get the data
giturl <- "https://raw.githubusercontent.com/NuoroForestrySchool/TreeStemAnalysis/master/"
filen <- "ReadMeasurements.R"
sf <- paste(ifelse(file.exists(filen),"", giturl), filen, sep="") 
source(sf)

source("Sezioni.R")

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
i <- unique(Fusti$IdFusto)[3]
f <- unique(Fusti$IdFusto)
for (i in unique(Fusti$IdFusto)) {
  p <- xyplot(h_sez~ifelse(IdRaggio=="a",-1,1)*Lettura|IdFusto, 
               xlim=c(-18,16), xlab="raggio [cm]", 
               ylim=c(-50, 1950), ylab="altezza [cm]", grid=T,
               group=paste(EtaCambiale,IdRaggio), 
               data=RaggiConfrontabili, subset=IdFusto %in% i, t="o", pch=0, lwd=2) +
    as.layer(xyplot(h_sez~ifelse(IdRaggio=="a",-1,1)*Lettura|IdFusto, 
                    group=paste(EtaCambiale,IdRaggio), 
                    data=LettureAccessorie, subset=IdFusto %in% i, t="o", pch=4, lty=2)) +
    as.layer(xyplot(h_palco~M|IdFusto, data=data.frame(Palchi1,M=-17), subset=IdFusto %in% i, 
                    panel=function(x, y, p=Palchi1$IdPalco[Palchi$IdFusto==i], ...){
                      panel.points(x, y, pch=2, cex=2, ...)
                      panel.text(x,y, labels=Palchi1$IdPalco, cex=.7)})) +
    as.layer(xyplot(h_sez~M|IdFusto, data=data.frame(Sezioni1,M=-15), subset=IdFusto %in% i, 
                    panel=function(x, y, e=Sezioni1$Eta_h_sez[Sezioni1$IdFusto==i], ...){
                      panel.points(x, y, pch=1, cex=3, ...)
                      panel.text(x,y, labels=round(e,1), cex=.7)}))
#  trellis.device("svg", width=6.3, height=8.6, filename=paste(i,"svg",sep="."))
  print(p)
#  dev.off()
}


