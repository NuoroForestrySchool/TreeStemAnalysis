library("googlesheets")
suppressMessages(library(dplyr))

AdF_Pinus.pinaster_Pattada <- gs_title("AdF_Pinus-pinaster_Pattada")
(t(t(AdF_Pinus.pinaster_Pattada$ws$ws_title)))
[1,] "Campagne"            
[2,] "UnitaTopografiche"   
[3,] "Fusti"               
[4,] "Palchi"              
[5,] "PalchiNonPrimaverili"
[6,] "Sezioni"             
[7,] "Raggi"     

Campagne <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Campagne")
UnitaTopografiche <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="UnitaTopografiche")
Fusti <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Fusti")
Fusti$CondConcorrenza<-factor(Fusti$CondConcorrenza,ordered=T)
Fusti <- Fusti[!is.na(Fusti$IdFusto),]
d <- c("piccolo","medio","grande")
for(i in levels(Fusti$CondConcorrenza)) 
  Fusti$dim[Fusti$CondConcorrenza==i] <- 
  d[rank(Fusti$d_130[Fusti$CondConcorrenza==i], ties.method="first")]
Fusti$dim <- factor(Fusti$dim, levels=d, ordered=T)
Palchi  <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Palchi")
PalchiNonPrimaverili<- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="PalchiNonPrimaverili")
Sezioni <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Sezioni")
Raggi   <- AdF_Pinus.pinaster_Pattada %>% gs_read(ws="Raggi")

Campagne 
UnitaTopografiche 
Fusti 
Sezioni 
Raggi 
Palchi
PalchiNonPrimaverili

save(list=AdF_Pinus.pinaster_Pattada$ws$ws_title, 
     file="AdF_Pinus.pinaster_Pattada.Rdata")
