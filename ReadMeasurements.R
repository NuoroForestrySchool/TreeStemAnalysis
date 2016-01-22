rm(list=ls())
library("googlesheets")
suppressMessages(library(dplyr))

RilievoAdF_Pinus.pinaster_Pattada <- gs_title("RilievoAdF_Pinus-pinaster_Pattada")
ws_list<-data.frame(ws_title=RilievoAdF_Pinus.pinaster_Pattada$ws$ws_title, data=F)
ws_list$data[c(1,2,4,5,7,9)]<-T
db_tbls <- ws_list[ws_list$data,]
db_tbls
##           ws_title data
## 1         Campagne TRUE
## 2 anagr_unita_topo TRUE
## 4            Fusti TRUE
## 5           Palchi TRUE
## 7          Sezioni TRUE
## 9            Raggi TRUE
Campagne <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="Campagne")
anagr_unita_topo <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="anagr_unita_topo")
Fusti <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="Fusti")
Fusti$cond_concorrenza<-factor(Fusti$cond_concorrenza,ordered=T)
Fusti <- Fusti[!is.na(Fusti$id_fusto),]
d <- c("piccolo","medio","grande")
for(i in levels(Fusti$cond_concorrenza)) 
  Fusti$dim[Fusti$cond_concorrenza==i] <- 
  d[rank(Fusti$d_130[Fusti$cond_concorrenza==i], ties.method="first")]
Fusti$dim <- factor(Fusti$dim, levels=d, ordered=T)
Palchi <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="Palchi")
Sezioni <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="Sezioni")
Raggi <- RilievoAdF_Pinus.pinaster_Pattada %>% gs_read(ws="Raggi")
Campagne 
anagr_unita_topo 
Fusti 
Palchi
Sezioni 
Raggi 
