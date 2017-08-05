library(dtw)
## > is.dtw
## Error: object 'is.dtw' not found
my.is.dtw <- function(a) inherits(a, "dtw")

wd <- "2017Douglasia-Podernovo"
dbfn <- "2017PodernovoDouglasiaProfiliSemplificati_GSapp.sqlite"
if (basename(getwd())!=wd) setwd(wd)
library(DBI, quietly=T)
dbcon <- dbConnect(RSQLite::SQLite(),dbfn)
if(!(dbIsValid(con))) print("ERROR: non database!")

fu <- dbReadTable(con, "FustiAM_eta")
ir <- dbReadTable(con, "CalcolaIncRaggio")
ir <- ir[ir$Id_fusto %in% fu$Id_fusto,]
sr <- dbGetQuery(con, "
   SELECT A.Id_fusto,
          A.Id_rotella,
                 A.num_inc num_inc_a,
                 B.num_inc num_inc_b,
                 A.num_inc - B.num_inc diff,
                 B.note
                 FROM SintesiRotelleDaAdF A
                 LEFT JOIN
                 SintesiRotelleDaAdF B USING (
                 Id_fusto,
                 Id_rotella
                 )
                 WHERE A.ripetizione = 'a' AND 
                 B.ripetizione = 'b' AND 
                 num_inc_a <> num_inc_b;
   ")

ir <- ir[with(ir, order(Id_fusto, Id_rotella, ripetizione, anno)),]

for(idf in unique(ir$Id_fusto)[33]) {
  for(idr in c(6)) {
  ir1 <- ir[ir$Id_fusto==idf & ir$Id_rotella==idr,]
  templ <- ir1[ir1$ripetizione=='a', c("anno", "ir")]
  query <- ir1[ir1$ripetizione=='b', c("anno", "ir")]
  ir.ts <- merge(templ, query, by="anno", all=T, suffixes = c(".t", ".q"))
  ir.ts <- ts(ir.ts[,-1], start = ir.ts$anno[1], names=c("templ", "query"))

cat(nrow(templ), nrow(query))
stpps <- c("symmetric1"
,"symmetric2"
,"asymmetric"
,"rabinerJuangStepPattern(6, 'c')"
)
for(stpp in stpps[3]) {
alg <- tryCatch(
  dtw(ir.ts[,"templ"], window(ir.ts[,"query"],end=2016),keep=TRUE
      , step.pattern=eval(parse(text=stpp))
) #      , open.begin=T, open.end=T)
  , error = function(e) cat(" -ERROR in dtw- ")
  , finally = NA)
cat(" test:",my.is.dtw(alg),"\n")
if(my.is.dtw(alg)) {
  plot(alg, type="twoway",offset=-2, xlab='2017-anno', ylab='inc_raggio') 
} else {
  ir1$ir[ir1$ripetizione=='b'] <-1.52+ir1$ir[ir1$ripetizione=='b']
  print(
    xyplot(ir.ts, col=c('red', 'blue'), superpose = T, grid=T
    , main=paste("Fusto:",idf,"- Rot:", idr
                 , "- lungh.a =", nrow(templ),"e .b =", nrow(query)))
  )
}
title(main=paste("Fusto:",idf,"- Rot:", idr
             , "- lungh.a =", nrow(templ),"e .b =", nrow(query))
      ,sub = stpp)
}
}
}

