library(dtw)
## > is.dtw
## Error: object 'is.dtw' not found
my.is.dtw <- function(a) "dtw" %in% class(a)
for(idf in unique(ir$Id_fusto)[1:4]) {
  ir1 <- ir[ir$Id_fusto==idf & ir$Id_rotella==1,]
templ <- ir1$ir[ir1$ripetizione=='a']
query <- ir1$ir[ir1$ripetizione=='b']
cat(length(templ), length(query))
alg <- tryCatch(
  dtw(query,templ,keep=TRUE,step=rabinerJuangStepPattern(6,"c"))
  , error = function(e) cat(" -ERROR in dtw- ")
  , finally = NA)
cat(" test:",my.is.dtw(alg),"\n")
if(my.is.dtw(alg)) {
  plot(alg, type="twoway",offset=-2, xlab='2017-anno', ylab='inc_raggio') 
} else {
  ir1$ir[ir1$ripetizione=='b'] <- 2+ir1$ir[ir1$ripetizione=='b']
  print(
    xyplot(ir~anno, groups = ripetizione, data = ir1, type="l", col=c('red', 'blue'),
    main=paste("Fusto:",idf,"- Rot: 1"
               , "- lungh.a =", length(templ),", .b =", length(query)))
  )
}
title(main=paste("Fusto:",idf,"- Rot: 1"
             , "- lungh.a =", length(templ),", .b =", length(query)))
}
