rw_ok <- sqldf("select A.* from rw_all A natural join Sects_sy where max_b_year = 2017")

rw_cor <- sqldf("select TreeId, SectId, gyear
                       , A.rrw rrw_a, B.rrw rrw_b 
                  from (select * from rw_ok where side='a') A 
                    join (select * from rw_ok where side='b') B
                    using(TreeId, SectId, gyear)")
ts <- with(rw_cor, paste(TreeId, SectId, sep="-"))
for (i in unique(ts)) {
  print(i)
  rw_cor$rw_ab_cor[ts==i] <- with(rw_cor[ts==i,]
                                  , localCor(rrw_a, rrw_b, 3))
}
#  rrw_a+rrw_b+
obj1 <- xyplot(I(rrw_a - rrw_b)^2  ~ gyear| ts, rw_cor, type='l'
               , scales=list(y=list(log="e"))
       , panel=function(...) {panel.xyplot(...)
                              panel.grid(h=-1, v=-1, ...)}
       , main="Comparing similarity measures")
obj2 <- xyplot(I((rw_ab_cor+1)/2)  ~ gyear| ts, rw_cor, type='l')
doubleYScale(obj1, obj2, text = c("E.dist. relativeRW", "scaled_localCor"), add.ylab2 = TRUE, main="Comparing similarity measures")
