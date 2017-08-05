## Time Series:
##     ir  [incrementi di raggio]
ir.ts <- ts(start = 1995,
data = c(
  0.2, 0.5, 0.6, 0.7, 0.7, 0.6, 0.7, 0.4, 0.5, 0.4, 0.4, 0.3, 0.3, 0.3
  , 0.3, 0.3, 0.3, 0.2, 0.1, 0.2, 0.3, 0.2, 0.1)
)
templ <- ir.ts
# Dynamic Time Warping
library("dtw")
N <- length(templ)
wrp <- matrix(data = c(1:N, 1:N), nrow=N)

M <- max(wrp[,2])
query <- ts(data = rep(NA, M))
for(i in 1:nrow(wrp)) query[wrp[i,2]] <- templ[wrp[i,1]]
alg <- dtw(query, templ, keep.internals = T)
ofs <- max(.2, 1.4*max(abs(as.vector(query)-as.vector(templ))))
plot(alg); plot(alg, type="twoway",offset=ofs)

wrp[,2] <- wrp[,1]-1
wrp[c(1,2),2] <- c(1,2)
M <- max(wrp[,2])
query <- ts(data = rep(NA, M))
for(i in 1:nrow(wrp)) query[wrp[i,2]] <- templ[wrp[i,1]] +
  ifelse(is.na(query[wrp[i,2]]), 0, query[wrp[i,2]])
alg <- dtw(query, templ, keep.internals = T)
ofs <- max(.2, 1.4*max(abs(as.vector(query)-as.vector(templ)[1:M])))
plot(alg); plot(alg, type="twoway",offset=ofs)

plot(wrp[,1], wrp[,2], type="l")
with(alg, lines(index1, index2, col="blue"))
