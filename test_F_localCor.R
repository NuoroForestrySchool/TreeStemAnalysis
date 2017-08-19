source('F_localCor.R')
x <- rep(.5,3)
y <- rep(.4,3)
r <- NULL
for (y2 in seq(.3, .5, .02)) {
  y[2] <- y2
  r <- c(r, localCor(x,y)[2])
}
plot(r)

