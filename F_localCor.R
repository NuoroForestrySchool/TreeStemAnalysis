localCor <- function(x, y, n=3) {
  if (n%%2==0) {
    stop("'n' must be odd")
  }
  if (class(x)!="numeric" | class(y)!="numeric") {
    stop('x and y must be "numeric"')
  }
  if (sum(is.na(x)) + sum(is.na(y)) > 0) {
    stop('no NA or NaN admitted!')
  }
#x <- c(7, 2, 4, 7, 2)
#y <- jitter(x)
  l <- length(x)
  if (l < n | length(y)!=l) {
    stop("length of 'x' and 'y' must be equal and greather than 'n'")
  }
  pad <- (n-1)/2
  x <- c(x[-(1:(l-pad))], x, x[1:pad])
  y <- c(y[-(1:(l-pad))], y, y[1:pad])
  almostZero <- 10 ^ (1 + ceiling(log10(.Machine$double.eps)))
  result <- rep_len(NA, l)
  for (j in (1+pad):(l+pad)) {
    lx <- x[(j-pad):(j+pad)]
    ly <- y[(j-pad):(j+pad)]
    dx <- diff(range(lx[is.finite(lx)]))
    dy <- diff(range(ly[is.finite(ly)]))
    rs <- FALSE
    if (dx < almostZero & dy < almostZero) {
      cr <- 1
    } else if (dy < almostZero) {
      cv <- sd(lx)/mean(lx)
      rs <- TRUE
    } else if (dx <  almostZero) {
      cv <- sd(ly)/mean(ly)
      rs <- TRUE
    } else {
      cr <- cor(lx, ly)
    }
    # rs: ReShape --> cor range is 1-:1 - cv range 0:almost 1, with 0 => no variance
    #     if x is constant (==> dx < a.0)  0 sd on y is optimal (and exchange y with x)
    #     so sd=0 is like cor=1, to standardize we set sd/mean=1 to correspond to cor=-1
    result[j-pad] <- ifelse(rs, 1 - 2*abs(cv), cr)
#    print(xyplot(x[(j-1):(j+1)] + y[(j-1):(j+1)] ~ 1:3, t='b', main=paste(result[j])))
  }
  return(result)
}