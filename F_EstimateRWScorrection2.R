EstimateRWScorrection2 <- function(ls, ss, year1, p_span=7, test_year = NULL) {
  N <- length(ls)
  if (length(ss) != N-1) stop(paste("Length difference NOT = 1 - ls =", length(ls), "ss =", length(ss)))
  if (N <= p_span)  cat("Warning: N smaller than 'p_span'")
  ls_ly <- year1 + N -1
  ss_ly <- ls_ly - 1
  csim <- data.frame()
    #  (partial) cumulated differences/correlations squared
  for (y in year1:ss_ly) { 
      # compute cumulative 'similarity' having I and D in position y
    
      # Insert (before y, splitting rw[y] in two, shifting +1 following years) - in ss
      ssI <- EditTreeRingWidthSeries('I', y, ss, year1)
    
      # Delete (ring y: sum j and j+1, till N-1) from long series
      lsD <- EditTreeRingWidthSeries('D', y, ls, year1)
      
      ln <- 3
      csim <- rbind(csim, 
          data.frame(proposed_e = 'I', edit_y=y, crit ='cd', c_value = sum((ls-ssI)^2) / length(ls)),
          data.frame(proposed_e = 'D', edit_y=y, crit ='cd', c_value = sum((lsD-ss)^2) / length(lsD)),
          data.frame(proposed_e = 'I', edit_y=y, crit ='cr', c_value = sum((localCor(ls, ssI, ln)+1)/2) / length(ls)),
          data.frame(proposed_e = 'D', edit_y=y, crit ='cr', c_value = sum((localCor(lsD, ss, ln)+1)/2) / length(lsD))
                  )
      if (anyNA(csim)) stop(paste(" NA in 'csim', year =", y))
  }
  
  myf <- function(tab, span) {
    sel <- which(peaks(tab$c_value,span=p_span))
    tmp <- data.frame()
    if (length(sel)==0) sel <- which.max(tab$c_value)
    order <- 1:min(length(sel),2)
    tmp <- data.frame(order=order
                      , edit_y=tab$edit_y[sel[order(tab$c_value[sel], decreasing = T)][order]]
                      , c_value=tab$c_value[sel[order(tab$c_value[sel], decreasing = T)][order]]
    )
    return(tmp)
    # for each criterion, select row (r) of best and, If possible, second best modification 
  }
  pcsim <- ddply(csim, .(proposed_e, crit), myf, span=7)
  return(pcsim)
}
