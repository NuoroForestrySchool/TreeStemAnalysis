EstimateRWScorrection <- function(ls, ss, year1, p_span=7) {
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
                    data.frame(modify_y=y
                             , cdI = sum((ls-ssI)^2) / length(ls)
                             , cdD = sum((lsD-ss)^2) / length(lsD)
                             , crI = sum((localCor(ls, ssI, ln)+1)/2) / length(ls)
                             , crD = sum((localCor(lsD, ss, ln)+1)/2) / length(lsD)
                  ))
    }
   pcsim_i <- data.frame()
   for (c in names(csim)[2:5]) {
     sel <- which(peaks(csim[,c],span=p_span))
     pcsim_i <- rbind(pcsim_i, data.frame(c=c, o=c(1,2)
                                         , r=sel[order(csim[,c][sel], decreasing = T)][1:2]))
     # for each criterion, select row (r) of best and second best modification 
   }
   csim$how <- ''
   for (i in 1:nrow(pcsim_i)) {
     csim$how[pcsim_i$r[i]] <- 
       paste(csim$how[pcsim_i$r[i]], 
            paste(pcsim_i$c[i], pcsim_i$o[i], sep=""), sep=":")
     # for years of best maodification, add note on which modification 'how' 
   }
   return(csim[csim$how != '',])
}
