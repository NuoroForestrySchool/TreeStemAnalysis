
EditTreeRingWidthSeries <- function(edit, year, rws, rwyear1, random=F) {
  # Insert or Delete tree ring corresponding to 'year'

  if (!edit %in% c("I", "D")) {
    stop("'edit' has to be either 'I' for Insert or 'D' for delete!")
  }
  rwys <- rwyear1:(rwyear1+length(rws)-1)
  if (year < rwyear1 | year > max(rwys)-ifelse(edit=='D',1,0)) {
    stop(paste("'year'=",year,"is out of range!"))
  }
  if (edit == "I") {
    # Insert a ring in rws, before year, splitting rws[year] in two [random] parts
    #  output: rwso[year] = part1,  rwso[year+1] = part2, rwso[year+2] = rws[year+1]
    rwyso <- c(rwys, rwys[length(rwys)]+1)
    rwso <- c(rws[rwys <= year+1], rws[rwys > year])
    rwso[rwyso == year] <- rws[rwys == year] * ifelse(random, runif(), .5)
    rwso[rwyso == year+1] <- rws[rwys == year] - rwso[rwyso == year]
  } else if (edit == "D") {
    # Delete ring for 'year', sum rw for year and year+1
    rwyso <- rwys[-length(rwys)]
    rwso <- c(rws[rwys <= year], rws[rwys > year+1])
    rwso[rwyso==year] <- rws[rwys==year] + rws[rwys==year+1]
  }
  return(rwso)
}

