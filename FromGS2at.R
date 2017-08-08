## Transform basic GS tables in analysis ready tables
##   and compute Sectors systesis

library(sqldf, quietly=F)
Sects_at <- Sects0
Diams_at <- Diams0

# In Sects_at: add numeric section height attribute
x <- as.numeric(Sects_at$s_height)
Sects_at$s_height_n <- ifelse(is.na(x), 0, x) 

# In Sects_at: add tree level progressive SectionId
if (!('SectId' %in% names(Sects_at))) {
  Sects_at <- sqldf("select A.*, count(*) SectId from Sects_at A join Sects_at B on A.TreeId=B.TreeId and A.S_height_n >= B.s_height_n group by A.TreeId, A.row order by A.TreeId, A.S_height_n")
}

# In Diams_at: add SectId reference
if (!('SectId' %in% names(Diams_at))) {
  Diams_at <- sqldf("select A.*, SectId from Diams_at A left join Sects_at B using(row)")
}

# Transfer 'comments' from Diams_at to Sects_at
Sects_comments <- sqldf("select row, col, TreeId, SectId, comment from Diams_at where value is NULL")
Diams_at <- sqldf("select * from Diams_at where value is not NULL")
comments_ok <- nrow(unique(Sects_comments[,1:2])) == nrow(Sects_comments[,1:2])
if (!comments_ok) {
  cat("*** there is a problem, 'comment' is expected to be unique for each section")
  stop()
}
if (!('comment' %in% names(Sects_at))) {
  Sects_at <- sqldf("select A.*, B.comment from Sects_at A natural left join Sects_comments B")
}

# In Diams_at: distinguish measurements before (a) and after (b) the pith
if (!('side' %in% names(Diams_at))) {
  Diams_at <- sqldf("select A.*, case when col <= a_max then 'a' else 'b' end side from Diams_at A natural join (select TreeId, SectId, min(col) a_max from (select * from Diams_at A where red) group by TreeId, SectId) B ")
}

# In Diams_at: add 'year of growth', select and reorder attributes
cutting_year <- 2017
if (!('year' %in% names(Diams_at))) {
  # col expresses the progression of measurements, to get the actual year 
  # consider following cases:
  # A1 - section starts with bold: 1st measuremen is under bark, year=cutting year
  # A2 - section starts without bold: 1st measuremen is under first ring, year=c.year-1
  # B - till pith (first red) year decreases
  # C - for 1st measurements after pith (2nd red) year = year of 'first red'
  # D - for successive measurements year increases
  
  # for side = 'a', year = cutting_year - (col - min_col)
  # if 1st meas. is bark, meas. includes cutting year growth, year = c.year
  # else, 1st meas. excludes cutting year, year = c.year -1
  tmp <- sqldf(" select TreeId, SectId, side, case when side='a' and not A.bold then A.col-1 else a.col end min_col from Diams_at A natural join (select TreeId, SectId, side, min(col) col from Diams_at group by TreeId, SectId, side) B ")
  tmp <- sqldf(" select A.*, col-min_col ry from Diams_at A natural join tmp B ")
  tmp[tmp$side=='a','year'] <- cutting_year - tmp[tmp$side=='a','ry']
  
  # process side = 'b'
  tmp <- sqldf("select A.*, min_year from tmp A natural left join (select TreeId, SectId, min(year) min_year from (select * from tmp where side='a') group by TreeId, SectId) B ")
  tmp[tmp$side=='b','year'] <- tmp[tmp$side=='b','min_year'] + tmp[tmp$side=='b','ry']
  tmp[tmp$side=='b' & tmp$bold,'year'] <- NA
  Diams_at <- sqldf("select row, col, TreeId, SectId, side, year, bold=1 bark, value from tmp")
}

# compute Sections synthesis
Sects_sy <- sqldf("select TreeId, SectId
                  , sum(side='a' and bark) abark, sum(side='a' and not bark) nam
                  , sum(side='b' and not bark) nbm, sum(side='b' and bark) bbark, count(*) as nm 
                  from Diams_at group by TreeId, SectId order by TreeId, SectId")
Sects_sy <- sqldf("select A.*, B.comment 
                  from Sects_sy A natural left join Sects_at B")
Sects_sy$complete <- Sects_sy$bbark | is.na(Sects_sy$comment)
Sects_sy <- sqldf("select * from Sects_sy natural join (select TreeId, SectId, max(year) max_b_year
                  from (select * from Diams_at where side='b') group by TreeId, SectId)")
