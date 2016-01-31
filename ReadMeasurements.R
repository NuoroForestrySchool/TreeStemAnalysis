# Get the data
library(RCurl)
library(foreign)
giturl <- "https://github.com/NuoroForestrySchool/TreeStemAnalysis/raw/master/"
filen <- "AdF_Pinus.pinaster_Pattada.Rdata"
ourl <- paste(giturl, filen, sep="")
url.exists(ourl)
load(url(ourl))
