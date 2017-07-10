# Get the data
filen <- "AdF_Pinus.pinaster_Pattada.Rdata"
if (file.exists(filen)) DB <- filen else 
  {
  library(RCurl)
  library(foreign)
  giturl <- "https://github.com/NuoroForestrySchool/TreeStemAnalysis/raw/master/"
  ourl <- paste(giturl, filen, sep="")
  if (!url.exists(ourl)) stop("ERROR: DATI non trovati!")
  DB <- url(ourl)
}
load(DB)

source("Fusti.R")
source("Sezioni.R")
source("Palchi.R")
source("CongruenzaAnelliPalchi.R")
