## ---- echo=FALSE, results='hide'-----------------------------------------
os <- Sys.info()['sysname']
if(os == 'Windows') {
  old_loc <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE","Chinese")
}
library("sinx")
library("utils")
f <- sinx:::merge_text(method = 'vig')

## ---- results='asis', echo=FALSE-----------------------------------------
cat(f$vig, sep = '\n\n\n\n')

## ---- echo=FALSE, results='hide'-----------------------------------------
if(os == 'Windows') {
  Sys.setlocale("LC_CTYPE",old_loc)
}

