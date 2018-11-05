get.curv.list <- function(data.dir,
                          id.ls,
                          hemi,
                          var.name) {
  outls <- character(length(id.ls))
  fls <- character(0)
  for (i in 1:length(data.dir)) {
    fls <- c(fls, list.files(data.dir[i], pattern=hemi, full.names = TRUE))
  }
  fls <- fls[grepl(pattern = var.name, fls)]
  for (i in 1:length(id.ls)) {
    fname <- fls[grepl(pattern=id.ls[i], fls)]
    if (length(fname) == 0) {
      stop(sprintf("Error: Could not find file for %s", as.character(id.ls[i])))
    } else if (length(fname) > 1) {
      stop(sprintf("Error: Found multiple files for %s", as.character(id.ls[i])))
    } else {
      outls[i] <- fname
    }
  }
  return(outls)
}
