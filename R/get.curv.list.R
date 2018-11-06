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
    if (!is.na(id.ls[i])) {
      fname <- fls[grepl(pattern=id.ls[i], fls)]
      if (length(fname) == 0) {
        print(sprintf("Error: Could not find file for %s", as.character(id.ls[i])))
        outls[i] <- NA
      } else if (length(fname) > 1) {
        print(sprintf("Error: Found multiple files for %s, using first found entry.", as.character(id.ls[i])))
        outls[i] <- fname[1]
      } else {
        outls[i] <- fname
      }
    } else {
      print("Error: ID is NA")
        outls[i] <- NA
    }
  }
  return(outls)
}
