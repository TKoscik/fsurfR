curv.hdr <- function(curv.file, field="all") {
  
  stopifnot(file.exists(curv.file))
  
  fid <- file(curv.file, "rb")
  endian <- "big"
  
  # Find version ---------------------------------------------------------------
  invisible(seek(fid, 0, "start", "rb"))
  b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  version <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
  
  if (version == 16777215) {
    version <- "new"
  } else {
    version <- "old"
  }
  
  hdr <- list()

  if (field[1] == "all") {
    if (version == "new") {
      field <- c("version",
                 "num.vertex",
                 "num.face",
                 "num.values")
    } else if (version == "old") {
      field <- c("version",
                 "num.vertex",
                 "num.face")
    }
  }
  
  if ("version" %in% field) {
    hdr$version <- version
  }

  if (version == "old") {
    if ("num.vertex" %in% field) {
      invisible(seek(fid, 0, "start", "rb"))
      b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      hdr$num.vertex <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    }
    if ("num.face" %in% field) {
      invisible(seek(fid, 3, "start", "rb"))
      b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
      hdr$num.face <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    }
  } else {
    if ("num.vertex" %in% field) {
      invisible(seek(fid, 3, "start", "rb"))
      hdr$num.vertex <- readBin(fid, "int", size=4, n=1, endian=endian)
    }
    if ("num.face" %in% field) {
      invisible(seek(fid, 7, "start", "rb"))
      hdr$num.face <- readBin(fid, "int", size=4, n=1, endian=endian)
    }
    if ("num.values" %in% field) {
      invisible(seek(fid, 11, "start", "rb"))
      hdr$num.values <- readBin(fid, "int", size=4, n=1, endian=endian)
    }
  }
  
  close(fid)
  return(hdr)
}