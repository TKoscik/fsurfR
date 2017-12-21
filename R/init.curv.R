init.curv <- function(file.name, surf.file) {
  
  stopifnot(!file.exists(file.name))
  
  surf.info <- read.surf(surf.file)
  
  # Initialize file connection -------------------------------------------------
  fid <- file(file.name, "w+b")
  endian <- "big"
  
  # Write Magic Number for File Version
  writeBin(bitwAnd(bitwShiftR(16777215, 16), 255), fid, size=1, endian=endian)
  writeBin(bitwAnd(bitwShiftR(16777215, 8), 255), fid, size=1, endian=endian)
  writeBin(bitwAnd(16777215, 255), fid, size=1, endian=endian)
  
  writeBin(surf.info$n.vertex, fid, size=4, endian=endian)  # Number of vertices
  writeBin(surf.info$n.face, fid, size=4, endian=endian)    # Number of faces
  writeBin(1L, fid, size=4, endian=endian)                    # Number of values
  writeBin(rep(NA, surf.info$n.vertex), fid, size=4, endian=endian) # Initialize empty bytes
  
  close(fid)
}