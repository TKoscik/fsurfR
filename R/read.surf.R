read.surf <- function(surf.file, which.vertex, which.face) {
  
  stopifnot(file.exists(surf.file))
  
  fid <- file(surf.file, "rb")
  endian <- "big"
  
  surf.info <- list()
  
  # Find version ---------------------------------------------------------------
  invisible(seek(fid, 0, "start", "rb"))
  b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
  version <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
  if (version == 16777214) {
    surf.info$version = "tri"
  } else if (version == 16777215) {
    surf.info$version = "quad"
  } else if (version == 16777213) {
    surf.info$version = "other_tri"
  }
  
  if (surf.info$version == "tri") {
    surf.info$created <- readLines(fid, 1)
    surf.info$info <- readLines(fid, 1)
    surf.info$n.vertex <- readBin(fid, integer(), size=4, 1, endian=endian)
    surf.info$n.face <- readBin(fid, integer(), size=4, 1, endian=endian)
    surf.info$vertex <- t(matrix(readBin(fid, numeric(), size=4, surf.info$n.vertex*3, endian=endian), nrow=3))
    surf.info$face <- t(matrix(readBin(fid, integer(), size=4, surf.info$n.face*3, endian=endian), nrow=3))
  } else if (surf.info$version == "other_tri") {
    b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    surf.info$n.vertex <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    
    b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    surf.info$n.face <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    
    surf.info$vertex <- t(matrix(readBin(fid, numeric(), size=4, surf.info$n.vertex*3, endian=endian), nrow=3))
    face.temp <- t(matrix(readBin(fid, integer(), size=1, surf.info$n.face*3*3, endian=endian), nrow=3))
    face.temp[ ,1] <- bitwShiftL(face.temp[ ,1],16)
    face.temp[ ,2] <- bitwShiftL(face.temp[ ,2],8)
    surf.info$face <- matrix(rowSums(face.temp), ncol=3, byrow=T)
  } else if (surf.info$version == "quad") {
    b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    surf.info$n.vertex <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    
    b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
    surf.info$n.face <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
    
    surf.info$vertex <- t(matrix(readBin(fid, numeric(), size=2, surf.info$n.vertex*3, endian=endian), nrow=3)) / 100
    
    surf.info$face <- matrix(0, nrow=surf.info$n.face, ncol=4)
    for (i in 1:surf.info$n.face){
      for (j in 1:4) {
        b1 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
        b2 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
        b3 <- readBin(fid, "int", size=1, n=1, signed=FALSE, endian=endian)
        surf.info$face[i,j] <- bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3
      }
    }
  }
  
  close(fid)
  
  surf.info$face <- surf.info$face + 1 # for Matlab compatibility (Is this necessary for R as well?)
  return(surf.info)
}