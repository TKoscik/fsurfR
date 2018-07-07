write.curv.vertex <- function(curv.file, n, values, surf.file=NULL, coords=NULL) {
  
  stopifnot(file.exists(curv.file))
  
  if (!is.null(surf.file)) {
    stopifnot(file.exists(surf.file), stopifnot(!is.null(coords)))
  }
  
  if (!is.null(surf.file)) {
    surf.info <- read.surf(surf.file)
    coords <- as.matrix(coords, ncol=3, by.row=TRUE)
    x <- surf.info$vertex[ ,1] %in% coords[ ,1]
    y <- surf.info$vertex[ ,2] %in% coords[ ,2]
    z <- surf.info$vertex[ ,3] %in% coords[ ,3]
    n <- which(as.logical(x * y * z))
  }
  
  hdr <- curv.hdr(curv.file, "all")
  
  if (!is.null(surf.file)) {
    surf.info <- read.surf(surf.file)
    
    coords <- as.matrix(coords, ncol=3, by.row=TRUE)
    
    x <- surf.info$vertex[ ,1] %in% coords[ ,1]
    y <- surf.info$vertex[ ,2] %in% coords[ ,2]
    z <- surf.info$vertex[ ,3] %in% coords[ ,3]
    n <- which(as.logical(x * y * z))
  }
  
  if (n == "all") {
    n <- 1:hdr$num.vertex
    write.all <- TRUE
    stopifnot(length(values) == hdr$num.vertex)
  } else {
    write.all <- FALSE
  }
  stopifnot(all(n <= hdr$num.vertex))
  
  fid <- file(curv.file, "r+b")
  endian <- "big"
  
  if (hdr$version == "new") {
    hdr.offset <- 15
    n.bytes <- 4
  } else if (hdr$version == "old") {
    hdr.offset <- 6
    n.bytes <- 2
  }
  
  if (write.all) {
    invisible(seek(fid, hdr.offset, origin="start", rw="write"))
    writeBin(values, fid, size=4, endian=endian)
  } else {
    for (i in 1:length(n)) {
      invisible(seek(fid, hdr.offset+n.bytes*n[i], origin="start", rw="write"))
      writeBin(values[i], fid, size=4, endian=endian)
    }
  }
  
  close(fid)
}
