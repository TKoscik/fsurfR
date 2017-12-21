read.curv.vertex <- function(curv.file, n="all", surf.file=NULL, coords=NULL) {
  
  stopifnot(!missing(curv.file))
  
  if (!is.null(surf.file)) { stopifnot(!is.null(coords)) }
  
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
    read.all <- TRUE
  } else {
    read.all <- FALSE
  }
  stopifnot(all(n <= hdr$num.vertex))
  
  fid <- file(curv.file, "rb")
  endian <- "big"
  
  if (hdr$version == "new") {
    hdr.offset <- 15
    n.bytes <- 4
  } else if (hdr$version == "old") {
    hdr.offset <- 6
    n.bytes <- 2
  }
  
  if (read.all) {
    invisible(seek(fid, hdr.offset, "start", "rb"))
    values <- readBin(fid, "double", size=n.bytes, n=hdr$num.vertex, endian=endian)
  } else {
    values <- numeric(length(n))
    for (i in 1:length(n)) {
      invisible(seek(fid, hdr.offset+n.bytes*n[i], "start", "rb"))
      values[i] <- readBin(fid, "double", size=n.bytes, n=1, endian=endian)
    }
  }
  
  close(fid)
  if (hdr$version == "old") {
    values <- values/100
  }
  return(values)
}