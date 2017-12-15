read.mgh.voxel <- function(mgh.file, coords) {
  
  stopifnot(!missing(mgh.file), file.exists(mgh.file),
            !missing(coords), any(length(coords)==3, length(coords)==4))
  
  # Read necessary info from header --------------------------------------------
  dims <- mgh.dims(mgh.file)
  type <- mgh.hdr(mgh.file, "type")
  nbytes <- switch(as.character(type), '0' = 1, '1' = 4, '3' = 4, '4' = 2,
                   stop("Unknown data type"))
  
  # Check if coordinates ar in range -------------------------------------------
  for (i in 1:length(coords)) {
    if (i < 4) {
      stopifnot(coords[i] <= dims[i])
    } else if (!is.infinite(coords[i])) {
      stopifnot(coords[i] <= dims[i])
    }
  }
  
  # Initilie MGH file for reading ----------------------------------------------
  fid <- file(mgh.file, "rb")
  endian <- "big"
  vox_offset <- 284
  
  # Get data -------------------------------------------------------------------
  n <- length(dims)
  dimorder <- 1:4
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  if (length(coords) == 4 & is.infinite(coords[4])) {
    t.count <- 1:dims[4]
  } else if (length(coords) == 4) {
    t.count <- coords[4]
  } else {
    t.count <- 1
  }
  data <- numeric(length(t.count))
  counter <- 0
  for (i in t.count) {
    counter <- counter + 1
    coord <- matrix(c(coords[1:3],i), nrow=1)
    loc <- as.integer(colSums(t(coord[ ,1:4, drop=FALSE]-1) * cdim) + 1L)
    seek(fid, where=(vox_offset + (loc-1) * nbytes), origin="start")
    # seek(fid, where=-1 * ((256*256*256)-(loc+1)*nbytes), origin="end")
    data[counter] <- readBin(fid, "int", size = nbytes, n=1, endian=endian)
  }
  close(fid)
  return(data)
}