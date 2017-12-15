write.mgh.voxel <- function(mgh.file, coords, value) {
  
  # Check inputs ---------------------------------------------------------------
  stopifnot(!missing(mgh.file), file.exists(mgh.file),
            !missing(coords), !missing(value))
  
  # Read necessary info from header --------------------------------------------
  dims <- mgh.dims(mgh.file)
  type <- mgh.hdr(mgh.file, "type")
  nbytes <- switch(as.character(type),
                   '0' = 1, '1' = 4, '3' = 4, '4' = 2,
                   stop("Unknown data type"))
  voxoffset <- 284
  
  # Check if coordinates are in range ------------------------------------------
  for (i in 1:length(coords)) {
    stopifnot(coords[i] <= dims[i])
  }
  
  # Find write location --------------------------------------------------------
  n <- length(coords)
  dimorder <- 1:n
  coords <- matrix(coords, nrow=1, ncol=length(coords))
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  loc <- as.integer(colSums(t(coords[ , , drop=FALSE]-1) * cdim) + 1L)
  
  # Initialize file connection -------------------------------------------------
  fid <- file(mgh.file, "r+b")
  endian <- "big"
  
  # Move to correct file position ----------------------------------------------
  seek(fid, where = (voxoffset + (loc-1) * nbytes), origin = "start", rw="write")
  
  # Write value ----------------------------------------------------------------
  writeBin(value, fid, size=nbytes, endian=endian)
  
  close(fid)
}