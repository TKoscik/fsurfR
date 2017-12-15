read.mgh.volume <- function(mgh.file, vol.num) {
  
  # Check Inputs ---------------------------------------------------------------
  stopifnot(!missing(mgh.file), file.exists(mgh.file),
            !missing(vol.num), is.numeric(vol.num), length(vol.num)==1)
  
  # Read necessary info from header --------------------------------------------
  dims <- mgh.dims(mgh.file)
  type <- mgh.hdr(mgh.file, "type")
  nbytes <- switch(as.character(type),
                   '0' = 1, '1' = 4, '3' = 4, '4' = 2,
                   stop("Unknown data type"))
  voxoffset <- 284
  
  # Check if volume number in range --------------------------------------------
  stopifnot(vol.num <= dims[4])
  
  # Find write location --------------------------------------------------------
  n <- length(coords)
  dimorder <- 1:n
  cdim <- cumprod(c(1, dims[dimorder][-n]))
  coord <- matrix(c(1,1,1, vol.num), nrow=1)
  loc <- as.integer(colSums(t(coord[ , , drop=FALSE]-1) * cdim) + 1L)
  
  # Initialize file connection -------------------------------------------------
  fid <- file(mgh.file, "rb")
  endian <- "big"
  
  # Move to correct file position ----------------------------------------------
  seek(fid, where = (voxoffset + (loc-1) * nbytes), origin = "start", rw="write")
  
  # Read values ----------------------------------------------------------------
  data <- switch(
    as.character(type),
    '0' = readBin(fid, "int", size = nbytes, n=prod(dims[1:3]), endian=endian, signed=FALSE),
    '1' = readBin(fid, "int", size = nbytes, n=prod(dims[1:3]), endian=endian),
    '3' = readBin(fid, double(), size = nbytes, n=prod(dims[1:3]), endian=endian),
    '4' = readBin(fid, "int", size = nbytes, n=prod(dims[1:3]), endian=endian),
    stop("Unknown data type"))
  data <- array(data, dim=dims[1:3])
  
  close(fid)
  return(data)
}