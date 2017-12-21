init.mgh <- function(file.name,
                     dims.whdn,
                     type = c(0,1,3,4),
                     dof = 0,
                     goodRASFlag = 1,
                     spacing = c(1,1,1),
                     x_ras = c(-1,0,0),
                     y_ras = c(0,0,-1),
                     z_ras = c(0,1,0),
                     c_ras = c(0,0,0)) {
  
  stopifnot(!missing(file.name), !missing(dims.whdn), any(length(dims.whdn)==3, length(dims.whdn)==4))
  stopifnot(!file.exists(file.name))
  
  if (length(dims.whdn)==3) { dims.whdn <- c(dims.whdn, 1) }
  if (length(type) != 1) { type <- 3 }
  
  fid <- file(file.name, "w+b")
  
  writeBin(1L, fid, size=4, endian="big")                                       # version
  writeBin(dims.whdn[1], fid, size=4, endian="big")                             # width
  writeBin(dims.whdn[2], fid, size=4, endian="big")                             # height
  writeBin(dims.whdn[3], fid, size=4, endian="big")                             # depth
  writeBin(dims.whdn[4], fid, size=4, endian="big")                             # nframe
  writeBin(type, fid, size=4, endian="big")                                     # image datatype code
  writeBin(dof, fid, size=4, endian="big")                                      # ndof
  writeBin(goodRASFlag, fid, size=2, endian="big")                              # indicates if directional cosines are in header, if not assume coronal
  writeBin(spacing, fid, size=4, endian="big")                                  # spacing
  writeBin(x_ras, fid, size=4, endian="big")                                    # x_ras
  writeBin(y_ras, fid, size=4, endian="big")                                    # x_ras
  writeBin(z_ras, fid, size=4, endian="big")                                    # x_ras
  writeBin(c_ras, fid, size=4, endian="big")                                    # x_ras
  writeBin(rep(0L, 206), fid, size=4, endian="big")                             # pad header to 284 length
  
  # Write blank volumes
  nbytes <- switch(as.character(type),
                   '0' = 1, '1' = 4, '3' = 4, '4' = 2,
                   stop("Unknown data type"))
  data <- array(NA, dim=dims.whdn[1:3])
  for (i in 1:dims.whdn[4]) {
    writeBin(data, fid, size = nbytes)
  }
  
  close(fid)
}