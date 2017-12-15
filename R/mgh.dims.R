mgh.dims <- function(mgh.file) {
  
  stopifnot(file.exists(mgh.file))
  
  hdr <- mgh.hdr(mgh.file, c("width", "height", "depth", "nframes"))
  
  dims <- c(hdr$width, hdr$height, hdr$depth, hdr$nframes)
  # if (dims[4] == 0) {
  #   dims <- dims[-4]
  # }
  return(dims)
}