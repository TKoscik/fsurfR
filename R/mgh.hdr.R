mgh.hdr <- function(mgh.file, field = "all") {
  
  stopifnot(file.exists(mgh.file))
  
  fid <- file(mgh.file, "rb")
  endian <- "big"
  
  if (field[1] == "all") {
    field <- c("version",
               "width", "height", "depth", "nframes",
               "type",
               "dof", "goodRASFlag",
               "spacing",
               "x_ras",
               "y_ras",
               "z_ras",
               "c_ras")
  }
  
  hdr <- list()
  
  if ("version" %in% field) {
    invisible(seek(fid, 0, "start", "rb"))
    hdr$version <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("width" %in% field) {
    invisible(seek(fid, 4, "start", "rb"))
    hdr$width <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("height" %in% field) {
    invisible(seek(fid, 8, "start", "rb"))
    hdr$height <- readBin(fid, integer(), n=1, endian=endian)
  }
  if ("depth" %in% field) {
    invisible(seek(fid, 12, "start", "rb"))
    hdr$depth <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("nframes" %in% field) {
    invisible(seek(fid, 16, "start", "rb"))
    hdr$nframes <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("type" %in% field) {
    invisible(seek(fid, 20, "start", "rb"))
    hdr$type <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("dof" %in% field) {
    invisible(seek(fid, 24, "start", "rb"))
    hdr$dof <- readBin(fid, "int", n=1, endian=endian)
  }
  if ("goodRASFlag" %in% field) {
    invisible(seek(fid, 28, "start", "rb"))
    hdr$goodRASFlag <- readBin(fid, integer(), size=2, n=1, endian=endian)
  }
  if ("spacing" %in% field) {
    invisible(seek(fid, 30, "start", "rb"))
    hdr$spacing <- readBin(fid, double(), size = 4, n=3, endian=endian)
  }
  if ("x_ras" %in% field) {
    invisible(seek(fid, 42, "start", "rb"))
    hdr$x_ras <- readBin(fid, double(), size = 4, n=3, endian=endian)
  }
  if ("y_ras" %in% field) {
    invisible(seek(fid, 54, "start", "rb"))
    hdr$y_ras <- readBin(fid, double(), size = 4, n=3, endian=endian)
  }
  if ("z_ras" %in% field) {
    invisible(seek(fid, 66, "start", "rb"))
    hdr$z_ras <- readBin(fid, double(), size = 4, n=3, endian=endian)
  }
  if ("c_ras" %in% field) {
    invisible(seek(fid, 78, "start", "rb"))
    hdr$c_ras <- readBin(fid, double(), size = 4, n=3, endian=endian)
  }
  if (length(hdr) == 1) {
    hdr <- hdr[[1]]
  }
  return(hdr)
  close(fid)
}