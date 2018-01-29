read.fsurf.stats <- function(stats.file) {
  
  fid <- file(stats.file, "r")
  x <- substr(readLines(stats.file, -1), 1, 1)
  skip.lines <- which(x == "b") - 1
  
  data <- read.delim(stats.file, header=F, sep="", skip=skip.lines)
  close(fid)
  return(data)
}