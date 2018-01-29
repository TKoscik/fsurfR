read.fsurf.stats <- function(stats.file) {
  
  fid <- file(stats.file, "r")
  x <- substr(readLines(stats.file, -1), 1, 1)
  skip.lines <- which(x == "b") - 1
  
  data <- read.delim(stats.file, header=F, sep="", skip=skip.lines)
  
  row.chk <- c("bankssts","caudalanteriorcingulate","caudalmiddlefrontal",
               "cuneus","entorhinal","fusiform","inferiorparietal",
               "inferiortemporal","isthmuscingulate","lateraloccipital",
               "lateralorbitofrontal","lingual","medialorbitofrontal",
               "middletemporal","parahippocampal","paracentral",
               "parsopercularis","parsorbitalis","parstriangularis",
               "pericalcarine","postcentral","posteriorcingulate","precentral",
               "precuneus","rostralanteriorcingulate","rostralmiddlefrontal",
               "superiorfrontal","superiorparietal","superiortemporal",
               "supramarginal","frontalpole","temporalpole",
               "transversetemporal","insula")
  
  data.chk <- data.frame(region=row.chk,
                         matrix(as.numeric(NA), nrow=length(row.chk), ncol=9))
  colnames(data.chk) <- c("region", "n", "sa", "g", "ta", "tsd", "mc", "gc", "fi", "ci")
  for (i in 1:nrow(data)) {
    data.chk[which(row.chk == data[i,1]), ] <- data[i, ]
  }
  
  close(fid)
  return(data)
}