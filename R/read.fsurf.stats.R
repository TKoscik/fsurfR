read.fsurf.stats <- function(stats.file) {
  
  fid <- file(stats.file, "r")
  x <- substr(readLines(stats.file, -1), 1, 1)
  skip.lines <- max(which(x == "#"))
  
  data <- read.delim(stats.file, header=F, sep="", skip=skip.lines, as.is = TRUE)
  
  type.chk <- unlist(strsplit(stats.file, "[.]"))
  if (type.chk[length(type.chk)-1] == "aparc" ) {
    type.chk <- 1
  } else { type.chk <- 0 }
  
  if (type.chk) {
    row.chk <- c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal",
                 "cuneus", "entorhinal", "fusiform", "inferiorparietal",
                 "inferiortemporal", "isthmuscingulate", "lateraloccipital",
                 "lateralorbitofrontal", "lingual", "medialorbitofrontal",
                 "middletemporal", "parahippocampal", "paracentral",
                 "parsopercularis", "parsorbitalis", "parstriangularis",
                 "pericalcarine", "postcentral", "posteriorcingulate",
                 "precentral", "precuneus", "rostralanteriorcingulate",
                 "rostralmiddlefrontal", "superiorfrontal", "superiorparietal",
                 "superiortemporal", "supramarginal", "frontalpole",
                 "temporalpole", "transversetemporal","insula")
    data.chk <- data.frame(region=row.chk,
                           matrix(as.numeric(NA), nrow=length(row.chk), ncol=9))
    colnames(data.chk) <- c("region", "n", "sa", "g", "ta", "tsd", "mc", "gc", "fi", "ci")
    for (i in 1:nrow(data)) {
      data.chk[which(row.chk == data[i,1]), ] <- data[i, ]
    }
    
    close(fid)
    return(data.chk)
    
  } else {
    row.chk <- c("Left-Lateral-Ventricle", "Left-Inf-Lat-Vent",
                 "Left-Cerebellum-White-Matter", "Left-Cerebellum-Cortex",
                 "Left-Thalamus-Proper", "Left-Caudate", "Left-Putamen",
                 "Left-Pallidum", "3rd-Ventricle", "4th-Ventricle",
                 "Brain-Stem", "Left-Hippocampus", "Left-Amygdala", "CSF",
                 "Left-Accumbens-area", "Left-VentralDC", "Left-vessel",
                 "Left-choroid-plexus", "Right-Lateral-Ventricle",
                 "Right-Inf-Lat-Vent", "Right-Cerebellum-White-Matter",
                 "Right-Cerebellum-Cortex", "Right-Thalamus-Proper",
                 "Right-Caudate", "Right-Putamen", "Right-Pallidum",
                 "Right-Hippocampus", "Right-Amygdala", "Right-Accumbens-area",
                 "Right-VentralDC", "Right-vessel", "Right-choroid-plexus",
                 "5th-Ventricle", "WM-hypointensities",
                 "Left-WM-hypointensities", "Right-WM-hypointensities",
                 "non-WM-hypointensities", "Left-non-WM-hypointensities",
                 "Right-non-WM-hypointensities", "Optic-Chiasm", "CC_Posterior",
                 "CC_Mid_Posterior", "CC_Central", "CC_Mid_Anterior",
                 "CC_Anterior")
    
    data.chk <- data.frame(matrix(as.numeric(NA), nrow=length(row.chk), ncol=4),
                           matrix(as.character(NA), nrow=length(row.chk), ncol=1),
                           matrix(as.numeric(NA), nrow=length(row.chk), ncol=5),
                           stringsAsFactors = FALSE)
    colnames(data.chk) <- c("ex1", "ex2", "n", "v", "region",
                            "nm", "nsd", "nmin", "nmax", "nrng")
    for (i in 1:nrow(data)) {
      data.chk[which(row.chk == data[i,5]), ] <- data[i, ]
    }
    
    close(fid)
    return(data.chk)
  }
  
 
}