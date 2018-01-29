fsurf.var.ls <- function(rois = c("peg","all"),
                         hemi = c("t", "r", "l"),
                         vars = c("sa", "g", "ta", "mc")) {
  
  if (vars[1] == "all") {
    vars <- c("n", "sa", "g", "ta", "tsd", "mc", "gc", "fi", "ci")
  }
  
  
    rois.temp <- rois
    rois <- character()
    if ("peg" %in% rois.temp) {
      rois <- c("wb", "lh", "rh", "frnt", "par", "temp", "occ")
      rois.temp <- rois.temp[-which(rois.temp == "peg")]
    }
    if ("ell" %in% rois.temp) {
      rois <- c(rois, "cing")
      rois.temp <- rois.temp[-which(rois.temp == "ell")]
    }
    if ("all" %in% rois.temp) {
      rois <- c(rois, "cmf", "lof", "mof", "fp", "po", "pt", "rmf", "sf", "parac", "prec", "pp", "cac", "rac", "ins",
                "postc", "ip", "sp", "pcun", "sm", "pc", "ic",
                "it", "mt", "st", "tt", "ph", "fus", "bsts", "er", "tp",
                "cun", "lo", "ling", "peric")
      rois.temp <- rois.temp[-which(rois.temp == "all")]
    }
    
    if ("frnt.roi" %in% rois.temp) {
      rois <- c(rois, "cmf", "lof", "mof", "fp", "po", "pt", "rmf", "sf", "parac", "prec", "pp", "cac", "rac", "ins")
      rois.temp <- rois.temp[-which(rois.temp == "frnt.roi")]
    }
    
    if ("par.roi" %in% rois.temp) {
      rois <- c(rois, "postc", "ip", "sp", "pcun", "sm", "pc", "ic")
      rois.temp <- rois.temp[-which(rois.temp == "par.roi")]
    }
    
    if ("temp.roi" %in% rois.temp) {
      rois <- c(rois, "it", "mt", "st", "tt", "ph", "fus", "bsts", "er", "tp")
      rois.temp <- rois.temp[-which(rois.temp == "temp.roi")]
    }
    
    if ("occ.roi" %in% rois.temp) {
      rois <- c(rois, "cun", "lo", "ling", "peric")
      rois.temp <- rois.temp[-which(rois.temp == "occ.roi")]
    }
    
    rois <- c(rois, rois.temp)
    rois <- unique(rois)
  
    var.names <- character()
    for (j in 1:length(rois)) {
      for (i in 1:length(hemi)) {
        for (k in 1:length(vars)) {
          if (rois[j] %in% c("wb", "lh", "rh")) {
            var.names <- c(var.names, paste(rois[j], vars[k], sep="_"))
          } else {
            var.names <- c(var.names, paste(rois[j], hemi[i], vars[k], sep="_"))
          }
        }
      }
    }
    var.names <- unique(var.names)
    return(var.names)
}