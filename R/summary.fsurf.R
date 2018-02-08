summary.fsurf <- function(data.dir,
                          vars = c("sa", "g", "ta", "mc"),
                          sub.vars = c("v"),
                          rois = c("peg", "all"),
                          sjx = "all",
                          hemi = c("t", "r", "l"),
                          save.csv=TRUE,
                          save.dir = NULL,
                          file.name = NULL,
                          return.df = FALSE) {

  if ("all" %in% sjx[1]) {
    sjx <- list.dirs(data.dir, full.names = FALSE, recursive = FALSE)
    if ("fsaverage" %in% sjx) { sjx <- sjx[-which(sjx == "fsaverage")] }
    if ("qdec" %in% sjx) { sjx <- sjx[-which(sjx == "qdec")] }
  }
  n.sjx <- length(sjx)
  
  hemi <- rev(sort(hemi))
  
  if ("all" %in% vars) {
    vars <- c("n", "sa", "g", "w", "ta", "tsd", "mc", "gc", "fi", "ci")
  }
  if ("all" %in% sub.vars) {
    sub.vars <- c("n", "v", "nm", "nsd", "nmin", "nmax", "nrng")
  }
  load.wm <- FALSE
  if ("w" %in% vars) { load.wm <- TRUE }
  
  load.sub <- FALSE
  if (any(c("peg", "ell", "all", "cx", "sub", "wm", "vnt") %in% rois)) {
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
      rois <- c(rois,
                "cmf", "lof", "mof", "fp", "po", "pt", "rmf", "sf", "parac", "prec", "pp", "cac", "rac", "ins",
                "postc", "ip", "sp", "pcun", "sm", "pc", "ic",
                "it", "mt", "st", "tt", "ph", "fus", "bsts", "er", "tp",
                "cun", "lo", "ling", "peric",
                "cgm", "thal", "caud", "put", "pall", "bs", "hpc", "amg", "acc", "vdc", 
                "cwm",  "oc", "ccp", "ccmp", "ccc", "ccma", "cca",
                "lv", "ilv", "v3", "v4", "v5", "csf", "ves", "cp", "wmhyp", "nwmhyp")
      rois.temp <- rois.temp[-which(rois.temp == "all")]
      load.sub <- TRUE
    }
    if ("cx" %in% rois.temp) {
      rois <- c(rois, "cmf", "lof", "mof", "fp", "po", "pt", "rmf", "sf", "parac", "prec", "pp", "cac", "rac", "ins",
                "postc", "ip", "sp", "pcun", "sm", "pc", "ic",
                "it", "mt", "st", "tt", "ph", "fus", "bsts", "er", "tp",
                "cun", "lo", "ling", "peric")
      rois.temp <- rois.temp[-which(rois.temp == "cx")]
    }
    if ("sub" %in% rois.temp) {
      rois <- c(rois, "cgm", "thal", "caud", "put", "pall", "bs", "hpc", "amg", "acc", "vdc")
      rois.temp <- rois.temp[-which(rois.temp == "sub")]
      load.sub <- TRUE
    }
    if ("wm" %in% rois.temp) {
      rois <- c(rois, "cwm",  "oc", "ccp", "ccmp", "ccc", "ccma", "cca")
      rois.temp <- rois.temp[-which(rois.temp == "wm")]
      load.sub <- TRUE
    }
    if ("vent" %in% rois.temp) {
      rois <- c(rois, "lv", "ilv", "v3", "v4", "v5", "csf", "ves", "cp", "wmhyp", "nwmhyp")
      rois.temp <- rois.temp[-which(rois.temp == "vent")]
      load.sub <- TRUE
    }
    rois <- c(rois, rois.temp)
    rois <- unique(rois)
  }
  
  cx.ls <- c("wb", "lh", "rh", "frnt", "par", "temp", "occ",
             "cmf", "lof", "mof", "fp", "po", "pt", "rmf", "sf", "parac", "prec", "pp", "cac", "rac", "ins",
             "postc", "ip", "sp", "pcun", "sm", "pc", "ic",
             "it", "mt", "st", "tt", "ph", "fus", "bsts", "er", "tp",
             "cun", "lo", "ling", "peric")
  sub.ls <- c("cgm", "thal", "caud", "put", "pall", "bs", "hpc", "amg", "acc", "vdc", 
              "cwm",  "oc", "ccp", "ccmp", "ccc", "ccma", "cca",
              "lv", "ilv", "v3", "v4", "v5", "csf", "ves", "cp", "wmhyp", "nwmhyp")
  if (any(sub.ls %in% rois)) { load.sub <- TRUE }
  
  
  var.names <- character()
  for (j in 1:length(rois)) {
    for (i in 1:length(hemi)) {
      if (rois[j] %in% cx.ls) {
        for (k in 1:length(vars)) {
          if (rois[j] %in% c("wb", "lh", "rh")) {
            var.names <- c(var.names, paste(rois[j], vars[k], sep="_"))
          } else {
            var.names <- c(var.names, paste(rois[j], hemi[i], vars[k], sep="_"))
          }
        }
      } else {
        for (k in 1:length(sub.vars)) {
          if (rois[j] %in% c("v3", "v4", "bs", "csf", "v5",
                             "oc", "ccp", "ccmp", "ccc", "ccma", "cca")) {
            var.names <- c(var.names, paste(rois[j], sub.vars[k], sep="_"))
          } else {
            var.names <- c(var.names, paste(rois[j], hemi[i], sub.vars[k], sep="_"))
          }
        }
      }
    }
  }
  var.names <- unique(var.names)
  
  df <- data.frame(id=sjx, matrix(as.numeric(NA), nrow=n.sjx, ncol=length(var.names)))
  colnames(df) <- c("id", var.names)
  
  for (i in 1:n.sjx) {
    run.sjx <- TRUE
    if ("t" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))) {
        lh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))
      } else {
        run.sjx <- FALSE
      }
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))) {
        rh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))
      } else {
        run.sjx <- FALSE
      }
    } else if ("l" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))) {
        lh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))
      } else {
        run.sjx <- FALSE
      }
    } else if ("r" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))) {
        rh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))
      } else {
        run.sjx <- FALSE
      }
    }
    
    if (load.sub) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/aseg.stats"))) {
        sub <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/aseg.stats"))
      } else {
        run.sjx <- FALSE
      }
    }
    
    if (load.wm) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/wmparc.stats"))) {
        wm <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/wmparc.stats"))
      } else {
        run.sjx <- FALSE
      }
    }
    
    
    if (run.sjx) {
      f.num <- c(3,11,13,31,18,19,26,27,16,23,17,2,25,34)
      p.num <- c(21,7,28,24,30,22,9)
      t.num <- c(8,14,29,33,15,6,1,5,32)
      o.num <- c(4,10,12,20)
      c.num <- c(2,9,22,25)
      
      out <- numeric()
      # Whole Brain ----
      if ("wb_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,2]) + sum(rh[ ,2]) }
      if ("wb_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3]) + sum(rh[ ,3]) }
      if ("wb_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,4]) + sum(rh[ ,4]) }
      if ("wb_w" %in% var.names ) { out[length(out) + 1] <- sum( wm[ ,4]) }
      if ("wb_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,5] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,5] / sum(lh[ ,3] + rh[ ,3])) }
      if ("wb_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,6] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,6] / sum(lh[ ,3] + rh[ ,3])) }
      if ("wb_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,7] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,7] / sum(lh[ ,3] + rh[ ,3])) }
      if ("wb_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,8] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,8] / sum(lh[ ,3] + rh[ ,3])) }
      if ("wb_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,9] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,9] / sum(lh[ ,3] + rh[ ,3])) }
      if ("wb_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,10] / sum(lh[ ,3] + rh[ ,3])) + sum(rh[ ,3] * rh[ ,10] / sum(lh[ ,3] + rh[ ,3])) }
      
      # LH ----
      if ("lh_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,2]) }
      if ("lh_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3]) }
      if ("lh_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,4]) }
      if ("lh_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[c(1:34,69),4]) }
      if ("lh_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,5] / sum(lh[ ,3])) }
      if ("lh_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,6] / sum(lh[ ,3])) }
      if ("lh_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,7] / sum(lh[ ,3])) }
      if ("lh_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,8] / sum(lh[ ,3])) }
      if ("lh_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,9] / sum(lh[ ,3])) }
      if ("lh_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[ ,3] * lh[ ,10] / sum(lh[ ,3])) }
      
      # RH ----
      if ("rh_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,2]) }
      if ("rh_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3]) }
      if ("rh_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,4]) }
      if ("rh_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[c(35:68, 70),4]) }
      if ("rh_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,5] / sum(rh[ ,3])) }
      if ("rh_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,6] / sum(rh[ ,3])) }
      if ("rh_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,7] / sum(rh[ ,3])) }
      if ("rh_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,8] / sum(rh[ ,3])) }
      if ("rh_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,9] / sum(rh[ ,3])) }
      if ("rh_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[ ,3] * rh[ ,10] / sum(rh[ ,3])) }
      
      # Frontal ----
      if ("frnt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,2]) + sum(rh[f.num,2]) }
      if ("frnt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3]) + sum(rh[f.num,3]) }
      if ("frnt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,4]) + sum(rh[f.num,4]) }
      if ("frnt_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[f.num,4]) + sum(wm[f.num+34,4]) }
      if ("frnt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,5] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,5] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,6] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,6] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,7] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,7] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,8] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,8] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,9] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,9] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,10] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,10] / sum(lh[f.num,3] + rh[f.num,3])) }
      
      if ("frnt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,2]) }
      if ("frnt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3]) }
      if ("frnt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,4]) }
      if ("frnt_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[f.num+34,4]) }
      if ("frnt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,5] / sum(rh[f.num,3])) }
      if ("frnt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,6] / sum(rh[f.num,3])) }
      if ("frnt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,7] / sum(rh[f.num,3])) }
      if ("frnt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,8] / sum(rh[f.num,3])) }
      if ("frnt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,9] / sum(rh[f.num,3])) }
      if ("frnt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,10] / sum(rh[f.num,3])) }
      
      if ("frnt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,2]) }
      if ("frnt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3]) }
      if ("frnt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,4]) }
      if ("frnt_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[f.num,4]) }
      if ("frnt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,5] / sum(lh[f.num,3])) }
      if ("frnt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,6] / sum(lh[f.num,3])) }
      if ("frnt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,7] / sum(lh[f.num,3])) }
      if ("frnt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,8] / sum(lh[f.num,3])) }
      if ("frnt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,9] / sum(lh[f.num,3])) }
      if ("frnt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,10] / sum(lh[f.num,3])) }
      
      # Parietal ----
      if ("par_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,2]) + sum(rh[p.num,2]) }
      if ("par_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3]) + sum(rh[p.num,3]) }
      if ("par_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,4]) + sum(rh[p.num,4]) }
      if ("par_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[p.num,4]) + sum(wm[p.num+34,4]) }
      if ("par_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,5] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,5] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,6] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,6] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,7] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,7] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,8] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,8] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,9] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,9] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,10] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,10] / sum(lh[p.num,3] + rh[p.num,3])) }
      
      if ("par_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,2]) }
      if ("par_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3]) }
      if ("par_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,4]) }
      if ("par_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[p.num+34,4]) }
      if ("par_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,5] / sum(rh[p.num,3])) }
      if ("par_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,6] / sum(rh[p.num,3])) }
      if ("par_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,7] / sum(rh[p.num,3])) }
      if ("par_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,8] / sum(rh[p.num,3])) }
      if ("par_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,9] / sum(rh[p.num,3])) }
      if ("par_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,10] / sum(rh[p.num,3])) }
      
      if ("par_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,2]) }
      if ("par_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3]) }
      if ("par_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,4]) }
      if ("par_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[p.num,4]) }
      if ("par_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,5] / sum(lh[p.num,3])) }
      if ("par_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,6] / sum(lh[p.num,3])) }
      if ("par_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,7] / sum(lh[p.num,3])) }
      if ("par_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,8] / sum(lh[p.num,3])) }
      if ("par_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,9] / sum(lh[p.num,3])) }
      if ("par_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,10] / sum(lh[p.num,3])) }
      
      # Temporal ----
      if ("temp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,2]) + sum(rh[t.num,2]) }
      if ("temp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3]) + sum(rh[t.num,3]) }
      if ("temp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,4]) + sum(rh[t.num,4]) }
      if ("temp_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[t.num,4]) + sum(wm[t.num+34,4]) }
      if ("temp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,5] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,5] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,6] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,6] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,7] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,7] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,8] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,8] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,9] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,9] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,10] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,10] / sum(lh[t.num,3] + rh[t.num,3])) }
      
      if ("temp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,2]) }
      if ("temp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3]) }
      if ("temp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,4]) }
      if ("temp_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[t.num+34,4]) }
      if ("temp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,5] / sum(rh[t.num,3])) }
      if ("temp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,6] / sum(rh[t.num,3])) }
      if ("temp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,7] / sum(rh[t.num,3])) }
      if ("temp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,8] / sum(rh[t.num,3])) }
      if ("temp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,9] / sum(rh[t.num,3])) }
      if ("temp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,10] / sum(rh[t.num,3])) }
      
      if ("temp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,2]) }
      if ("temp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3]) }
      if ("temp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,4]) }
      if ("temp_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[t.num,4]) }
      if ("temp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,5] / sum(lh[t.num,3])) }
      if ("temp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,6] / sum(lh[t.num,3])) }
      if ("temp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,7] / sum(lh[t.num,3])) }
      if ("temp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,8] / sum(lh[t.num,3])) }
      if ("temp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,9] / sum(lh[t.num,3])) }
      if ("temp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,10] / sum(lh[t.num,3])) }
      
      # Occipital ----
      if ("occ_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,2]) + sum(rh[o.num,2]) }
      if ("occ_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3]) + sum(rh[o.num,3]) }
      if ("occ_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,4]) + sum(rh[o.num,4]) }
      if ("occ_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[o.num,4]) + sum(wm[o.num+34,4]) }
      if ("occ_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,5] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,5] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,6] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,6] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,7] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,7] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,8] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,8] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,9] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,9] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,10] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,10] / sum(lh[o.num,3] + rh[o.num,3])) }
      
      if ("occ_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,2]) }
      if ("occ_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3]) }
      if ("occ_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,4]) }
      if ("occ_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[o.num+34,4]) }
      if ("occ_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,5] / sum(rh[o.num,3])) }
      if ("occ_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,6] / sum(rh[o.num,3])) }
      if ("occ_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,7] / sum(rh[o.num,3])) }
      if ("occ_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,8] / sum(rh[o.num,3])) }
      if ("occ_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,9] / sum(rh[o.num,3])) }
      if ("occ_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,10] / sum(rh[o.num,3])) }
      
      if ("occ_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,2]) }
      if ("occ_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3]) }
      if ("occ_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,4]) }
      if ("occ_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[o.num,4]) }
      if ("occ_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,5] / sum(lh[o.num,3])) }
      if ("occ_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,6] / sum(lh[o.num,3])) }
      if ("occ_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,7] / sum(lh[o.num,3])) }
      if ("occ_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,8] / sum(lh[o.num,3])) }
      if ("occ_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,9] / sum(lh[o.num,3])) }
      if ("occ_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,10] / sum(lh[o.num,3])) }
      
      # Cingulate ----
      if ("cing_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,2]) + sum(rh[c.num,2]) }
      if ("cing_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3]) + sum(rh[c.num,3]) }
      if ("cing_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,4]) + sum(rh[c.num,4]) }
      if ("cing_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[c.num,4]) + sum(wm[c.num+34,4]) }
      if ("cing_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,5] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,5] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,6] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,6] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,7] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,7] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,8] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,8] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,9] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,9] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,10] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,10] / sum(lh[c.num,3] + rh[c.num,3])) }
      
      if ("cing_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,2]) }
      if ("cing_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3]) }
      if ("cing_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,4]) }
      if ("cing_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[c.num+34,4]) }
      if ("cing_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,5] / sum(rh[c.num,3])) }
      if ("cing_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,6] / sum(rh[c.num,3])) }
      if ("cing_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,7] / sum(rh[c.num,3])) }
      if ("cing_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,8] / sum(rh[c.num,3])) }
      if ("cing_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,9] / sum(rh[c.num,3])) }
      if ("cing_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,10] / sum(rh[c.num,3])) }
      
      if ("cing_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,2]) }
      if ("cing_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3]) }
      if ("cing_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,4]) }
      if ("cing_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[c.num,4]) }
      if ("cing_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,5] / sum(lh[c.num,3])) }
      if ("cing_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,6] / sum(lh[c.num,3])) }
      if ("cing_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,7] / sum(lh[c.num,3])) }
      if ("cing_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,8] / sum(lh[c.num,3])) }
      if ("cing_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,9] / sum(lh[c.num,3])) }
      if ("cing_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,10] / sum(lh[c.num,3])) }
      
      # Cortical ROIs ----
      if ("cmf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,2]) + sum(rh[3,2]) }
      if ("cmf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3]) + sum(rh[3,3]) }
      if ("cmf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,4]) + sum(rh[3,4]) }
      if ("cmf_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[3,4]) + sum(wm[3+34,4]) }
      if ("cmf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,5] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,5] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,6] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,6] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,7] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,7] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,8] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,8] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,9] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,9] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,10] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,10] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,2]) }
      if ("cmf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3]) }
      if ("cmf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,4]) }
      if ("cmf_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[3+34,4]) }
      if ("cmf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,5] / sum(rh[3,3])) }
      if ("cmf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,6] / sum(rh[3,3])) }
      if ("cmf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,7] / sum(rh[3,3])) }
      if ("cmf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,8] / sum(rh[3,3])) }
      if ("cmf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,9] / sum(rh[3,3])) }
      if ("cmf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,10] / sum(rh[3,3])) }
      if ("cmf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,2]) }
      if ("cmf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3]) }
      if ("cmf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,4]) }
      if ("cmf_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[3,4]) }
      if ("cmf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,5] / sum(lh[3,3])) }
      if ("cmf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,6] / sum(lh[3,3])) }
      if ("cmf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,7] / sum(lh[3,3])) }
      if ("cmf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,8] / sum(lh[3,3])) }
      if ("cmf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,9] / sum(lh[3,3])) }
      if ("cmf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,10] / sum(lh[3,3])) }
      
      if ("lof_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,2]) + sum(rh[11,2]) }
      if ("lof_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3]) + sum(rh[11,3]) }
      if ("lof_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,4]) + sum(rh[11,4]) }
      if ("lof_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[11,4]) + sum(wm[11+34,4]) }
      if ("lof_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,5] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,5] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,6] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,6] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,7] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,7] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,8] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,8] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,9] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,9] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,10] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,10] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,2]) }
      if ("lof_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3]) }
      if ("lof_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,4]) }
      if ("lof_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[11+34,4]) }
      if ("lof_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,5] / sum(rh[11,3])) }
      if ("lof_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,6] / sum(rh[11,3])) }
      if ("lof_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,7] / sum(rh[11,3])) }
      if ("lof_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,8] / sum(rh[11,3])) }
      if ("lof_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,9] / sum(rh[11,3])) }
      if ("lof_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,10] / sum(rh[11,3])) }
      if ("lof_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,2]) }
      if ("lof_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3]) }
      if ("lof_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,4]) }
      if ("lof_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[11,4]) }
      if ("lof_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,5] / sum(lh[11,3])) }
      if ("lof_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,6] / sum(lh[11,3])) }
      if ("lof_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,7] / sum(lh[11,3])) }
      if ("lof_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,8] / sum(lh[11,3])) }
      if ("lof_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,9] / sum(lh[11,3])) }
      if ("lof_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,10] / sum(lh[11,3])) }
      
      if ("mof_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,2]) + sum(rh[13,2]) }
      if ("mof_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3]) + sum(rh[13,3]) }
      if ("mof_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,4]) + sum(rh[13,4]) }
      if ("mof_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[13,4]) + sum(wm[13+34,4]) }
      if ("mof_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,5] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,5] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,6] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,6] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,7] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,7] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,8] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,8] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,9] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,9] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,10] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,10] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,2]) }
      if ("mof_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3]) }
      if ("mof_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,4]) }
      if ("mof_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[13+34,4]) }
      if ("mof_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,5] / sum(rh[13,3])) }
      if ("mof_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,6] / sum(rh[13,3])) }
      if ("mof_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,7] / sum(rh[13,3])) }
      if ("mof_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,8] / sum(rh[13,3])) }
      if ("mof_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,9] / sum(rh[13,3])) }
      if ("mof_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,10] / sum(rh[13,3])) }
      if ("mof_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,2]) }
      if ("mof_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3]) }
      if ("mof_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,4]) }
      if ("mof_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[13,4]) }
      if ("mof_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,5] / sum(lh[13,3])) }
      if ("mof_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,6] / sum(lh[13,3])) }
      if ("mof_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,7] / sum(lh[13,3])) }
      if ("mof_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,8] / sum(lh[13,3])) }
      if ("mof_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,9] / sum(lh[13,3])) }
      if ("mof_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,10] / sum(lh[13,3])) }
      
      if ("fp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,2]) + sum(rh[31,2]) }
      if ("fp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3]) + sum(rh[31,3]) }
      if ("fp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,4]) + sum(rh[31,4]) }
      if ("fp_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[31,4]) + sum(wm[31+34,4]) }
      if ("fp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,5] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,5] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,6] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,6] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,7] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,7] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,8] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,8] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,9] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,9] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,10] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,10] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,2]) }
      if ("fp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3]) }
      if ("fp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,4]) }
      if ("fp_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[31+34,4]) }
      if ("fp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,5] / sum(rh[31,3])) }
      if ("fp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,6] / sum(rh[31,3])) }
      if ("fp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,7] / sum(rh[31,3])) }
      if ("fp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,8] / sum(rh[31,3])) }
      if ("fp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,9] / sum(rh[31,3])) }
      if ("fp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,10] / sum(rh[31,3])) }
      if ("fp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,2]) }
      if ("fp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3]) }
      if ("fp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,4]) }
      if ("fp_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[31,4]) }
      if ("fp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,5] / sum(lh[31,3])) }
      if ("fp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,6] / sum(lh[31,3])) }
      if ("fp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,7] / sum(lh[31,3])) }
      if ("fp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,8] / sum(lh[31,3])) }
      if ("fp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,9] / sum(lh[31,3])) }
      if ("fp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,10] / sum(lh[31,3])) }
      
      if ("po_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,2]) + sum(rh[18,2]) }
      if ("po_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3]) + sum(rh[18,3]) }
      if ("po_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,4]) + sum(rh[18,4]) }
      if ("po_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[18,4]) + sum(wm[18+34,4]) }
      if ("po_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,5] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,5] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,6] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,6] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,7] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,7] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,8] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,8] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,9] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,9] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,10] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,10] / sum(lh[18,3] + rh[18,3])) }
      if ("po_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,2]) }
      if ("po_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3]) }
      if ("po_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,4]) }
      if ("po_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[18+34,4]) }
      if ("po_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,5] / sum(rh[18,3])) }
      if ("po_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,6] / sum(rh[18,3])) }
      if ("po_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,7] / sum(rh[18,3])) }
      if ("po_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,8] / sum(rh[18,3])) }
      if ("po_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,9] / sum(rh[18,3])) }
      if ("po_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,10] / sum(rh[18,3])) }
      if ("po_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,2]) }
      if ("po_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3]) }
      if ("po_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,4]) }
      if ("po_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[18,4]) }
      if ("po_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,5] / sum(lh[18,3])) }
      if ("po_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,6] / sum(lh[18,3])) }
      if ("po_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,7] / sum(lh[18,3])) }
      if ("po_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,8] / sum(lh[18,3])) }
      if ("po_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,9] / sum(lh[18,3])) }
      if ("po_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,10] / sum(lh[18,3])) }
      
      if ("pt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,2]) + sum(rh[19,2]) }
      if ("pt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3]) + sum(rh[19,3]) }
      if ("pt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,4]) + sum(rh[19,4]) }
      if ("pt_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[19,4]) + sum(wm[19+34,4]) }
      if ("pt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,5] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,5] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,6] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,6] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,7] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,7] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,8] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,8] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,9] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,9] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,10] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,10] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,2]) }
      if ("pt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3]) }
      if ("pt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,4]) }
      if ("pt_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[19+34,4]) }
      if ("pt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,5] / sum(rh[19,3])) }
      if ("pt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,6] / sum(rh[19,3])) }
      if ("pt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,7] / sum(rh[19,3])) }
      if ("pt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,8] / sum(rh[19,3])) }
      if ("pt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,9] / sum(rh[19,3])) }
      if ("pt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,10] / sum(rh[19,3])) }
      if ("pt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,2]) }
      if ("pt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3]) }
      if ("pt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,4]) }
      if ("pt_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[19,4]) }
      if ("pt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,5] / sum(lh[19,3])) }
      if ("pt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,6] / sum(lh[19,3])) }
      if ("pt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,7] / sum(lh[19,3])) }
      if ("pt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,8] / sum(lh[19,3])) }
      if ("pt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,9] / sum(lh[19,3])) }
      if ("pt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,10] / sum(lh[19,3])) }
      
      if ("rmf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,2]) + sum(rh[26,2]) }
      if ("rmf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3]) + sum(rh[26,3]) }
      if ("rmf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,4]) + sum(rh[26,4]) }
      if ("rmf_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[26,4]) + sum(wm[26+34,4]) }
      if ("rmf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,5] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,5] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,6] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,6] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,7] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,7] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,8] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,8] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,9] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,9] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,10] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,10] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,2]) }
      if ("rmf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3]) }
      if ("rmf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,4]) }
      if ("rmf_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[26+34,4]) }
      if ("rmf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,5] / sum(rh[26,3])) }
      if ("rmf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,6] / sum(rh[26,3])) }
      if ("rmf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,7] / sum(rh[26,3])) }
      if ("rmf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,8] / sum(rh[26,3])) }
      if ("rmf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,9] / sum(rh[26,3])) }
      if ("rmf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,10] / sum(rh[26,3])) }
      if ("rmf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,2]) }
      if ("rmf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3]) }
      if ("rmf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,4]) }
      if ("rmf_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[26,4]) }
      if ("rmf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,5] / sum(lh[26,3])) }
      if ("rmf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,6] / sum(lh[26,3])) }
      if ("rmf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,7] / sum(lh[26,3])) }
      if ("rmf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,8] / sum(lh[26,3])) }
      if ("rmf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,9] / sum(lh[26,3])) }
      if ("rmf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,10] / sum(lh[26,3])) }
      
      if ("sf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,2]) + sum(rh[27,2]) }
      if ("sf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3]) + sum(rh[27,3]) }
      if ("sf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,4]) + sum(rh[27,4]) }
      if ("sf_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[27,4]) + sum(wm[27+34,4]) }
      if ("sf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,5] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,5] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,6] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,6] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,7] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,7] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,8] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,8] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,9] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,9] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,10] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,10] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,2]) }
      if ("sf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3]) }
      if ("sf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,4]) }
      if ("sf_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[27+34,4]) }
      if ("sf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,5] / sum(rh[27,3])) }
      if ("sf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,6] / sum(rh[27,3])) }
      if ("sf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,7] / sum(rh[27,3])) }
      if ("sf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,8] / sum(rh[27,3])) }
      if ("sf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,9] / sum(rh[27,3])) }
      if ("sf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,10] / sum(rh[27,3])) }
      if ("sf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,2]) }
      if ("sf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3]) }
      if ("sf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,4]) }
      if ("sf_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[27,4]) }
      if ("sf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,5] / sum(lh[27,3])) }
      if ("sf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,6] / sum(lh[27,3])) }
      if ("sf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,7] / sum(lh[27,3])) }
      if ("sf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,8] / sum(lh[27,3])) }
      if ("sf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,9] / sum(lh[27,3])) }
      if ("sf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,10] / sum(lh[27,3])) }
      
      if ("parac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,2]) + sum(rh[16,2]) }
      if ("parac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3]) + sum(rh[16,3]) }
      if ("parac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,4]) + sum(rh[16,4]) }
      if ("parac_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[16,4]) + sum(wm[16+34,4]) }
      if ("parac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,5] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,5] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,6] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,6] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,7] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,7] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,8] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,8] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,9] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,9] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,10] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,10] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,2]) }
      if ("parac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3]) }
      if ("parac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,4]) }
      if ("parac_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[16+34,4]) }
      if ("parac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,5] / sum(rh[16,3])) }
      if ("parac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,6] / sum(rh[16,3])) }
      if ("parac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,7] / sum(rh[16,3])) }
      if ("parac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,8] / sum(rh[16,3])) }
      if ("parac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,9] / sum(rh[16,3])) }
      if ("parac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,10] / sum(rh[16,3])) }
      if ("parac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,2]) }
      if ("parac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3]) }
      if ("parac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,4]) }
      if ("parac_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[16,4]) }
      if ("parac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,5] / sum(lh[16,3])) }
      if ("parac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,6] / sum(lh[16,3])) }
      if ("parac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,7] / sum(lh[16,3])) }
      if ("parac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,8] / sum(lh[16,3])) }
      if ("parac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,9] / sum(lh[16,3])) }
      if ("parac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,10] / sum(lh[16,3])) }
      
      if ("prec_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,2]) + sum(rh[23,2]) }
      if ("prec_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3]) + sum(rh[23,3]) }
      if ("prec_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,4]) + sum(rh[23,4]) }
      if ("prec_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[23,4]) + sum(wm[23+34,4]) }
      if ("prec_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,5] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,5] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,6] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,6] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,7] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,7] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,8] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,8] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,9] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,9] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,10] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,10] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,2]) }
      if ("prec_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3]) }
      if ("prec_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,4]) }
      if ("prec_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[23+34,4]) }
      if ("prec_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,5] / sum(rh[23,3])) }
      if ("prec_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,6] / sum(rh[23,3])) }
      if ("prec_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,7] / sum(rh[23,3])) }
      if ("prec_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,8] / sum(rh[23,3])) }
      if ("prec_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,9] / sum(rh[23,3])) }
      if ("prec_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,10] / sum(rh[23,3])) }
      if ("prec_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,2]) }
      if ("prec_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3]) }
      if ("prec_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,4]) }
      if ("prec_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[23,4]) }
      if ("prec_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,5] / sum(lh[23,3])) }
      if ("prec_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,6] / sum(lh[23,3])) }
      if ("prec_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,7] / sum(lh[23,3])) }
      if ("prec_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,8] / sum(lh[23,3])) }
      if ("prec_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,9] / sum(lh[23,3])) }
      if ("prec_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,10] / sum(lh[23,3])) }
      
      if ("pp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,2]) + sum(rh[17,2]) }
      if ("pp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3]) + sum(rh[17,3]) }
      if ("pp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,4]) + sum(rh[17,4]) }
      if ("pp_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[17,4]) + sum(wm[17+34,4]) }
      if ("pp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,5] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,5] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,6] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,6] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,7] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,7] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,8] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,8] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,9] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,9] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,10] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,10] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,2]) }
      if ("pp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3]) }
      if ("pp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,4]) }
      if ("pp_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[17+34,4]) }
      if ("pp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,5] / sum(rh[17,3])) }
      if ("pp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,6] / sum(rh[17,3])) }
      if ("pp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,7] / sum(rh[17,3])) }
      if ("pp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,8] / sum(rh[17,3])) }
      if ("pp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,9] / sum(rh[17,3])) }
      if ("pp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,10] / sum(rh[17,3])) }
      if ("pp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,2]) }
      if ("pp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3]) }
      if ("pp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,4]) }
      if ("pp_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[17+34,4]) }
      if ("pp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,5] / sum(lh[17,3])) }
      if ("pp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,6] / sum(lh[17,3])) }
      if ("pp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,7] / sum(lh[17,3])) }
      if ("pp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,8] / sum(lh[17,3])) }
      if ("pp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,9] / sum(lh[17,3])) }
      if ("pp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,10] / sum(lh[17,3])) }
      
      if ("cac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,2]) + sum(rh[2,2]) }
      if ("cac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3]) + sum(rh[2,3]) }
      if ("cac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,4]) + sum(rh[2,4]) }
      if ("cac_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[2,4]) + sum(wm[2+34,4]) }
      if ("cac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,5] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,5] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,6] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,6] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,7] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,7] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,8] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,8] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,9] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,9] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,10] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,10] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,2]) }
      if ("cac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3]) }
      if ("cac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,4]) }
      if ("cac_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[2+34,4]) }
      if ("cac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,5] / sum(rh[2,3])) }
      if ("cac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,6] / sum(rh[2,3])) }
      if ("cac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,7] / sum(rh[2,3])) }
      if ("cac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,8] / sum(rh[2,3])) }
      if ("cac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,9] / sum(rh[2,3])) }
      if ("cac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,10] / sum(rh[2,3])) }
      if ("cac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,2]) }
      if ("cac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3]) }
      if ("cac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,4]) }
      if ("cac_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[2,4]) }
      if ("cac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,5] / sum(lh[2,3])) }
      if ("cac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,6] / sum(lh[2,3])) }
      if ("cac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,7] / sum(lh[2,3])) }
      if ("cac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,8] / sum(lh[2,3])) }
      if ("cac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,9] / sum(lh[2,3])) }
      if ("cac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,10] / sum(lh[2,3])) }
      
      if ("rac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,2]) + sum(rh[25,2]) }
      if ("rac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3]) + sum(rh[25,3]) }
      if ("rac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,4]) + sum(rh[25,4]) }
      if ("rac_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[25,4]) + sum(wm[25+34,4]) }
      if ("rac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,5] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,5] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,6] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,6] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,7] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,7] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,8] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,8] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,9] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,9] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,10] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,10] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,2]) }
      if ("rac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3]) }
      if ("rac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,4]) }
      if ("rac_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[25+34,4]) }
      if ("rac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,5] / sum(rh[25,3])) }
      if ("rac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,6] / sum(rh[25,3])) }
      if ("rac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,7] / sum(rh[25,3])) }
      if ("rac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,8] / sum(rh[25,3])) }
      if ("rac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,9] / sum(rh[25,3])) }
      if ("rac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,10] / sum(rh[25,3])) }
      if ("rac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,2]) }
      if ("rac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3]) }
      if ("rac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,4]) }
      if ("rac_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[25,4]) }
      if ("rac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,5] / sum(lh[25,3])) }
      if ("rac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,6] / sum(lh[25,3])) }
      if ("rac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,7] / sum(lh[25,3])) }
      if ("rac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,8] / sum(lh[25,3])) }
      if ("rac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,9] / sum(lh[25,3])) }
      if ("rac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,10] / sum(lh[25,3])) }
      
      if ("ins_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,2]) + sum(rh[34,2]) }
      if ("ins_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3]) + sum(rh[34,3]) }
      if ("ins_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,4]) + sum(rh[34,4]) }
      if ("ins_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[34,4]) + sum(wm[34+34,4]) }
      if ("ins_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,5] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,5] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,6] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,6] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,7] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,7] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,8] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,8] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,9] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,9] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,10] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,10] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,2]) }
      if ("ins_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3]) }
      if ("ins_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,4]) }
      if ("ins_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[34+34,4]) }
      if ("ins_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,5] / sum(rh[34,3])) }
      if ("ins_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,6] / sum(rh[34,3])) }
      if ("ins_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,7] / sum(rh[34,3])) }
      if ("ins_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,8] / sum(rh[34,3])) }
      if ("ins_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,9] / sum(rh[34,3])) }
      if ("ins_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,10] / sum(rh[34,3])) }
      if ("ins_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,2]) }
      if ("ins_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3]) }
      if ("ins_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,4]) }
      if ("ins_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[34,4]) }
      if ("ins_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,5] / sum(lh[34,3])) }
      if ("ins_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,6] / sum(lh[34,3])) }
      if ("ins_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,7] / sum(lh[34,3])) }
      if ("ins_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,8] / sum(lh[34,3])) }
      if ("ins_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,9] / sum(lh[34,3])) }
      if ("ins_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,10] / sum(lh[34,3])) }
      
      if ("postc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,2]) + sum(rh[21,2]) }
      if ("postc_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3]) + sum(rh[21,3]) }
      if ("postc_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,4]) + sum(rh[21,4]) }
      if ("postc_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[21,4]) + sum(wm[21+34,4]) }
      if ("postc_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,5] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,5] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,6] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,6] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,7] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,7] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,8] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,8] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,9] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,9] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,10] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,10] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,2]) }
      if ("postc_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3]) }
      if ("postc_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,4]) }
      if ("postc_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[21+34,4]) }
      if ("postc_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,5] / sum(rh[21,3])) }
      if ("postc_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,6] / sum(rh[21,3])) }
      if ("postc_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,7] / sum(rh[21,3])) }
      if ("postc_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,8] / sum(rh[21,3])) }
      if ("postc_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,9] / sum(rh[21,3])) }
      if ("postc_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,10] / sum(rh[21,3])) }
      if ("postc_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,2]) }
      if ("postc_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3]) }
      if ("postc_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,4]) }
      if ("postc_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[21,4]) }
      if ("postc_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,5] / sum(lh[21,3])) }
      if ("postc_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,6] / sum(lh[21,3])) }
      if ("postc_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,7] / sum(lh[21,3])) }
      if ("postc_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,8] / sum(lh[21,3])) }
      if ("postc_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,9] / sum(lh[21,3])) }
      if ("postc_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,10] / sum(lh[21,3])) }
      
      if ("ip_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,2]) + sum(rh[7,2]) }
      if ("ip_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3]) + sum(rh[7,3]) }
      if ("ip_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,4]) + sum(rh[7,4]) }
      if ("ip_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[7,4]) + sum(wm[7+34,4]) }
      if ("ip_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,5] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,5] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,6] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,6] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,7] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,7] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,8] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,8] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,9] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,9] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,10] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,10] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,2]) }
      if ("ip_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3]) }
      if ("ip_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,4]) }
      if ("ip_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[7+34,4]) }
      if ("ip_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,5] / sum(rh[7,3])) }
      if ("ip_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,6] / sum(rh[7,3])) }
      if ("ip_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,7] / sum(rh[7,3])) }
      if ("ip_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,8] / sum(rh[7,3])) }
      if ("ip_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,9] / sum(rh[7,3])) }
      if ("ip_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,10] / sum(rh[7,3])) }
      if ("ip_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,2]) }
      if ("ip_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3]) }
      if ("ip_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,4]) }
      if ("ip_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[7,4]) }
      if ("ip_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,5] / sum(lh[7,3])) }
      if ("ip_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,6] / sum(lh[7,3])) }
      if ("ip_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,7] / sum(lh[7,3])) }
      if ("ip_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,8] / sum(lh[7,3])) }
      if ("ip_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,9] / sum(lh[7,3])) }
      if ("ip_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,10] / sum(lh[7,3])) }
      
      if ("sp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,2]) + sum(rh[28,2]) }
      if ("sp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3]) + sum(rh[28,3]) }
      if ("sp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,4]) + sum(rh[28,4]) }
      if ("sp_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[28,4]) + sum(wm[28+34,4]) }
      if ("sp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,5] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,5] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,6] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,6] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,7] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,7] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,8] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,8] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,9] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,9] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,10] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,10] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,2]) }
      if ("sp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3]) }
      if ("sp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,4]) }
      if ("sp_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[28+34,4]) }
      if ("sp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,5] / sum(rh[28,3])) }
      if ("sp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,6] / sum(rh[28,3])) }
      if ("sp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,7] / sum(rh[28,3])) }
      if ("sp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,8] / sum(rh[28,3])) }
      if ("sp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,9] / sum(rh[28,3])) }
      if ("sp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,10] / sum(rh[28,3])) }
      if ("sp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,2]) }
      if ("sp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3]) }
      if ("sp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,4]) }
      if ("sp_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[28,4]) }
      if ("sp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,5] / sum(lh[28,3])) }
      if ("sp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,6] / sum(lh[28,3])) }
      if ("sp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,7] / sum(lh[28,3])) }
      if ("sp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,8] / sum(lh[28,3])) }
      if ("sp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,9] / sum(lh[28,3])) }
      if ("sp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,10] / sum(lh[28,3])) }
      
      if ("pcun_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,2]) + sum(rh[24,2]) }
      if ("pcun_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3]) + sum(rh[24,3]) }
      if ("pcun_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,4]) + sum(rh[24,4]) }
      if ("pcun_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[24,4]) + sum(wm[24+34,4]) }
      if ("pcun_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,5] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,5] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,6] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,6] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,7] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,7] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,8] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,8] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,9] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,9] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,10] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,10] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,2]) }
      if ("pcun_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3]) }
      if ("pcun_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,4]) }
      if ("pcun_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[24+34,4]) }
      if ("pcun_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,5] / sum(rh[24,3])) }
      if ("pcun_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,6] / sum(rh[24,3])) }
      if ("pcun_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,7] / sum(rh[24,3])) }
      if ("pcun_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,8] / sum(rh[24,3])) }
      if ("pcun_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,9] / sum(rh[24,3])) }
      if ("pcun_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,10] / sum(rh[24,3])) }
      if ("pcun_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,2]) }
      if ("pcun_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3]) }
      if ("pcun_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,4]) }
      if ("pcun_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[24,4]) }
      if ("pcun_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,5] / sum(lh[24,3])) }
      if ("pcun_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,6] / sum(lh[24,3])) }
      if ("pcun_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,7] / sum(lh[24,3])) }
      if ("pcun_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,8] / sum(lh[24,3])) }
      if ("pcun_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,9] / sum(lh[24,3])) }
      if ("pcun_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,10] / sum(lh[24,3])) }
      
      if ("sm_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,2]) + sum(rh[30,2]) }
      if ("sm_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3]) + sum(rh[30,3]) }
      if ("sm_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,4]) + sum(rh[30,4]) }
      if ("sm_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[30,4]) + sum(wm[30+34,4]) }
      if ("sm_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,5] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,5] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,6] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,6] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,7] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,7] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,8] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,8] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,9] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,9] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,10] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,10] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,2]) }
      if ("sm_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3]) }
      if ("sm_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,4]) }
      if ("sm_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[30+34,4]) }
      if ("sm_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,5] / sum(rh[30,3])) }
      if ("sm_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,6] / sum(rh[30,3])) }
      if ("sm_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,7] / sum(rh[30,3])) }
      if ("sm_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,8] / sum(rh[30,3])) }
      if ("sm_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,9] / sum(rh[30,3])) }
      if ("sm_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,10] / sum(rh[30,3])) }
      if ("sm_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,2]) }
      if ("sm_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3]) }
      if ("sm_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,4]) }
      if ("sm_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[30,4]) }
      if ("sm_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,5] / sum(lh[30,3])) }
      if ("sm_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,6] / sum(lh[30,3])) }
      if ("sm_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,7] / sum(lh[30,3])) }
      if ("sm_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,8] / sum(lh[30,3])) }
      if ("sm_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,9] / sum(lh[30,3])) }
      if ("sm_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,10] / sum(lh[30,3])) }
      
      if ("pc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,2]) + sum(rh[22,2]) }
      if ("pc_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3]) + sum(rh[22,3]) }
      if ("pc_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,4]) + sum(rh[22,4]) }
      if ("pc_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[22,4]) + sum(wm[22+34,4]) }
      if ("pc_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,5] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,5] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,6] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,6] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,7] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,7] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,8] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,8] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,9] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,9] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,10] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,10] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,2]) }
      if ("pc_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3]) }
      if ("pc_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,4]) }
      if ("pc_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[22+34,4]) }
      if ("pc_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,5] / sum(rh[22,3])) }
      if ("pc_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,6] / sum(rh[22,3])) }
      if ("pc_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,7] / sum(rh[22,3])) }
      if ("pc_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,8] / sum(rh[22,3])) }
      if ("pc_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,9] / sum(rh[22,3])) }
      if ("pc_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,10] / sum(rh[22,3])) }
      if ("pc_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,2]) }
      if ("pc_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3]) }
      if ("pc_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,4]) }
      if ("pc_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[22,4]) }
      if ("pc_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,5] / sum(lh[22,3])) }
      if ("pc_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,6] / sum(lh[22,3])) }
      if ("pc_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,7] / sum(lh[22,3])) }
      if ("pc_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,8] / sum(lh[22,3])) }
      if ("pc_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,9] / sum(lh[22,3])) }
      if ("pc_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,10] / sum(lh[22,3])) }
      
      if ("ic_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,2]) + sum(rh[9,2]) }
      if ("ic_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3]) + sum(rh[9,3]) }
      if ("ic_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,4]) + sum(rh[9,4]) }
      if ("ic_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[9,4]) + sum(wm[9+34,4]) }
      if ("ic_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,5] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,5] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,6] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,6] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,7] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,7] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,8] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,8] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,9] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,9] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,10] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,10] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,2]) }
      if ("ic_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3]) }
      if ("ic_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,4]) }
      if ("ic_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[9+34,4]) }
      if ("ic_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,5] / sum(rh[9,3])) }
      if ("ic_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,6] / sum(rh[9,3])) }
      if ("ic_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,7] / sum(rh[9,3])) }
      if ("ic_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,8] / sum(rh[9,3])) }
      if ("ic_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,9] / sum(rh[9,3])) }
      if ("ic_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,10] / sum(rh[9,3])) }
      if ("ic_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,2]) }
      if ("ic_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3]) }
      if ("ic_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,4]) }
      if ("ic_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[9,4]) }
      if ("ic_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,5] / sum(lh[9,3])) }
      if ("ic_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,6] / sum(lh[9,3])) }
      if ("ic_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,7] / sum(lh[9,3])) }
      if ("ic_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,8] / sum(lh[9,3])) }
      if ("ic_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,9] / sum(lh[9,3])) }
      if ("ic_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,10] / sum(lh[9,3])) }
      
      if ("it_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,2]) + sum(rh[8,2]) }
      if ("it_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3]) + sum(rh[8,3]) }
      if ("it_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,4]) + sum(rh[8,4]) }
      if ("it_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[8,4]) + sum(wm[8+34,4]) }
      if ("it_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,5] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,5] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,6] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,6] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,7] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,7] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,8] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,8] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,9] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,9] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,10] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,10] / sum(lh[8,3] + rh[8,3])) }
      if ("it_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,2]) }
      if ("it_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3]) }
      if ("it_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,4]) }
      if ("it_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[8+34,4]) }
      if ("it_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,5] / sum(rh[8,3])) }
      if ("it_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,6] / sum(rh[8,3])) }
      if ("it_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,7] / sum(rh[8,3])) }
      if ("it_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,8] / sum(rh[8,3])) }
      if ("it_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,9] / sum(rh[8,3])) }
      if ("it_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,10] / sum(rh[8,3])) }
      if ("it_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,2]) }
      if ("it_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3]) }
      if ("it_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,4]) }
      if ("it_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[8,4]) }
      if ("it_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,5] / sum(lh[8,3])) }
      if ("it_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,6] / sum(lh[8,3])) }
      if ("it_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,7] / sum(lh[8,3])) }
      if ("it_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,8] / sum(lh[8,3])) }
      if ("it_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,9] / sum(lh[8,3])) }
      if ("it_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,10] / sum(lh[8,3])) }
      
      if ("mt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,2]) + sum(rh[14,2]) }
      if ("mt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3]) + sum(rh[14,3]) }
      if ("mt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,4]) + sum(rh[14,4]) }
      if ("mt_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[14,4]) + sum(wm[14+34,4]) }
      if ("mt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,5] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,5] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,6] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,6] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,7] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,7] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,8] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,8] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,9] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,9] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,10] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,10] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,2]) }
      if ("mt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3]) }
      if ("mt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,4]) }
      if ("mt_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[14+34,4]) }
      if ("mt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,5] / sum(rh[14,3])) }
      if ("mt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,6] / sum(rh[14,3])) }
      if ("mt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,7] / sum(rh[14,3])) }
      if ("mt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,8] / sum(rh[14,3])) }
      if ("mt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,9] / sum(rh[14,3])) }
      if ("mt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,10] / sum(rh[14,3])) }
      if ("mt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,2]) }
      if ("mt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3]) }
      if ("mt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,4]) }
      if ("mt_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[14,4]) }
      if ("mt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,5] / sum(lh[14,3])) }
      if ("mt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,6] / sum(lh[14,3])) }
      if ("mt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,7] / sum(lh[14,3])) }
      if ("mt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,8] / sum(lh[14,3])) }
      if ("mt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,9] / sum(lh[14,3])) }
      if ("mt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,10] / sum(lh[14,3])) }
      
      if ("st_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,2]) + sum(rh[29,2]) }
      if ("st_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3]) + sum(rh[29,3]) }
      if ("st_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,4]) + sum(rh[29,4]) }
      if ("st_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[29,4]) + sum(wm[29+34,4]) }
      if ("st_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,5] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,5] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,6] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,6] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,7] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,7] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,8] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,8] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,9] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,9] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,10] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,10] / sum(lh[29,3] + rh[29,3])) }
      if ("st_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,2]) }
      if ("st_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3]) }
      if ("st_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,4]) }
      if ("st_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[29+34,4]) }
      if ("st_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,5] / sum(rh[29,3])) }
      if ("st_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,6] / sum(rh[29,3])) }
      if ("st_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,7] / sum(rh[29,3])) }
      if ("st_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,8] / sum(rh[29,3])) }
      if ("st_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,9] / sum(rh[29,3])) }
      if ("st_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,10] / sum(rh[29,3])) }
      if ("st_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,2]) }
      if ("st_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3]) }
      if ("st_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,4]) }
      if ("st_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[29,4]) }
      if ("st_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,5] / sum(lh[29,3])) }
      if ("st_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,6] / sum(lh[29,3])) }
      if ("st_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,7] / sum(lh[29,3])) }
      if ("st_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,8] / sum(lh[29,3])) }
      if ("st_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,9] / sum(lh[29,3])) }
      if ("st_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,10] / sum(lh[29,3])) }
      
      if ("tt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,2]) + sum(rh[33,2]) }
      if ("tt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3]) + sum(rh[33,3]) }
      if ("tt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,4]) + sum(rh[33,4]) }
      if ("tt_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[33,4]) + sum(wm[33+34,4]) }
      if ("tt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,5] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,5] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,6] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,6] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,7] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,7] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,8] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,8] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,9] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,9] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,10] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,10] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,2]) }
      if ("tt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3]) }
      if ("tt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,4]) }
      if ("tt_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[33+34,4]) }
      if ("tt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,5] / sum(rh[33,3])) }
      if ("tt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,6] / sum(rh[33,3])) }
      if ("tt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,7] / sum(rh[33,3])) }
      if ("tt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,8] / sum(rh[33,3])) }
      if ("tt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,9] / sum(rh[33,3])) }
      if ("tt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,10] / sum(rh[33,3])) }
      if ("tt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,2]) }
      if ("tt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3]) }
      if ("tt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,4]) }
      if ("tt_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[33,4]) }
      if ("tt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,5] / sum(lh[33,3])) }
      if ("tt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,6] / sum(lh[33,3])) }
      if ("tt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,7] / sum(lh[33,3])) }
      if ("tt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,8] / sum(lh[33,3])) }
      if ("tt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,9] / sum(lh[33,3])) }
      if ("tt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,10] / sum(lh[33,3])) }
      
      if ("ph_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,2]) + sum(rh[15,2]) }
      if ("ph_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3]) + sum(rh[15,3]) }
      if ("ph_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,4]) + sum(rh[15,4]) }
      if ("ph_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[15,4]) + sum(wm[15+34,4]) }
      if ("ph_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,5] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,5] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,6] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,6] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,7] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,7] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,8] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,8] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,9] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,9] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,10] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,10] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,2]) }
      if ("ph_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3]) }
      if ("ph_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,4]) }
      if ("ph_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[15+34,4]) }
      if ("ph_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,5] / sum(rh[15,3])) }
      if ("ph_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,6] / sum(rh[15,3])) }
      if ("ph_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,7] / sum(rh[15,3])) }
      if ("ph_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,8] / sum(rh[15,3])) }
      if ("ph_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,9] / sum(rh[15,3])) }
      if ("ph_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,10] / sum(rh[15,3])) }
      if ("ph_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,2]) }
      if ("ph_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3]) }
      if ("ph_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,4]) }
      if ("ph_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[15,4]) }
      if ("ph_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,5] / sum(lh[15,3])) }
      if ("ph_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,6] / sum(lh[15,3])) }
      if ("ph_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,7] / sum(lh[15,3])) }
      if ("ph_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,8] / sum(lh[15,3])) }
      if ("ph_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,9] / sum(lh[15,3])) }
      if ("ph_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,10] / sum(lh[15,3])) }
      
      if ("fus_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,2]) + sum(rh[6,2]) }
      if ("fus_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3]) + sum(rh[6,3]) }
      if ("fus_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,4]) + sum(rh[6,4]) }
      if ("fus_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[6,4]) + sum(wm[6+34,4]) }
      if ("fus_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,5] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,5] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,6] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,6] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,7] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,7] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,8] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,8] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,9] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,9] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,10] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,10] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,2]) }
      if ("fus_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3]) }
      if ("fus_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,4]) }
      if ("fus_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[6+34,4]) }
      if ("fus_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,5] / sum(rh[6,3])) }
      if ("fus_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,6] / sum(rh[6,3])) }
      if ("fus_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,7] / sum(rh[6,3])) }
      if ("fus_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,8] / sum(rh[6,3])) }
      if ("fus_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,9] / sum(rh[6,3])) }
      if ("fus_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,10] / sum(rh[6,3])) }
      if ("fus_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,2]) }
      if ("fus_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3]) }
      if ("fus_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,4]) }
      if ("fus_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[6,4]) }
      if ("fus_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,5] / sum(lh[6,3])) }
      if ("fus_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,6] / sum(lh[6,3])) }
      if ("fus_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,7] / sum(lh[6,3])) }
      if ("fus_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,8] / sum(lh[6,3])) }
      if ("fus_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,9] / sum(lh[6,3])) }
      if ("fus_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,10] / sum(lh[6,3])) }
      
      if ("bsts_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,2]) + sum(rh[1,2]) }
      if ("bsts_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3]) + sum(rh[1,3]) }
      if ("bsts_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,4]) + sum(rh[1,4]) }
      if ("bsts_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[1,4]) + sum(wm[1+34,4]) }
      if ("bsts_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,5] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,5] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,6] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,6] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,7] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,7] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,8] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,8] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,9] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,9] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,10] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,10] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,2]) }
      if ("bsts_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3]) }
      if ("bsts_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,4]) }
      if ("bsts_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[1+34,4]) }
      if ("bsts_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,5] / sum(rh[1,3])) }
      if ("bsts_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,6] / sum(rh[1,3])) }
      if ("bsts_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,7] / sum(rh[1,3])) }
      if ("bsts_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,8] / sum(rh[1,3])) }
      if ("bsts_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,9] / sum(rh[1,3])) }
      if ("bsts_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,10] / sum(rh[1,3])) }
      if ("bsts_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,2]) }
      if ("bsts_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3]) }
      if ("bsts_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,4]) }
      if ("bsts_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[1,4]) }
      if ("bsts_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,5] / sum(lh[1,3])) }
      if ("bsts_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,6] / sum(lh[1,3])) }
      if ("bsts_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,7] / sum(lh[1,3])) }
      if ("bsts_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,8] / sum(lh[1,3])) }
      if ("bsts_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,9] / sum(lh[1,3])) }
      if ("bsts_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,10] / sum(lh[1,3])) }
      
      if ("er_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,2]) + sum(rh[5,2]) }
      if ("er_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3]) + sum(rh[5,3]) }
      if ("er_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,4]) + sum(rh[5,4]) }
      if ("er_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[5,4]) + sum(wm[5+34,4]) }
      if ("er_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,5] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,5] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,6] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,6] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,7] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,7] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,8] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,8] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,9] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,9] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,10] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,10] / sum(lh[5,3] + rh[5,3])) }
      if ("er_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,2]) }
      if ("er_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3]) }
      if ("er_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,4]) }
      if ("er_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[5+34,4]) }
      if ("er_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,5] / sum(rh[5,3])) }
      if ("er_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,6] / sum(rh[5,3])) }
      if ("er_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,7] / sum(rh[5,3])) }
      if ("er_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,8] / sum(rh[5,3])) }
      if ("er_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,9] / sum(rh[5,3])) }
      if ("er_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,10] / sum(rh[5,3])) }
      if ("er_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,2]) }
      if ("er_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3]) }
      if ("er_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,4]) }
      if ("er_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[5,4]) }
      if ("er_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,5] / sum(lh[5,3])) }
      if ("er_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,6] / sum(lh[5,3])) }
      if ("er_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,7] / sum(lh[5,3])) }
      if ("er_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,8] / sum(lh[5,3])) }
      if ("er_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,9] / sum(lh[5,3])) }
      if ("er_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,10] / sum(lh[5,3])) }
      
      if ("tp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,2]) + sum(rh[32,2]) }
      if ("tp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3]) + sum(rh[32,3]) }
      if ("tp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,4]) + sum(rh[32,4]) }
      if ("tp_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[32,4]) + sum(wm[32+34,4]) }
      if ("tp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,5] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,5] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,6] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,6] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,7] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,7] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,8] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,8] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,9] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,9] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,10] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,10] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,2]) }
      if ("tp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3]) }
      if ("tp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,4]) }
      if ("tp_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[32+34,4]) }
      if ("tp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,5] / sum(rh[32,3])) }
      if ("tp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,6] / sum(rh[32,3])) }
      if ("tp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,7] / sum(rh[32,3])) }
      if ("tp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,8] / sum(rh[32,3])) }
      if ("tp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,9] / sum(rh[32,3])) }
      if ("tp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,10] / sum(rh[32,3])) }
      if ("tp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,2]) }
      if ("tp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3]) }
      if ("tp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,4]) }
      if ("tp_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[32,4]) }
      if ("tp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,5] / sum(lh[32,3])) }
      if ("tp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,6] / sum(lh[32,3])) }
      if ("tp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,7] / sum(lh[32,3])) }
      if ("tp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,8] / sum(lh[32,3])) }
      if ("tp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,9] / sum(lh[32,3])) }
      if ("tp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,10] / sum(lh[32,3])) }
      
      if ("cun_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,2]) + sum(rh[4,2]) }
      if ("cun_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3]) + sum(rh[4,3]) }
      if ("cun_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,4]) + sum(rh[4,4]) }
      if ("cun_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[4,4]) + sum(wm[4+34,4]) }
      if ("cun_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,5] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,5] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,6] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,6] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,7] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,7] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,8] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,8] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,9] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,9] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,10] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,10] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,2]) }
      if ("cun_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3]) }
      if ("cun_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,4]) }
      if ("cun_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[4+34,4]) }
      if ("cun_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,5] / sum(rh[4,3])) }
      if ("cun_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,6] / sum(rh[4,3])) }
      if ("cun_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,7] / sum(rh[4,3])) }
      if ("cun_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,8] / sum(rh[4,3])) }
      if ("cun_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,9] / sum(rh[4,3])) }
      if ("cun_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,10] / sum(rh[4,3])) }
      if ("cun_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,2]) }
      if ("cun_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3]) }
      if ("cun_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,4]) }
      if ("cun_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[4,4]) }
      if ("cun_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,5] / sum(lh[4,3])) }
      if ("cun_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,6] / sum(lh[4,3])) }
      if ("cun_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,7] / sum(lh[4,3])) }
      if ("cun_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,8] / sum(lh[4,3])) }
      if ("cun_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,9] / sum(lh[4,3])) }
      if ("cun_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,10] / sum(lh[4,3])) }
      
      if ("lo_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,2]) + sum(rh[10,2]) }
      if ("lo_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3]) + sum(rh[10,3]) }
      if ("lo_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,4]) + sum(rh[10,4]) }
      if ("lo_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[10,4]) + sum(wm[10+34,4]) }
      if ("lo_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,5] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,5] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,6] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,6] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,7] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,7] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,8] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,8] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,9] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,9] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,10] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,10] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,2]) }
      if ("lo_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3]) }
      if ("lo_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,4]) }
      if ("lo_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[10+34,4]) }
      if ("lo_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,5] / sum(rh[10,3])) }
      if ("lo_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,6] / sum(rh[10,3])) }
      if ("lo_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,7] / sum(rh[10,3])) }
      if ("lo_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,8] / sum(rh[10,3])) }
      if ("lo_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,9] / sum(rh[10,3])) }
      if ("lo_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,10] / sum(rh[10,3])) }
      if ("lo_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,2]) }
      if ("lo_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3]) }
      if ("lo_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,4]) }
      if ("lo_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[10,4]) }
      if ("lo_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,5] / sum(lh[10,3])) }
      if ("lo_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,6] / sum(lh[10,3])) }
      if ("lo_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,7] / sum(lh[10,3])) }
      if ("lo_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,8] / sum(lh[10,3])) }
      if ("lo_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,9] / sum(lh[10,3])) }
      if ("lo_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,10] / sum(lh[10,3])) }
      
      if ("ling_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,2]) + sum(rh[12,2]) }
      if ("ling_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3]) + sum(rh[12,3]) }
      if ("ling_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,4]) + sum(rh[12,4]) }
      if ("ling_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[12,4]) + sum(wm[12+34,4]) }
      if ("ling_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,5] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,5] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,6] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,6] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,7] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,7] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,8] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,8] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,9] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,9] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,10] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,10] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,2]) }
      if ("ling_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3]) }
      if ("ling_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,4]) }
      if ("ling_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[12+34,4]) }
      if ("ling_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,5] / sum(rh[12,3])) }
      if ("ling_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,6] / sum(rh[12,3])) }
      if ("ling_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,7] / sum(rh[12,3])) }
      if ("ling_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,8] / sum(rh[12,3])) }
      if ("ling_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,9] / sum(rh[12,3])) }
      if ("ling_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,10] / sum(rh[12,3])) }
      if ("ling_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,2]) }
      if ("ling_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3]) }
      if ("ling_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,4]) }
      if ("ling_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[12,4]) }
      if ("ling_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,5] / sum(lh[12,3])) }
      if ("ling_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,6] / sum(lh[12,3])) }
      if ("ling_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,7] / sum(lh[12,3])) }
      if ("ling_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,8] / sum(lh[12,3])) }
      if ("ling_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,9] / sum(lh[12,3])) }
      if ("ling_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,10] / sum(lh[12,3])) }
      
      if ("peric_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,2]) + sum(rh[20,2]) }
      if ("peric_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3]) + sum(rh[20,3]) }
      if ("peric_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,4]) + sum(rh[20,4]) }
      if ("peric_t_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[20,4]) + sum(wm[20+34,4]) }
      if ("peric_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,5] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,5] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,6] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,6] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,7] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,7] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,8] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,8] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,9] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,9] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,10] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,10] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,2]) }
      if ("peric_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3]) }
      if ("peric_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,4]) }
      if ("peric_r_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[20+34,4]) }
      if ("peric_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,5] / sum(rh[20,3])) }
      if ("peric_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,6] / sum(rh[20,3])) }
      if ("peric_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,7] / sum(rh[20,3])) }
      if ("peric_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,8] / sum(rh[20,3])) }
      if ("peric_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,9] / sum(rh[20,3])) }
      if ("peric_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,10] / sum(rh[20,3])) }
      if ("peric_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,2]) }
      if ("peric_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3]) }
      if ("peric_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,4]) }
      if ("peric_l_w" %in% var.names ) { out[length(out) + 1] <- sum(wm[20,4]) }
      if ("peric_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,5] / sum(lh[20,3])) }
      if ("peric_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,6] / sum(lh[20,3])) }
      if ("peric_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,7] / sum(lh[20,3])) }
      if ("peric_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,8] / sum(lh[20,3])) }
      if ("peric_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,9] / sum(lh[20,3])) }
      if ("peric_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,10] / sum(lh[20,3])) }
      
      # Sub cortical ROIs ----
      if ("cgm_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(4,22),3]) }
      if ("cgm_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(4,22),4]) }
      if ("cgm_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[4,6] * sub[4,4] + sub[22,6] * sub[22,4] ) / sum(sub[c(4,22),4]) }
      if ("cgm_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[4,7] * sub[4,4] + sub[22,7] * sub[22,4] ) / sum(sub[c(4,22),4]) }
      if ("cgm_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[4,8] * sub[4,4] + sub[22,8] * sub[22,4] ) / sum(sub[c(4,22),4]) }
      if ("cgm_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[4,9] * sub[4,4] + sub[22,9] * sub[22,4] ) / sum(sub[c(4,22),4]) }
      if ("cgm_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[4,10] * sub[4,4] + sub[22,10] * sub[22,4] ) / sum(sub[c(4,22),4]) }
      if ("cgm_r_n" %in% var.names ) { out[length(out) + 1] <- sub[22,3] }
      if ("cgm_r_v" %in% var.names ) { out[length(out) + 1] <- sub[22,4] }
      if ("cgm_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[22,6] }
      if ("cgm_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[22,7] }
      if ("cgm_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[22,8] }
      if ("cgm_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[22,9] }
      if ("cgm_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[22,10] }
      if ("cgm_l_n" %in% var.names ) { out[length(out) + 1] <- sub[4,3] }
      if ("cgm_l_v" %in% var.names ) { out[length(out) + 1] <- sub[4,4] }
      if ("cgm_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[4,6] }
      if ("cgm_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[4,7] }
      if ("cgm_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[4,8] }
      if ("cgm_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[4,9] }
      if ("cgm_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[4,10] }
      
      if ("thal_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(5,23),3]) }
      if ("thal_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(5,23),4]) }
      if ("thal_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[5,6] * sub[5,4] + sub[23,6] * sub[23,4] ) / sum(sub[c(5,23),4]) }
      if ("thal_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[5,7] * sub[5,4] + sub[23,7] * sub[23,4] ) / sum(sub[c(5,23),4]) }
      if ("thal_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[5,8] * sub[5,4] + sub[23,8] * sub[23,4] ) / sum(sub[c(5,23),4]) }
      if ("thal_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[5,9] * sub[5,4] + sub[23,9] * sub[23,4] ) / sum(sub[c(5,23),4]) }
      if ("thal_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[5,10] * sub[5,4] + sub[23,10] * sub[23,4] ) / sum(sub[c(5,23),4]) }
      if ("thal_r_n" %in% var.names ) { out[length(out) + 1] <- sub[23,3] }
      if ("thal_r_v" %in% var.names ) { out[length(out) + 1] <- sub[23,4] }
      if ("thal_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[23,6] }
      if ("thal_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[23,7] }
      if ("thal_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[23,8] }
      if ("thal_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[23,9] }
      if ("thal_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[23,10] }
      if ("thal_l_n" %in% var.names ) { out[length(out) + 1] <- sub[5,3] }
      if ("thal_l_v" %in% var.names ) { out[length(out) + 1] <- sub[5,4] }
      if ("thal_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[5,6] }
      if ("thal_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[5,7] }
      if ("thal_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[5,8] }
      if ("thal_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[5,9] }
      if ("thal_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[5,10] }
      
      if ("caud_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(6,24),3]) }
      if ("caud_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(6,24),4]) }
      if ("caud_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[6,6] * sub[6,4] + sub[24,6] * sub[24,4] ) / sum(sub[c(6,24),4]) }
      if ("caud_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[6,7] * sub[6,4] + sub[24,7] * sub[24,4] ) / sum(sub[c(6,24),4]) }
      if ("caud_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[6,8] * sub[6,4] + sub[24,8] * sub[24,4] ) / sum(sub[c(6,24),4]) }
      if ("caud_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[6,9] * sub[6,4] + sub[24,9] * sub[24,4] ) / sum(sub[c(6,24),4]) }
      if ("caud_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[6,10] * sub[6,4] + sub[24,10] * sub[24,4] ) / sum(sub[c(6,24),4]) }
      if ("caud_r_n" %in% var.names ) { out[length(out) + 1] <- sub[24,3] }
      if ("caud_r_v" %in% var.names ) { out[length(out) + 1] <- sub[24,4] }
      if ("caud_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[24,6] }
      if ("caud_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[24,7] }
      if ("caud_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[24,8] }
      if ("caud_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[24,9] }
      if ("caud_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[24,10] }
      if ("caud_l_n" %in% var.names ) { out[length(out) + 1] <- sub[6,3] }
      if ("caud_l_v" %in% var.names ) { out[length(out) + 1] <- sub[6,4] }
      if ("caud_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[6,6] }
      if ("caud_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[6,7] }
      if ("caud_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[6,8] }
      if ("caud_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[6,9] }
      if ("caud_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[6,10] }
      
      if ("put_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(7,25),3]) }
      if ("put_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(7,25),4]) }
      if ("put_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[7,6] * sub[7,4] + sub[25,6] * sub[25,4] ) / sum(sub[c(7,25),4]) }
      if ("put_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[7,7] * sub[7,4] + sub[25,7] * sub[25,4] ) / sum(sub[c(7,25),4]) }
      if ("put_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[7,8] * sub[7,4] + sub[25,8] * sub[25,4] ) / sum(sub[c(7,25),4]) }
      if ("put_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[7,9] * sub[7,4] + sub[25,9] * sub[25,4] ) / sum(sub[c(7,25),4]) }
      if ("put_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[7,10] * sub[7,4] + sub[25,10] * sub[25,4] ) / sum(sub[c(7,25),4]) }
      if ("put_r_n" %in% var.names ) { out[length(out) + 1] <- sub[25,3] }
      if ("put_r_v" %in% var.names ) { out[length(out) + 1] <- sub[25,4] }
      if ("put_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[25,6] }
      if ("put_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[25,7] }
      if ("put_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[25,8] }
      if ("put_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[25,9] }
      if ("put_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[25,10] }
      if ("put_l_n" %in% var.names ) { out[length(out) + 1] <- sub[7,3] }
      if ("put_l_v" %in% var.names ) { out[length(out) + 1] <- sub[7,4] }
      if ("put_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[7,6] }
      if ("put_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[7,7] }
      if ("put_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[7,8] }
      if ("put_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[7,9] }
      if ("put_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[7,10] }
      
      if ("pall_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(8,26),3]) }
      if ("pall_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(8,26),4]) }
      if ("pall_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[8,6] * sub[8,4] + sub[26,6] * sub[26,4] ) / sum(sub[c(8,26),4]) }
      if ("pall_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[8,7] * sub[8,4] + sub[26,7] * sub[26,4] ) / sum(sub[c(8,26),4]) }
      if ("pall_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[8,8] * sub[8,4] + sub[26,8] * sub[26,4] ) / sum(sub[c(8,26),4]) }
      if ("pall_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[8,9] * sub[8,4] + sub[26,9] * sub[26,4] ) / sum(sub[c(8,26),4]) }
      if ("pall_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[8,10] * sub[8,4] + sub[26,10] * sub[26,4] ) / sum(sub[c(8,26),4]) }
      if ("pall_r_n" %in% var.names ) { out[length(out) + 1] <- sub[26,3] }
      if ("pall_r_v" %in% var.names ) { out[length(out) + 1] <- sub[26,4] }
      if ("pall_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[26,6] }
      if ("pall_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[26,7] }
      if ("pall_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[26,8] }
      if ("pall_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[26,9] }
      if ("pall_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[26,10] }
      if ("pall_l_n" %in% var.names ) { out[length(out) + 1] <- sub[8,3] }
      if ("pall_l_v" %in% var.names ) { out[length(out) + 1] <- sub[8,4] }
      if ("pall_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[8,6] }
      if ("pall_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[8,7] }
      if ("pall_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[8,8] }
      if ("pall_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[8,9] }
      if ("pall_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[8,10] }
      
      if ("bs_n" %in% var.names ) { out[length(out) + 1] <- sub[11,3] }
      if ("bs_v" %in% var.names ) { out[length(out) + 1] <- sub[11,4] }
      if ("bs_nm" %in% var.names ) { out[length(out) + 1] <- sub[11,6] }
      if ("bs_nsd" %in% var.names ) { out[length(out) + 1] <- sub[11,7] }
      if ("bs_nmin" %in% var.names ) { out[length(out) + 1] <- sub[11,8] }
      if ("bs_nmax" %in% var.names ) { out[length(out) + 1] <- sub[11,9] }
      if ("bs_nrng" %in% var.names ) { out[length(out) + 1] <- sub[11,10] }
      
      if ("hpc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(12,27),3]) }
      if ("hpc_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(12,27),4]) }
      if ("hpc_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[12,6] * sub[12,4] + sub[27,6] * sub[27,4] ) / sum(sub[c(12,27),4]) }
      if ("hpc_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[12,7] * sub[12,4] + sub[27,7] * sub[27,4] ) / sum(sub[c(12,27),4]) }
      if ("hpc_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[12,8] * sub[12,4] + sub[27,8] * sub[27,4] ) / sum(sub[c(12,27),4]) }
      if ("hpc_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[12,9] * sub[12,4] + sub[27,9] * sub[27,4] ) / sum(sub[c(12,27),4]) }
      if ("hpc_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[12,10] * sub[12,4] + sub[27,10] * sub[27,4] ) / sum(sub[c(12,27),4]) }
      if ("hpc_r_n" %in% var.names ) { out[length(out) + 1] <- sub[27,3] }
      if ("hpc_r_v" %in% var.names ) { out[length(out) + 1] <- sub[27,4] }
      if ("hpc_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[27,6] }
      if ("hpc_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[27,7] }
      if ("hpc_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[27,8] }
      if ("hpc_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[27,9] }
      if ("hpc_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[27,10] }
      if ("hpc_l_n" %in% var.names ) { out[length(out) + 1] <- sub[12,3] }
      if ("hpc_l_v" %in% var.names ) { out[length(out) + 1] <- sub[12,4] }
      if ("hpc_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[12,6] }
      if ("hpc_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[12,7] }
      if ("hpc_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[12,8] }
      if ("hpc_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[12,9] }
      if ("hpc_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[12,10] }
      
      if ("amg_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(13,28),3]) }
      if ("amg_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(13,28),4]) }
      if ("amg_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[13,6] * sub[13,4] + sub[28,6] * sub[28,4] ) / sum(sub[c(13,28),4]) }
      if ("amg_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[13,7] * sub[13,4] + sub[28,7] * sub[28,4] ) / sum(sub[c(13,28),4]) }
      if ("amg_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[13,8] * sub[13,4] + sub[28,8] * sub[28,4] ) / sum(sub[c(13,28),4]) }
      if ("amg_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[13,9] * sub[13,4] + sub[28,9] * sub[28,4] ) / sum(sub[c(13,28),4]) }
      if ("amg_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[13,10] * sub[13,4] + sub[28,10] * sub[28,4] ) / sum(sub[c(13,28),4]) }
      if ("amg_r_n" %in% var.names ) { out[length(out) + 1] <- sub[28,3] }
      if ("amg_r_v" %in% var.names ) { out[length(out) + 1] <- sub[28,4] }
      if ("amg_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[28,6] }
      if ("amg_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[28,7] }
      if ("amg_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[28,8] }
      if ("amg_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[28,9] }
      if ("amg_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[28,10] }
      if ("amg_l_n" %in% var.names ) { out[length(out) + 1] <- sub[13,3] }
      if ("amg_l_v" %in% var.names ) { out[length(out) + 1] <- sub[13,4] }
      if ("amg_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[13,6] }
      if ("amg_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[13,7] }
      if ("amg_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[13,8] }
      if ("amg_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[13,9] }
      if ("amg_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[13,10] }
      
      if ("acc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(15,29),3]) }
      if ("acc_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(15,29),4]) }
      if ("acc_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[15,6] * sub[15,4] + sub[29,6] * sub[29,4] ) / sum(sub[c(15,29),4]) }
      if ("acc_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[15,7] * sub[15,4] + sub[29,7] * sub[29,4] ) / sum(sub[c(15,29),4]) }
      if ("acc_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[15,8] * sub[15,4] + sub[29,8] * sub[29,4] ) / sum(sub[c(15,29),4]) }
      if ("acc_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[15,9] * sub[15,4] + sub[29,9] * sub[29,4] ) / sum(sub[c(15,29),4]) }
      if ("acc_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[15,10] * sub[15,4] + sub[29,10] * sub[29,4] ) / sum(sub[c(15,29),4]) }
      if ("acc_r_n" %in% var.names ) { out[length(out) + 1] <- sub[29,3] }
      if ("acc_r_v" %in% var.names ) { out[length(out) + 1] <- sub[29,4] }
      if ("acc_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[29,6] }
      if ("acc_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[29,7] }
      if ("acc_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[29,8] }
      if ("acc_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[29,9] }
      if ("acc_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[29,10] }
      if ("acc_l_n" %in% var.names ) { out[length(out) + 1] <- sub[15,3] }
      if ("acc_l_v" %in% var.names ) { out[length(out) + 1] <- sub[15,4] }
      if ("acc_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[15,6] }
      if ("acc_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[15,7] }
      if ("acc_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[15,8] }
      if ("acc_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[15,9] }
      if ("acc_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[15,10] }
      
      if ("vdc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(16,30),3]) }
      if ("vdc_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(16,30),4]) }
      if ("vdc_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[16,6] * sub[16,4] + sub[30,6] * sub[30,4] ) / sum(sub[c(16,30),4]) }
      if ("vdc_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[16,7] * sub[16,4] + sub[30,7] * sub[30,4] ) / sum(sub[c(16,30),4]) }
      if ("vdc_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[16,8] * sub[16,4] + sub[30,8] * sub[30,4] ) / sum(sub[c(16,30),4]) }
      if ("vdc_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[16,9] * sub[16,4] + sub[30,9] * sub[30,4] ) / sum(sub[c(16,30),4]) }
      if ("vdc_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[16,10] * sub[16,4] + sub[30,10] * sub[30,4] ) / sum(sub[c(16,30),4]) }
      if ("vdc_r_n" %in% var.names ) { out[length(out) + 1] <- sub[30,3] }
      if ("vdc_r_v" %in% var.names ) { out[length(out) + 1] <- sub[30,4] }
      if ("vdc_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[30,6] }
      if ("vdc_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[30,7] }
      if ("vdc_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[30,8] }
      if ("vdc_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[30,9] }
      if ("vdc_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[30,10] }
      if ("vdc_l_n" %in% var.names ) { out[length(out) + 1] <- sub[16,3] }
      if ("vdc_l_v" %in% var.names ) { out[length(out) + 1] <- sub[16,4] }
      if ("vdc_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[16,6] }
      if ("vdc_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[16,7] }
      if ("vdc_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[16,8] }
      if ("vdc_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[16,9] }
      if ("vdc_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[16,10] }
      
      if ("cwm_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(3,21),3]) }
      if ("cwm_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(3,21),4]) }
      if ("cwm_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[3,6] * sub[3,4] + sub[21,6] * sub[21,4] ) / sum(sub[c(3,21),4]) }
      if ("cwm_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[3,7] * sub[3,4] + sub[21,7] * sub[21,4] ) / sum(sub[c(3,21),4]) }
      if ("cwm_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[3,8] * sub[3,4] + sub[21,8] * sub[21,4] ) / sum(sub[c(3,21),4]) }
      if ("cwm_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[3,9] * sub[3,4] + sub[21,9] * sub[21,4] ) / sum(sub[c(3,21),4]) }
      if ("cwm_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[3,10] * sub[3,4] + sub[21,10] * sub[21,4] ) / sum(sub[c(3,21),4]) }
      if ("cwm_r_n" %in% var.names ) { out[length(out) + 1] <- sub[21,3] }
      if ("cwm_r_v" %in% var.names ) { out[length(out) + 1] <- sub[21,4] }
      if ("cwm_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[21,6] }
      if ("cwm_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[21,7] }
      if ("cwm_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[21,8] }
      if ("cwm_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[21,9] }
      if ("cwm_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[21,10] }
      if ("cwm_l_n" %in% var.names ) { out[length(out) + 1] <- sub[3,3] }
      if ("cwm_l_v" %in% var.names ) { out[length(out) + 1] <- sub[3,4] }
      if ("cwm_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[3,6] }
      if ("cwm_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[3,7] }
      if ("cwm_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[3,8] }
      if ("cwm_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[3,9] }
      if ("cwm_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[3,10] }
      
      if ("oc_n" %in% var.names ) { out[length(out) + 1] <- sub[40,3] }
      if ("oc_v" %in% var.names ) { out[length(out) + 1] <- sub[40,4] }
      if ("oc_nm" %in% var.names ) { out[length(out) + 1] <- sub[40,6] }
      if ("oc_nsd" %in% var.names ) { out[length(out) + 1] <- sub[40,7] }
      if ("oc_nmin" %in% var.names ) { out[length(out) + 1] <- sub[40,8] }
      if ("oc_nmax" %in% var.names ) { out[length(out) + 1] <- sub[40,9] }
      if ("oc_nrng" %in% var.names ) { out[length(out) + 1] <- sub[40,10] }
      
      if ("ccp_n" %in% var.names ) { out[length(out) + 1] <- sub[41,3] }
      if ("ccp_v" %in% var.names ) { out[length(out) + 1] <- sub[41,4] }
      if ("ccp_nm" %in% var.names ) { out[length(out) + 1] <- sub[41,6] }
      if ("ccp_nsd" %in% var.names ) { out[length(out) + 1] <- sub[41,7] }
      if ("ccp_nmin" %in% var.names ) { out[length(out) + 1] <- sub[41,8] }
      if ("ccp_nmax" %in% var.names ) { out[length(out) + 1] <- sub[41,9] }
      if ("ccp_nrng" %in% var.names ) { out[length(out) + 1] <- sub[41,10] }
      
      if ("ccmp_n" %in% var.names ) { out[length(out) + 1] <- sub[42,3] }
      if ("ccmp_v" %in% var.names ) { out[length(out) + 1] <- sub[42,4] }
      if ("ccmp_nm" %in% var.names ) { out[length(out) + 1] <- sub[42,6] }
      if ("ccmp_nsd" %in% var.names ) { out[length(out) + 1] <- sub[42,7] }
      if ("ccmp_nmin" %in% var.names ) { out[length(out) + 1] <- sub[42,8] }
      if ("ccmp_nmax" %in% var.names ) { out[length(out) + 1] <- sub[42,9] }
      if ("ccmp_nrng" %in% var.names ) { out[length(out) + 1] <- sub[42,10] }
      
      if ("ccc_n" %in% var.names ) { out[length(out) + 1] <- sub[43,3] }
      if ("ccc_v" %in% var.names ) { out[length(out) + 1] <- sub[43,4] }
      if ("ccc_nm" %in% var.names ) { out[length(out) + 1] <- sub[43,6] }
      if ("ccc_nsd" %in% var.names ) { out[length(out) + 1] <- sub[43,7] }
      if ("ccc_nmin" %in% var.names ) { out[length(out) + 1] <- sub[43,8] }
      if ("ccc_nmax" %in% var.names ) { out[length(out) + 1] <- sub[43,9] }
      if ("ccc_nrng" %in% var.names ) { out[length(out) + 1] <- sub[43,10] }
      
      if ("ccma_n" %in% var.names ) { out[length(out) + 1] <- sub[44,3] }
      if ("ccma_v" %in% var.names ) { out[length(out) + 1] <- sub[44,4] }
      if ("ccma_nm" %in% var.names ) { out[length(out) + 1] <- sub[44,6] }
      if ("ccma_nsd" %in% var.names ) { out[length(out) + 1] <- sub[44,7] }
      if ("ccma_nmin" %in% var.names ) { out[length(out) + 1] <- sub[44,8] }
      if ("ccma_nmax" %in% var.names ) { out[length(out) + 1] <- sub[44,9] }
      if ("ccma_nrng" %in% var.names ) { out[length(out) + 1] <- sub[44,10] }
      
      if ("cca_n" %in% var.names ) { out[length(out) + 1] <- sub[45,3] }
      if ("cca_v" %in% var.names ) { out[length(out) + 1] <- sub[45,4] }
      if ("cca_nm" %in% var.names ) { out[length(out) + 1] <- sub[45,6] }
      if ("cca_nsd" %in% var.names ) { out[length(out) + 1] <- sub[45,7] }
      if ("cca_nmin" %in% var.names ) { out[length(out) + 1] <- sub[45,8] }
      if ("cca_nmax" %in% var.names ) { out[length(out) + 1] <- sub[45,9] }
      if ("cca_nrng" %in% var.names ) { out[length(out) + 1] <- sub[45,10] }
      
      if ("lv_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(1,19),3]) }
      if ("lv_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(1,19),4]) }
      if ("lv_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[1,6] * sub[1,4] + sub[19,6] * sub[19,4] ) / sum(sub[c(1,19),4]) }
      if ("lv_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[1,7] * sub[1,4] + sub[19,7] * sub[19,4] ) / sum(sub[c(1,19),4]) }
      if ("lv_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[1,8] * sub[1,4] + sub[19,8] * sub[19,4] ) / sum(sub[c(1,19),4]) }
      if ("lv_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[1,9] * sub[1,4] + sub[19,9] * sub[19,4] ) / sum(sub[c(1,19),4]) }
      if ("lv_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[1,10] * sub[1,4] + sub[19,10] * sub[19,4] ) / sum(sub[c(1,19),4]) }
      if ("lv_r_n" %in% var.names ) { out[length(out) + 1] <- sub[19,3] }
      if ("lv_r_v" %in% var.names ) { out[length(out) + 1] <- sub[19,4] }
      if ("lv_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[19,6] }
      if ("lv_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[19,7] }
      if ("lv_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[19,8] }
      if ("lv_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[19,9] }
      if ("lv_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[19,10] }
      if ("lv_l_n" %in% var.names ) { out[length(out) + 1] <- sub[1,3] }
      if ("lv_l_v" %in% var.names ) { out[length(out) + 1] <- sub[1,4] }
      if ("lv_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[1,6] }
      if ("lv_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[1,7] }
      if ("lv_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[1,8] }
      if ("lv_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[1,9] }
      if ("lv_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[1,10] }
      
      if ("ilv_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(2,20),3]) }
      if ("ilv_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(2,20),4]) }
      if ("ilv_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[2,6] * sub[2,4] + sub[20,6] * sub[20,4] ) / sum(sub[c(2,20),4]) }
      if ("ilv_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[2,7] * sub[2,4] + sub[20,7] * sub[20,4] ) / sum(sub[c(2,20),4]) }
      if ("ilv_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[2,8] * sub[2,4] + sub[20,8] * sub[20,4] ) / sum(sub[c(2,20),4]) }
      if ("ilv_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[2,9] * sub[2,4] + sub[20,9] * sub[20,4] ) / sum(sub[c(2,20),4]) }
      if ("ilv_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[2,10] * sub[2,4] + sub[20,10] * sub[20,4] ) / sum(sub[c(2,20),4]) }
      if ("ilv_r_n" %in% var.names ) { out[length(out) + 1] <- sub[20,3] }
      if ("ilv_r_v" %in% var.names ) { out[length(out) + 1] <- sub[20,4] }
      if ("ilv_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[20,6] }
      if ("ilv_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[20,7] }
      if ("ilv_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[20,8] }
      if ("ilv_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[20,9] }
      if ("ilv_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[20,10] }
      if ("ilv_l_n" %in% var.names ) { out[length(out) + 1] <- sub[2,3] }
      if ("ilv_l_v" %in% var.names ) { out[length(out) + 1] <- sub[2,4] }
      if ("ilv_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[2,6] }
      if ("ilv_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[2,7] }
      if ("ilv_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[2,8] }
      if ("ilv_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[2,9] }
      if ("ilv_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[2,10] }
      
      if ("v3_n" %in% var.names ) { out[length(out) + 1] <- sub[9,3] }
      if ("v3_v" %in% var.names ) { out[length(out) + 1] <- sub[9,4] }
      if ("v3_nm" %in% var.names ) { out[length(out) + 1] <- sub[9,6] }
      if ("v3_nsd" %in% var.names ) { out[length(out) + 1] <- sub[9,7] }
      if ("v3_nmin" %in% var.names ) { out[length(out) + 1] <- sub[9,8] }
      if ("v3_nmax" %in% var.names ) { out[length(out) + 1] <- sub[9,9] }
      if ("v3_nrng" %in% var.names ) { out[length(out) + 1] <- sub[9,10] }
      
      if ("v4_n" %in% var.names ) { out[length(out) + 1] <- sub[10,3] }
      if ("v4_v" %in% var.names ) { out[length(out) + 1] <- sub[10,4] }
      if ("v4_nm" %in% var.names ) { out[length(out) + 1] <- sub[10,6] }
      if ("v4_nsd" %in% var.names ) { out[length(out) + 1] <- sub[10,7] }
      if ("v4_nmin" %in% var.names ) { out[length(out) + 1] <- sub[10,8] }
      if ("v4_nmax" %in% var.names ) { out[length(out) + 1] <- sub[10,9] }
      if ("v4_nrng" %in% var.names ) { out[length(out) + 1] <- sub[10,10] }
      
      if ("v5_n" %in% var.names ) { out[length(out) + 1] <- sub[33,3] }
      if ("v5_v" %in% var.names ) { out[length(out) + 1] <- sub[33,4] }
      if ("v5_nm" %in% var.names ) { out[length(out) + 1] <- sub[33,6] }
      if ("v5_nsd" %in% var.names ) { out[length(out) + 1] <- sub[33,7] }
      if ("v5_nmin" %in% var.names ) { out[length(out) + 1] <- sub[33,8] }
      if ("v5_nmax" %in% var.names ) { out[length(out) + 1] <- sub[33,9] }
      if ("v5_nrng" %in% var.names ) { out[length(out) + 1] <- sub[33,10] }
      
      if ("csf_n" %in% var.names ) { out[length(out) + 1] <- sub[14,3] }
      if ("csf_v" %in% var.names ) { out[length(out) + 1] <- sub[14,4] }
      if ("csf_nm" %in% var.names ) { out[length(out) + 1] <- sub[14,6] }
      if ("csf_nsd" %in% var.names ) { out[length(out) + 1] <- sub[14,7] }
      if ("csf_nmin" %in% var.names ) { out[length(out) + 1] <- sub[14,8] }
      if ("csf_nmax" %in% var.names ) { out[length(out) + 1] <- sub[14,9] }
      if ("csf_nrng" %in% var.names ) { out[length(out) + 1] <- sub[14,10] }
      
      if ("ves_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(17,31),3]) }
      if ("ves_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(17,31),4]) }
      if ("ves_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[17,6] * sub[17,4] + sub[31,6] * sub[31,4] ) / sum(sub[c(17,31),4]) }
      if ("ves_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[17,7] * sub[17,4] + sub[31,7] * sub[31,4] ) / sum(sub[c(17,31),4]) }
      if ("ves_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[17,8] * sub[17,4] + sub[31,8] * sub[31,4] ) / sum(sub[c(17,31),4]) }
      if ("ves_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[17,9] * sub[17,4] + sub[31,9] * sub[31,4] ) / sum(sub[c(17,31),4]) }
      if ("ves_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[17,10] * sub[17,4] + sub[31,10] * sub[31,4] ) / sum(sub[c(17,31),4]) }
      if ("ves_r_n" %in% var.names ) { out[length(out) + 1] <- sub[31,3] }
      if ("ves_r_v" %in% var.names ) { out[length(out) + 1] <- sub[31,4] }
      if ("ves_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[31,6] }
      if ("ves_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[31,7] }
      if ("ves_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[31,8] }
      if ("ves_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[31,9] }
      if ("ves_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[31,10] }
      if ("ves_l_n" %in% var.names ) { out[length(out) + 1] <- sub[17,3] }
      if ("ves_l_v" %in% var.names ) { out[length(out) + 1] <- sub[17,4] }
      if ("ves_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[17,6] }
      if ("ves_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[17,7] }
      if ("ves_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[17,8] }
      if ("ves_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[17,9] }
      if ("ves_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[17,10] }
      
      if ("cp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(18,32),3]) }
      if ("cp_t_v" %in% var.names ) { out[length(out) + 1] <- sum(sub[c(18,32),4]) }
      if ("cp_t_nm" %in% var.names ) { out[length(out) + 1] <- (sub[18,6] * sub[18,4] + sub[32,6] * sub[32,4] ) / sum(sub[c(18,32),4]) }
      if ("cp_t_nsd" %in% var.names ) { out[length(out) + 1] <- (sub[18,7] * sub[18,4] + sub[32,7] * sub[32,4] ) / sum(sub[c(18,32),4]) }
      if ("cp_t_nmin" %in% var.names ) { out[length(out) + 1] <- (sub[18,8] * sub[18,4] + sub[32,8] * sub[32,4] ) / sum(sub[c(18,32),4]) }
      if ("cp_t_nmax" %in% var.names ) { out[length(out) + 1] <- (sub[18,9] * sub[18,4] + sub[32,9] * sub[32,4] ) / sum(sub[c(18,32),4]) }
      if ("cp_t_nrng" %in% var.names ) { out[length(out) + 1] <- (sub[18,10] * sub[18,4] + sub[32,10] * sub[32,4] ) / sum(sub[c(18,32),4]) }
      if ("cp_r_n" %in% var.names ) { out[length(out) + 1] <- sub[32,3] }
      if ("cp_r_v" %in% var.names ) { out[length(out) + 1] <- sub[32,4] }
      if ("cp_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[32,6] }
      if ("cp_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[32,7] }
      if ("cp_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[32,8] }
      if ("cp_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[32,9] }
      if ("cp_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[32,10] }
      if ("cp_l_n" %in% var.names ) { out[length(out) + 1] <- sub[18,3] }
      if ("cp_l_v" %in% var.names ) { out[length(out) + 1] <- sub[18,4] }
      if ("cp_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[18,6] }
      if ("cp_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[18,7] }
      if ("cp_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[18,8] }
      if ("cp_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[18,9] }
      if ("cp_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[18,10] }
      
      if ("wmhyp_t_n" %in% var.names ) { out[length(out) + 1] <- sub[34,3] }
      if ("wmhyp_t_v" %in% var.names ) { out[length(out) + 1] <- sub[34,4] }
      if ("wmhyp_t_nm" %in% var.names ) { out[length(out) + 1] <- sub[34,6] }
      if ("wmhyp_t_nsd" %in% var.names ) { out[length(out) + 1] <- sub[34,7] }
      if ("wmhyp_t_nmin" %in% var.names ) { out[length(out) + 1] <- sub[34,8] }
      if ("wmhyp_t_nmax" %in% var.names ) { out[length(out) + 1] <- sub[34,9] }
      if ("wmhyp_t_nrng" %in% var.names ) { out[length(out) + 1] <- sub[34,10] }
      
      if ("wmhyp_r_n" %in% var.names ) { out[length(out) + 1] <- sub[35,3] }
      if ("wmhyp_r_v" %in% var.names ) { out[length(out) + 1] <- sub[35,4] }
      if ("wmhyp_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[35,6] }
      if ("wmhyp_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[35,7] }
      if ("wmhyp_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[35,8] }
      if ("wmhyp_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[35,9] }
      if ("wmhyp_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[35,10] }
      
      if ("wmhyp_l_n" %in% var.names ) { out[length(out) + 1] <- sub[36,3] }
      if ("wmhyp_l_v" %in% var.names ) { out[length(out) + 1] <- sub[36,4] }
      if ("wmhyp_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[36,6] }
      if ("wmhyp_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[36,7] }
      if ("wmhyp_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[36,8] }
      if ("wmhyp_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[36,9] }
      if ("wmhyp_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[36,10] }
      
      if ("nwmhyp_t_n" %in% var.names ) { out[length(out) + 1] <- sub[37,3] }
      if ("nwmhyp_t_v" %in% var.names ) { out[length(out) + 1] <- sub[37,4] }
      if ("nwmhyp_t_nm" %in% var.names ) { out[length(out) + 1] <- sub[37,6] }
      if ("nwmhyp_t_nsd" %in% var.names ) { out[length(out) + 1] <- sub[37,7] }
      if ("nwmhyp_t_nmin" %in% var.names ) { out[length(out) + 1] <- sub[37,8] }
      if ("nwmhyp_t_nmax" %in% var.names ) { out[length(out) + 1] <- sub[37,9] }
      if ("nwmhyp_t_nrng" %in% var.names ) { out[length(out) + 1] <- sub[37,10] }
      
      if ("nwmhyp_r_n" %in% var.names ) { out[length(out) + 1] <- sub[38,3] }
      if ("nwmhyp_r_v" %in% var.names ) { out[length(out) + 1] <- sub[38,4] }
      if ("nwmhyp_r_nm" %in% var.names ) { out[length(out) + 1] <- sub[38,6] }
      if ("nwmhyp_r_nsd" %in% var.names ) { out[length(out) + 1] <- sub[38,7] }
      if ("nwmhyp_r_nmin" %in% var.names ) { out[length(out) + 1] <- sub[38,8] }
      if ("nwmhyp_r_nmax" %in% var.names ) { out[length(out) + 1] <- sub[38,9] }
      if ("nwmhyp_r_nrng" %in% var.names ) { out[length(out) + 1] <- sub[38,10] }
      
      if ("nwmhyp_l_n" %in% var.names ) { out[length(out) + 1] <- sub[39,3] }
      if ("nwmhyp_l_v" %in% var.names ) { out[length(out) + 1] <- sub[39,4] }
      if ("nwmhyp_l_nm" %in% var.names ) { out[length(out) + 1] <- sub[39,6] }
      if ("nwmhyp_l_nsd" %in% var.names ) { out[length(out) + 1] <- sub[39,7] }
      if ("nwmhyp_l_nmin" %in% var.names ) { out[length(out) + 1] <- sub[39,8] }
      if ("nwmhyp_l_nmax" %in% var.names ) { out[length(out) + 1] <- sub[39,9] }
      if ("nwmhyp_l_nrng" %in% var.names ) { out[length(out) + 1] <- sub[39,10] }
      
      # ----
      df[i,2:ncol(df)] <- out
    } else {
      df[i,2:ncol(df)] <- NA
    }
  }
  
  if (save.csv) {
    if (is.null(save.dir)) {
      save.dir <- data.dir
    }
    if (is.null(file.name)) {
      write.table(df, file=paste0(save.dir, "/fsurf.summary.", format(Sys.time(), "%Y%m%d"), ".csv"),
                  quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",")
    } else {
      write.table(df, file=paste0(save.dir, "/", file.name, ".csv"),
                  quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",")
    }
    
  }
  
  
  if (return.df) {
    return(df)
  }
  
}



