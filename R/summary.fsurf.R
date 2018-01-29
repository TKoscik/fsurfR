summary.fsurf <- function(data.dir,
                          vars = c("sa", "g", "ta", "mc"),
                          rois = c("peg", "all"),
                          sjx = "all",
                          hemi = c("t", "r", "l"),
                          save.csv=TRUE,
                          save.dir = NULL,
                          return.df = FALSE) {

  if (sjx[1] == "all") {
    sjx <- list.dirs(data.dir, full.names = FALSE, recursive = FALSE)
    if ("fsaverage" %in% sjx) { sjx <- sjx[-which(sjx == "fsaverage")] }
    if ("qdec" %in% sjx) { sjx <- sjx[-which(sjx == "qdec")] }
  }
  n.sjx <- length(sjx)
  
  if (vars[1] == "all") {
    vars <- c("n", "sa", "g", "ta", "tsd", "mc", "gc", "fi", "ci")
  }
  
  if (any(c("peg", "ell", "all") %in% rois)) {
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
    rois <- c(rois, rois.temp)
    rois <- unique(rois)
  }
  
  ### order is messed up
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
  
  df <- data.frame(id=sjx, matrix(as.numeric(NA), nrow=n.sjx, ncol=length(var.names)))
  colnames(df) <- c("id", var.names)
  
  for (i in 1:n.sjx) {
    skip.sjx <- 1
    if ("t" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))) {
        # lh <- read.delim(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"),
        #                  header=F, sep="", skip=60)
        lh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))
      } else {
        skip.sjx <- 0
      }
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))) {
        # rh <- read.delim(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"),
        #                  header=F, sep="", skip=60)
        rh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))
      } else {
        skip.sjx <- 0
      }
    } else if ("l" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))) {
        # lh <- read.delim(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"),
        #                  header=F, sep="", skip=60)
        lh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/lh.aparc.stats"))
      } else {
        skip.sjx <- 0
      }
    } else if ("r" %in% hemi) {
      if (file.exists(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))) {
        # rh <- read.delim(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"),
        #                  header=F, sep="", skip=60)
        rh <- read.fsurf.stats(paste0(data.dir, "/", sjx[i], "/stats/rh.aparc.stats"))
      } else {
        skip.sjx <- 0
      }
    }
    
    if (skip.sjx) {
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
      if ("frnt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,5] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,5] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,6] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,6] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,7] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,7] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,8] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,8] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,9] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,9] / sum(lh[f.num,3] + rh[f.num,3])) }
      if ("frnt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3] * lh[f.num,10] / sum(lh[f.num,3] + rh[f.num,3])) + sum(rh[f.num,3] * rh[f.num,10] / sum(lh[f.num,3] + rh[f.num,3])) }
      
      if ("frnt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,2]) }
      if ("frnt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3]) }
      if ("frnt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,4]) }
      if ("frnt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,5] / sum(rh[f.num,3])) }
      if ("frnt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,6] / sum(rh[f.num,3])) }
      if ("frnt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,7] / sum(rh[f.num,3])) }
      if ("frnt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,8] / sum(rh[f.num,3])) }
      if ("frnt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,9] / sum(rh[f.num,3])) }
      if ("frnt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[f.num,3] * rh[f.num,10] / sum(rh[f.num,3])) }
      
      if ("frnt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,2]) }
      if ("frnt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,3]) }
      if ("frnt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[f.num,4]) }
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
      if ("par_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,5] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,5] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,6] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,6] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,7] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,7] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,8] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,8] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,9] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,9] / sum(lh[p.num,3] + rh[p.num,3])) }
      if ("par_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3] * lh[p.num,10] / sum(lh[p.num,3] + rh[p.num,3])) + sum(rh[p.num,3] * rh[p.num,10] / sum(lh[p.num,3] + rh[p.num,3])) }
      
      if ("par_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,2]) }
      if ("par_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3]) }
      if ("par_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,4]) }
      if ("par_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,5] / sum(rh[p.num,3])) }
      if ("par_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,6] / sum(rh[p.num,3])) }
      if ("par_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,7] / sum(rh[p.num,3])) }
      if ("par_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,8] / sum(rh[p.num,3])) }
      if ("par_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,9] / sum(rh[p.num,3])) }
      if ("par_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[p.num,3] * rh[p.num,10] / sum(rh[p.num,3])) }
      
      if ("par_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,2]) }
      if ("par_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,3]) }
      if ("par_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[p.num,4]) }
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
      if ("temp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,5] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,5] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,6] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,6] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,7] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,7] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,8] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,8] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,9] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,9] / sum(lh[t.num,3] + rh[t.num,3])) }
      if ("temp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3] * lh[t.num,10] / sum(lh[t.num,3] + rh[t.num,3])) + sum(rh[t.num,3] * rh[t.num,10] / sum(lh[t.num,3] + rh[t.num,3])) }
      
      if ("temp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,2]) }
      if ("temp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3]) }
      if ("temp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,4]) }
      if ("temp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,5] / sum(rh[t.num,3])) }
      if ("temp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,6] / sum(rh[t.num,3])) }
      if ("temp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,7] / sum(rh[t.num,3])) }
      if ("temp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,8] / sum(rh[t.num,3])) }
      if ("temp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,9] / sum(rh[t.num,3])) }
      if ("temp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[t.num,3] * rh[t.num,10] / sum(rh[t.num,3])) }
      
      if ("temp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,2]) }
      if ("temp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,3]) }
      if ("temp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[t.num,4]) }
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
      if ("occ_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,5] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,5] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,6] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,6] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,7] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,7] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,8] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,8] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,9] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,9] / sum(lh[o.num,3] + rh[o.num,3])) }
      if ("occ_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3] * lh[o.num,10] / sum(lh[o.num,3] + rh[o.num,3])) + sum(rh[o.num,3] * rh[o.num,10] / sum(lh[o.num,3] + rh[o.num,3])) }
      
      if ("occ_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,2]) }
      if ("occ_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3]) }
      if ("occ_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,4]) }
      if ("occ_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,5] / sum(rh[o.num,3])) }
      if ("occ_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,6] / sum(rh[o.num,3])) }
      if ("occ_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,7] / sum(rh[o.num,3])) }
      if ("occ_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,8] / sum(rh[o.num,3])) }
      if ("occ_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,9] / sum(rh[o.num,3])) }
      if ("occ_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[o.num,3] * rh[o.num,10] / sum(rh[o.num,3])) }
      
      if ("occ_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,2]) }
      if ("occ_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,3]) }
      if ("occ_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[o.num,4]) }
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
      if ("cing_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,5] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,5] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,6] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,6] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,7] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,7] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,8] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,8] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,9] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,9] / sum(lh[c.num,3] + rh[c.num,3])) }
      if ("cing_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,10] / sum(lh[c.num,3] + rh[c.num,3])) + sum(rh[c.num,3] * rh[c.num,10] / sum(lh[c.num,3] + rh[c.num,3])) }
      
      if ("cing_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,2]) }
      if ("cing_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3]) }
      if ("cing_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,4]) }
      if ("cing_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,5] / sum(rh[c.num,3])) }
      if ("cing_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,6] / sum(rh[c.num,3])) }
      if ("cing_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,7] / sum(rh[c.num,3])) }
      if ("cing_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,8] / sum(rh[c.num,3])) }
      if ("cing_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,9] / sum(rh[c.num,3])) }
      if ("cing_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[c.num,3] * rh[c.num,10] / sum(rh[c.num,3])) }
      
      if ("cing_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,2]) }
      if ("cing_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3]) }
      if ("cing_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,4]) }
      if ("cing_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,5] / sum(lh[c.num,3])) }
      if ("cing_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,6] / sum(lh[c.num,3])) }
      if ("cing_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,7] / sum(lh[c.num,3])) }
      if ("cing_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,8] / sum(lh[c.num,3])) }
      if ("cing_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,9] / sum(lh[c.num,3])) }
      if ("cing_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[c.num,3] * lh[c.num,10] / sum(lh[c.num,3])) }
      
      # ROIs
      if ("cmf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,2]) + sum(rh[3,2]) }
      if ("cmf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3]) + sum(rh[3,3]) }
      if ("cmf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,4]) + sum(rh[3,4]) }
      if ("cmf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,5] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,5] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,6] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,6] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,7] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,7] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,8] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,8] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,9] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,9] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,10] / sum(lh[3,3] + rh[3,3])) + sum(rh[3,3] * rh[3,10] / sum(lh[3,3] + rh[3,3])) }
      if ("cmf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,2]) }
      if ("cmf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3]) }
      if ("cmf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,4]) }
      if ("cmf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,5] / sum(rh[3,3])) }
      if ("cmf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,6] / sum(rh[3,3])) }
      if ("cmf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,7] / sum(rh[3,3])) }
      if ("cmf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,8] / sum(rh[3,3])) }
      if ("cmf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,9] / sum(rh[3,3])) }
      if ("cmf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[3,3] * rh[3,10] / sum(rh[3,3])) }
      if ("cmf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,2]) }
      if ("cmf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3]) }
      if ("cmf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,4]) }
      if ("cmf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,5] / sum(lh[3,3])) }
      if ("cmf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,6] / sum(lh[3,3])) }
      if ("cmf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,7] / sum(lh[3,3])) }
      if ("cmf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,8] / sum(lh[3,3])) }
      if ("cmf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,9] / sum(lh[3,3])) }
      if ("cmf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[3,3] * lh[3,10] / sum(lh[3,3])) }
      if ("lof_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,2]) + sum(rh[11,2]) }
      if ("lof_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3]) + sum(rh[11,3]) }
      if ("lof_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,4]) + sum(rh[11,4]) }
      if ("lof_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,5] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,5] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,6] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,6] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,7] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,7] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,8] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,8] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,9] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,9] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,10] / sum(lh[11,3] + rh[11,3])) + sum(rh[11,3] * rh[11,10] / sum(lh[11,3] + rh[11,3])) }
      if ("lof_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,2]) }
      if ("lof_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3]) }
      if ("lof_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,4]) }
      if ("lof_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,5] / sum(rh[11,3])) }
      if ("lof_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,6] / sum(rh[11,3])) }
      if ("lof_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,7] / sum(rh[11,3])) }
      if ("lof_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,8] / sum(rh[11,3])) }
      if ("lof_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,9] / sum(rh[11,3])) }
      if ("lof_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[11,3] * rh[11,10] / sum(rh[11,3])) }
      if ("lof_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,2]) }
      if ("lof_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3]) }
      if ("lof_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,4]) }
      if ("lof_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,5] / sum(lh[11,3])) }
      if ("lof_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,6] / sum(lh[11,3])) }
      if ("lof_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,7] / sum(lh[11,3])) }
      if ("lof_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,8] / sum(lh[11,3])) }
      if ("lof_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,9] / sum(lh[11,3])) }
      if ("lof_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[11,3] * lh[11,10] / sum(lh[11,3])) }
      if ("mof_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,2]) + sum(rh[13,2]) }
      if ("mof_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3]) + sum(rh[13,3]) }
      if ("mof_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,4]) + sum(rh[13,4]) }
      if ("mof_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,5] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,5] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,6] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,6] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,7] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,7] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,8] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,8] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,9] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,9] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,10] / sum(lh[13,3] + rh[13,3])) + sum(rh[13,3] * rh[13,10] / sum(lh[13,3] + rh[13,3])) }
      if ("mof_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,2]) }
      if ("mof_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3]) }
      if ("mof_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,4]) }
      if ("mof_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,5] / sum(rh[13,3])) }
      if ("mof_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,6] / sum(rh[13,3])) }
      if ("mof_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,7] / sum(rh[13,3])) }
      if ("mof_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,8] / sum(rh[13,3])) }
      if ("mof_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,9] / sum(rh[13,3])) }
      if ("mof_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[13,3] * rh[13,10] / sum(rh[13,3])) }
      if ("mof_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,2]) }
      if ("mof_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3]) }
      if ("mof_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,4]) }
      if ("mof_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,5] / sum(lh[13,3])) }
      if ("mof_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,6] / sum(lh[13,3])) }
      if ("mof_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,7] / sum(lh[13,3])) }
      if ("mof_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,8] / sum(lh[13,3])) }
      if ("mof_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,9] / sum(lh[13,3])) }
      if ("mof_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[13,3] * lh[13,10] / sum(lh[13,3])) }
      if ("fp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,2]) + sum(rh[31,2]) }
      if ("fp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3]) + sum(rh[31,3]) }
      if ("fp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,4]) + sum(rh[31,4]) }
      if ("fp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,5] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,5] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,6] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,6] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,7] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,7] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,8] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,8] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,9] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,9] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,10] / sum(lh[31,3] + rh[31,3])) + sum(rh[31,3] * rh[31,10] / sum(lh[31,3] + rh[31,3])) }
      if ("fp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,2]) }
      if ("fp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3]) }
      if ("fp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,4]) }
      if ("fp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,5] / sum(rh[31,3])) }
      if ("fp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,6] / sum(rh[31,3])) }
      if ("fp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,7] / sum(rh[31,3])) }
      if ("fp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,8] / sum(rh[31,3])) }
      if ("fp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,9] / sum(rh[31,3])) }
      if ("fp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[31,3] * rh[31,10] / sum(rh[31,3])) }
      if ("fp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,2]) }
      if ("fp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3]) }
      if ("fp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,4]) }
      if ("fp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,5] / sum(lh[31,3])) }
      if ("fp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,6] / sum(lh[31,3])) }
      if ("fp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,7] / sum(lh[31,3])) }
      if ("fp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,8] / sum(lh[31,3])) }
      if ("fp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,9] / sum(lh[31,3])) }
      if ("fp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[31,3] * lh[31,10] / sum(lh[31,3])) }
      if ("po_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,2]) + sum(rh[18,2]) }
      if ("po_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3]) + sum(rh[18,3]) }
      if ("po_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,4]) + sum(rh[18,4]) }
      if ("po_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,5] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,5] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,6] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,6] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,7] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,7] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,8] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,8] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,9] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,9] / sum(lh[18,3] + rh[18,3])) }
      if ("po_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,10] / sum(lh[18,3] + rh[18,3])) + sum(rh[18,3] * rh[18,10] / sum(lh[18,3] + rh[18,3])) }
      if ("po_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,2]) }
      if ("po_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3]) }
      if ("po_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,4]) }
      if ("po_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,5] / sum(rh[18,3])) }
      if ("po_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,6] / sum(rh[18,3])) }
      if ("po_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,7] / sum(rh[18,3])) }
      if ("po_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,8] / sum(rh[18,3])) }
      if ("po_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,9] / sum(rh[18,3])) }
      if ("po_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[18,3] * rh[18,10] / sum(rh[18,3])) }
      if ("po_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,2]) }
      if ("po_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3]) }
      if ("po_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,4]) }
      if ("po_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,5] / sum(lh[18,3])) }
      if ("po_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,6] / sum(lh[18,3])) }
      if ("po_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,7] / sum(lh[18,3])) }
      if ("po_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,8] / sum(lh[18,3])) }
      if ("po_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,9] / sum(lh[18,3])) }
      if ("po_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[18,3] * lh[18,10] / sum(lh[18,3])) }
      if ("pt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,2]) + sum(rh[19,2]) }
      if ("pt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3]) + sum(rh[19,3]) }
      if ("pt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,4]) + sum(rh[19,4]) }
      if ("pt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,5] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,5] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,6] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,6] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,7] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,7] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,8] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,8] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,9] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,9] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,10] / sum(lh[19,3] + rh[19,3])) + sum(rh[19,3] * rh[19,10] / sum(lh[19,3] + rh[19,3])) }
      if ("pt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,2]) }
      if ("pt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3]) }
      if ("pt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,4]) }
      if ("pt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,5] / sum(rh[19,3])) }
      if ("pt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,6] / sum(rh[19,3])) }
      if ("pt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,7] / sum(rh[19,3])) }
      if ("pt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,8] / sum(rh[19,3])) }
      if ("pt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,9] / sum(rh[19,3])) }
      if ("pt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[19,3] * rh[19,10] / sum(rh[19,3])) }
      if ("pt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,2]) }
      if ("pt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3]) }
      if ("pt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,4]) }
      if ("pt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,5] / sum(lh[19,3])) }
      if ("pt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,6] / sum(lh[19,3])) }
      if ("pt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,7] / sum(lh[19,3])) }
      if ("pt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,8] / sum(lh[19,3])) }
      if ("pt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,9] / sum(lh[19,3])) }
      if ("pt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[19,3] * lh[19,10] / sum(lh[19,3])) }
      if ("rmf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,2]) + sum(rh[26,2]) }
      if ("rmf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3]) + sum(rh[26,3]) }
      if ("rmf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,4]) + sum(rh[26,4]) }
      if ("rmf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,5] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,5] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,6] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,6] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,7] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,7] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,8] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,8] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,9] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,9] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,10] / sum(lh[26,3] + rh[26,3])) + sum(rh[26,3] * rh[26,10] / sum(lh[26,3] + rh[26,3])) }
      if ("rmf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,2]) }
      if ("rmf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3]) }
      if ("rmf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,4]) }
      if ("rmf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,5] / sum(rh[26,3])) }
      if ("rmf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,6] / sum(rh[26,3])) }
      if ("rmf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,7] / sum(rh[26,3])) }
      if ("rmf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,8] / sum(rh[26,3])) }
      if ("rmf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,9] / sum(rh[26,3])) }
      if ("rmf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[26,3] * rh[26,10] / sum(rh[26,3])) }
      if ("rmf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,2]) }
      if ("rmf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3]) }
      if ("rmf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,4]) }
      if ("rmf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,5] / sum(lh[26,3])) }
      if ("rmf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,6] / sum(lh[26,3])) }
      if ("rmf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,7] / sum(lh[26,3])) }
      if ("rmf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,8] / sum(lh[26,3])) }
      if ("rmf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,9] / sum(lh[26,3])) }
      if ("rmf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[26,3] * lh[26,10] / sum(lh[26,3])) }
      if ("sf_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,2]) + sum(rh[27,2]) }
      if ("sf_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3]) + sum(rh[27,3]) }
      if ("sf_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,4]) + sum(rh[27,4]) }
      if ("sf_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,5] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,5] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,6] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,6] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,7] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,7] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,8] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,8] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,9] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,9] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,10] / sum(lh[27,3] + rh[27,3])) + sum(rh[27,3] * rh[27,10] / sum(lh[27,3] + rh[27,3])) }
      if ("sf_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,2]) }
      if ("sf_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3]) }
      if ("sf_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,4]) }
      if ("sf_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,5] / sum(rh[27,3])) }
      if ("sf_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,6] / sum(rh[27,3])) }
      if ("sf_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,7] / sum(rh[27,3])) }
      if ("sf_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,8] / sum(rh[27,3])) }
      if ("sf_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,9] / sum(rh[27,3])) }
      if ("sf_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[27,3] * rh[27,10] / sum(rh[27,3])) }
      if ("sf_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,2]) }
      if ("sf_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3]) }
      if ("sf_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,4]) }
      if ("sf_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,5] / sum(lh[27,3])) }
      if ("sf_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,6] / sum(lh[27,3])) }
      if ("sf_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,7] / sum(lh[27,3])) }
      if ("sf_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,8] / sum(lh[27,3])) }
      if ("sf_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,9] / sum(lh[27,3])) }
      if ("sf_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[27,3] * lh[27,10] / sum(lh[27,3])) }
      if ("parac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,2]) + sum(rh[16,2]) }
      if ("parac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3]) + sum(rh[16,3]) }
      if ("parac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,4]) + sum(rh[16,4]) }
      if ("parac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,5] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,5] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,6] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,6] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,7] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,7] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,8] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,8] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,9] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,9] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,10] / sum(lh[16,3] + rh[16,3])) + sum(rh[16,3] * rh[16,10] / sum(lh[16,3] + rh[16,3])) }
      if ("parac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,2]) }
      if ("parac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3]) }
      if ("parac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,4]) }
      if ("parac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,5] / sum(rh[16,3])) }
      if ("parac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,6] / sum(rh[16,3])) }
      if ("parac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,7] / sum(rh[16,3])) }
      if ("parac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,8] / sum(rh[16,3])) }
      if ("parac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,9] / sum(rh[16,3])) }
      if ("parac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[16,3] * rh[16,10] / sum(rh[16,3])) }
      if ("parac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,2]) }
      if ("parac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3]) }
      if ("parac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,4]) }
      if ("parac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,5] / sum(lh[16,3])) }
      if ("parac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,6] / sum(lh[16,3])) }
      if ("parac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,7] / sum(lh[16,3])) }
      if ("parac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,8] / sum(lh[16,3])) }
      if ("parac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,9] / sum(lh[16,3])) }
      if ("parac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[16,3] * lh[16,10] / sum(lh[16,3])) }
      if ("prec_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,2]) + sum(rh[23,2]) }
      if ("prec_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3]) + sum(rh[23,3]) }
      if ("prec_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,4]) + sum(rh[23,4]) }
      if ("prec_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,5] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,5] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,6] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,6] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,7] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,7] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,8] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,8] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,9] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,9] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,10] / sum(lh[23,3] + rh[23,3])) + sum(rh[23,3] * rh[23,10] / sum(lh[23,3] + rh[23,3])) }
      if ("prec_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,2]) }
      if ("prec_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3]) }
      if ("prec_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,4]) }
      if ("prec_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,5] / sum(rh[23,3])) }
      if ("prec_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,6] / sum(rh[23,3])) }
      if ("prec_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,7] / sum(rh[23,3])) }
      if ("prec_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,8] / sum(rh[23,3])) }
      if ("prec_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,9] / sum(rh[23,3])) }
      if ("prec_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[23,3] * rh[23,10] / sum(rh[23,3])) }
      if ("prec_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,2]) }
      if ("prec_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3]) }
      if ("prec_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,4]) }
      if ("prec_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,5] / sum(lh[23,3])) }
      if ("prec_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,6] / sum(lh[23,3])) }
      if ("prec_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,7] / sum(lh[23,3])) }
      if ("prec_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,8] / sum(lh[23,3])) }
      if ("prec_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,9] / sum(lh[23,3])) }
      if ("prec_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[23,3] * lh[23,10] / sum(lh[23,3])) }
      if ("pp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,2]) + sum(rh[17,2]) }
      if ("pp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3]) + sum(rh[17,3]) }
      if ("pp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,4]) + sum(rh[17,4]) }
      if ("pp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,5] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,5] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,6] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,6] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,7] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,7] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,8] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,8] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,9] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,9] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,10] / sum(lh[17,3] + rh[17,3])) + sum(rh[17,3] * rh[17,10] / sum(lh[17,3] + rh[17,3])) }
      if ("pp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,2]) }
      if ("pp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3]) }
      if ("pp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,4]) }
      if ("pp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,5] / sum(rh[17,3])) }
      if ("pp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,6] / sum(rh[17,3])) }
      if ("pp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,7] / sum(rh[17,3])) }
      if ("pp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,8] / sum(rh[17,3])) }
      if ("pp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,9] / sum(rh[17,3])) }
      if ("pp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[17,3] * rh[17,10] / sum(rh[17,3])) }
      if ("pp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,2]) }
      if ("pp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3]) }
      if ("pp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,4]) }
      if ("pp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,5] / sum(lh[17,3])) }
      if ("pp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,6] / sum(lh[17,3])) }
      if ("pp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,7] / sum(lh[17,3])) }
      if ("pp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,8] / sum(lh[17,3])) }
      if ("pp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,9] / sum(lh[17,3])) }
      if ("pp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[17,3] * lh[17,10] / sum(lh[17,3])) }
      if ("cac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,2]) + sum(rh[2,2]) }
      if ("cac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3]) + sum(rh[2,3]) }
      if ("cac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,4]) + sum(rh[2,4]) }
      if ("cac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,5] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,5] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,6] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,6] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,7] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,7] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,8] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,8] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,9] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,9] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,10] / sum(lh[2,3] + rh[2,3])) + sum(rh[2,3] * rh[2,10] / sum(lh[2,3] + rh[2,3])) }
      if ("cac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,2]) }
      if ("cac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3]) }
      if ("cac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,4]) }
      if ("cac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,5] / sum(rh[2,3])) }
      if ("cac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,6] / sum(rh[2,3])) }
      if ("cac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,7] / sum(rh[2,3])) }
      if ("cac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,8] / sum(rh[2,3])) }
      if ("cac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,9] / sum(rh[2,3])) }
      if ("cac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[2,3] * rh[2,10] / sum(rh[2,3])) }
      if ("cac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,2]) }
      if ("cac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3]) }
      if ("cac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,4]) }
      if ("cac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,5] / sum(lh[2,3])) }
      if ("cac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,6] / sum(lh[2,3])) }
      if ("cac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,7] / sum(lh[2,3])) }
      if ("cac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,8] / sum(lh[2,3])) }
      if ("cac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,9] / sum(lh[2,3])) }
      if ("cac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[2,3] * lh[2,10] / sum(lh[2,3])) }
      if ("rac_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,2]) + sum(rh[25,2]) }
      if ("rac_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3]) + sum(rh[25,3]) }
      if ("rac_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,4]) + sum(rh[25,4]) }
      if ("rac_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,5] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,5] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,6] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,6] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,7] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,7] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,8] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,8] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,9] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,9] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,10] / sum(lh[25,3] + rh[25,3])) + sum(rh[25,3] * rh[25,10] / sum(lh[25,3] + rh[25,3])) }
      if ("rac_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,2]) }
      if ("rac_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3]) }
      if ("rac_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,4]) }
      if ("rac_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,5] / sum(rh[25,3])) }
      if ("rac_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,6] / sum(rh[25,3])) }
      if ("rac_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,7] / sum(rh[25,3])) }
      if ("rac_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,8] / sum(rh[25,3])) }
      if ("rac_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,9] / sum(rh[25,3])) }
      if ("rac_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[25,3] * rh[25,10] / sum(rh[25,3])) }
      if ("rac_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,2]) }
      if ("rac_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3]) }
      if ("rac_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,4]) }
      if ("rac_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,5] / sum(lh[25,3])) }
      if ("rac_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,6] / sum(lh[25,3])) }
      if ("rac_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,7] / sum(lh[25,3])) }
      if ("rac_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,8] / sum(lh[25,3])) }
      if ("rac_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,9] / sum(lh[25,3])) }
      if ("rac_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[25,3] * lh[25,10] / sum(lh[25,3])) }
      if ("ins_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,2]) + sum(rh[34,2]) }
      if ("ins_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3]) + sum(rh[34,3]) }
      if ("ins_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,4]) + sum(rh[34,4]) }
      if ("ins_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,5] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,5] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,6] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,6] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,7] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,7] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,8] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,8] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,9] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,9] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,10] / sum(lh[34,3] + rh[34,3])) + sum(rh[34,3] * rh[34,10] / sum(lh[34,3] + rh[34,3])) }
      if ("ins_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,2]) }
      if ("ins_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3]) }
      if ("ins_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,4]) }
      if ("ins_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,5] / sum(rh[34,3])) }
      if ("ins_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,6] / sum(rh[34,3])) }
      if ("ins_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,7] / sum(rh[34,3])) }
      if ("ins_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,8] / sum(rh[34,3])) }
      if ("ins_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,9] / sum(rh[34,3])) }
      if ("ins_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[34,3] * rh[34,10] / sum(rh[34,3])) }
      if ("ins_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,2]) }
      if ("ins_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3]) }
      if ("ins_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,4]) }
      if ("ins_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,5] / sum(lh[34,3])) }
      if ("ins_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,6] / sum(lh[34,3])) }
      if ("ins_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,7] / sum(lh[34,3])) }
      if ("ins_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,8] / sum(lh[34,3])) }
      if ("ins_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,9] / sum(lh[34,3])) }
      if ("ins_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[34,3] * lh[34,10] / sum(lh[34,3])) }
      if ("postc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,2]) + sum(rh[21,2]) }
      if ("postc_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3]) + sum(rh[21,3]) }
      if ("postc_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,4]) + sum(rh[21,4]) }
      if ("postc_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,5] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,5] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,6] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,6] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,7] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,7] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,8] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,8] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,9] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,9] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,10] / sum(lh[21,3] + rh[21,3])) + sum(rh[21,3] * rh[21,10] / sum(lh[21,3] + rh[21,3])) }
      if ("postc_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,2]) }
      if ("postc_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3]) }
      if ("postc_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,4]) }
      if ("postc_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,5] / sum(rh[21,3])) }
      if ("postc_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,6] / sum(rh[21,3])) }
      if ("postc_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,7] / sum(rh[21,3])) }
      if ("postc_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,8] / sum(rh[21,3])) }
      if ("postc_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,9] / sum(rh[21,3])) }
      if ("postc_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[21,3] * rh[21,10] / sum(rh[21,3])) }
      if ("postc_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,2]) }
      if ("postc_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3]) }
      if ("postc_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,4]) }
      if ("postc_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,5] / sum(lh[21,3])) }
      if ("postc_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,6] / sum(lh[21,3])) }
      if ("postc_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,7] / sum(lh[21,3])) }
      if ("postc_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,8] / sum(lh[21,3])) }
      if ("postc_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,9] / sum(lh[21,3])) }
      if ("postc_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[21,3] * lh[21,10] / sum(lh[21,3])) }
      if ("ip_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,2]) + sum(rh[7,2]) }
      if ("ip_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3]) + sum(rh[7,3]) }
      if ("ip_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,4]) + sum(rh[7,4]) }
      if ("ip_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,5] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,5] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,6] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,6] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,7] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,7] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,8] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,8] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,9] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,9] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,10] / sum(lh[7,3] + rh[7,3])) + sum(rh[7,3] * rh[7,10] / sum(lh[7,3] + rh[7,3])) }
      if ("ip_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,2]) }
      if ("ip_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3]) }
      if ("ip_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,4]) }
      if ("ip_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,5] / sum(rh[7,3])) }
      if ("ip_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,6] / sum(rh[7,3])) }
      if ("ip_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,7] / sum(rh[7,3])) }
      if ("ip_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,8] / sum(rh[7,3])) }
      if ("ip_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,9] / sum(rh[7,3])) }
      if ("ip_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[7,3] * rh[7,10] / sum(rh[7,3])) }
      if ("ip_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,2]) }
      if ("ip_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3]) }
      if ("ip_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,4]) }
      if ("ip_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,5] / sum(lh[7,3])) }
      if ("ip_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,6] / sum(lh[7,3])) }
      if ("ip_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,7] / sum(lh[7,3])) }
      if ("ip_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,8] / sum(lh[7,3])) }
      if ("ip_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,9] / sum(lh[7,3])) }
      if ("ip_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[7,3] * lh[7,10] / sum(lh[7,3])) }
      if ("sp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,2]) + sum(rh[28,2]) }
      if ("sp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3]) + sum(rh[28,3]) }
      if ("sp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,4]) + sum(rh[28,4]) }
      if ("sp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,5] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,5] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,6] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,6] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,7] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,7] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,8] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,8] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,9] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,9] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,10] / sum(lh[28,3] + rh[28,3])) + sum(rh[28,3] * rh[28,10] / sum(lh[28,3] + rh[28,3])) }
      if ("sp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,2]) }
      if ("sp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3]) }
      if ("sp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,4]) }
      if ("sp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,5] / sum(rh[28,3])) }
      if ("sp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,6] / sum(rh[28,3])) }
      if ("sp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,7] / sum(rh[28,3])) }
      if ("sp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,8] / sum(rh[28,3])) }
      if ("sp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,9] / sum(rh[28,3])) }
      if ("sp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[28,3] * rh[28,10] / sum(rh[28,3])) }
      if ("sp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,2]) }
      if ("sp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3]) }
      if ("sp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,4]) }
      if ("sp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,5] / sum(lh[28,3])) }
      if ("sp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,6] / sum(lh[28,3])) }
      if ("sp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,7] / sum(lh[28,3])) }
      if ("sp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,8] / sum(lh[28,3])) }
      if ("sp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,9] / sum(lh[28,3])) }
      if ("sp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[28,3] * lh[28,10] / sum(lh[28,3])) }
      if ("pcun_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,2]) + sum(rh[24,2]) }
      if ("pcun_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3]) + sum(rh[24,3]) }
      if ("pcun_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,4]) + sum(rh[24,4]) }
      if ("pcun_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,5] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,5] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,6] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,6] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,7] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,7] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,8] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,8] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,9] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,9] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,10] / sum(lh[24,3] + rh[24,3])) + sum(rh[24,3] * rh[24,10] / sum(lh[24,3] + rh[24,3])) }
      if ("pcun_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,2]) }
      if ("pcun_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3]) }
      if ("pcun_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,4]) }
      if ("pcun_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,5] / sum(rh[24,3])) }
      if ("pcun_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,6] / sum(rh[24,3])) }
      if ("pcun_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,7] / sum(rh[24,3])) }
      if ("pcun_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,8] / sum(rh[24,3])) }
      if ("pcun_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,9] / sum(rh[24,3])) }
      if ("pcun_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[24,3] * rh[24,10] / sum(rh[24,3])) }
      if ("pcun_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,2]) }
      if ("pcun_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3]) }
      if ("pcun_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,4]) }
      if ("pcun_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,5] / sum(lh[24,3])) }
      if ("pcun_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,6] / sum(lh[24,3])) }
      if ("pcun_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,7] / sum(lh[24,3])) }
      if ("pcun_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,8] / sum(lh[24,3])) }
      if ("pcun_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,9] / sum(lh[24,3])) }
      if ("pcun_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[24,3] * lh[24,10] / sum(lh[24,3])) }
      if ("sm_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,2]) + sum(rh[30,2]) }
      if ("sm_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3]) + sum(rh[30,3]) }
      if ("sm_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,4]) + sum(rh[30,4]) }
      if ("sm_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,5] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,5] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,6] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,6] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,7] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,7] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,8] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,8] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,9] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,9] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,10] / sum(lh[30,3] + rh[30,3])) + sum(rh[30,3] * rh[30,10] / sum(lh[30,3] + rh[30,3])) }
      if ("sm_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,2]) }
      if ("sm_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3]) }
      if ("sm_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,4]) }
      if ("sm_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,5] / sum(rh[30,3])) }
      if ("sm_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,6] / sum(rh[30,3])) }
      if ("sm_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,7] / sum(rh[30,3])) }
      if ("sm_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,8] / sum(rh[30,3])) }
      if ("sm_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,9] / sum(rh[30,3])) }
      if ("sm_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[30,3] * rh[30,10] / sum(rh[30,3])) }
      if ("sm_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,2]) }
      if ("sm_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3]) }
      if ("sm_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,4]) }
      if ("sm_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,5] / sum(lh[30,3])) }
      if ("sm_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,6] / sum(lh[30,3])) }
      if ("sm_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,7] / sum(lh[30,3])) }
      if ("sm_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,8] / sum(lh[30,3])) }
      if ("sm_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,9] / sum(lh[30,3])) }
      if ("sm_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[30,3] * lh[30,10] / sum(lh[30,3])) }
      if ("pc_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,2]) + sum(rh[22,2]) }
      if ("pc_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3]) + sum(rh[22,3]) }
      if ("pc_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,4]) + sum(rh[22,4]) }
      if ("pc_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,5] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,5] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,6] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,6] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,7] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,7] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,8] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,8] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,9] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,9] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,10] / sum(lh[22,3] + rh[22,3])) + sum(rh[22,3] * rh[22,10] / sum(lh[22,3] + rh[22,3])) }
      if ("pc_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,2]) }
      if ("pc_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3]) }
      if ("pc_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,4]) }
      if ("pc_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,5] / sum(rh[22,3])) }
      if ("pc_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,6] / sum(rh[22,3])) }
      if ("pc_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,7] / sum(rh[22,3])) }
      if ("pc_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,8] / sum(rh[22,3])) }
      if ("pc_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,9] / sum(rh[22,3])) }
      if ("pc_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[22,3] * rh[22,10] / sum(rh[22,3])) }
      if ("pc_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,2]) }
      if ("pc_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3]) }
      if ("pc_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,4]) }
      if ("pc_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,5] / sum(lh[22,3])) }
      if ("pc_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,6] / sum(lh[22,3])) }
      if ("pc_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,7] / sum(lh[22,3])) }
      if ("pc_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,8] / sum(lh[22,3])) }
      if ("pc_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,9] / sum(lh[22,3])) }
      if ("pc_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[22,3] * lh[22,10] / sum(lh[22,3])) }
      if ("ic_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,2]) + sum(rh[9,2]) }
      if ("ic_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3]) + sum(rh[9,3]) }
      if ("ic_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,4]) + sum(rh[9,4]) }
      if ("ic_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,5] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,5] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,6] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,6] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,7] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,7] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,8] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,8] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,9] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,9] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,10] / sum(lh[9,3] + rh[9,3])) + sum(rh[9,3] * rh[9,10] / sum(lh[9,3] + rh[9,3])) }
      if ("ic_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,2]) }
      if ("ic_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3]) }
      if ("ic_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,4]) }
      if ("ic_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,5] / sum(rh[9,3])) }
      if ("ic_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,6] / sum(rh[9,3])) }
      if ("ic_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,7] / sum(rh[9,3])) }
      if ("ic_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,8] / sum(rh[9,3])) }
      if ("ic_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,9] / sum(rh[9,3])) }
      if ("ic_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[9,3] * rh[9,10] / sum(rh[9,3])) }
      if ("ic_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,2]) }
      if ("ic_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3]) }
      if ("ic_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,4]) }
      if ("ic_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,5] / sum(lh[9,3])) }
      if ("ic_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,6] / sum(lh[9,3])) }
      if ("ic_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,7] / sum(lh[9,3])) }
      if ("ic_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,8] / sum(lh[9,3])) }
      if ("ic_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,9] / sum(lh[9,3])) }
      if ("ic_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[9,3] * lh[9,10] / sum(lh[9,3])) }
      if ("it_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,2]) + sum(rh[8,2]) }
      if ("it_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3]) + sum(rh[8,3]) }
      if ("it_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,4]) + sum(rh[8,4]) }
      if ("it_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,5] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,5] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,6] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,6] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,7] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,7] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,8] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,8] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,9] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,9] / sum(lh[8,3] + rh[8,3])) }
      if ("it_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,10] / sum(lh[8,3] + rh[8,3])) + sum(rh[8,3] * rh[8,10] / sum(lh[8,3] + rh[8,3])) }
      if ("it_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,2]) }
      if ("it_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3]) }
      if ("it_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,4]) }
      if ("it_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,5] / sum(rh[8,3])) }
      if ("it_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,6] / sum(rh[8,3])) }
      if ("it_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,7] / sum(rh[8,3])) }
      if ("it_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,8] / sum(rh[8,3])) }
      if ("it_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,9] / sum(rh[8,3])) }
      if ("it_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[8,3] * rh[8,10] / sum(rh[8,3])) }
      if ("it_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,2]) }
      if ("it_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3]) }
      if ("it_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,4]) }
      if ("it_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,5] / sum(lh[8,3])) }
      if ("it_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,6] / sum(lh[8,3])) }
      if ("it_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,7] / sum(lh[8,3])) }
      if ("it_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,8] / sum(lh[8,3])) }
      if ("it_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,9] / sum(lh[8,3])) }
      if ("it_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[8,3] * lh[8,10] / sum(lh[8,3])) }
      if ("mt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,2]) + sum(rh[14,2]) }
      if ("mt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3]) + sum(rh[14,3]) }
      if ("mt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,4]) + sum(rh[14,4]) }
      if ("mt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,5] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,5] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,6] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,6] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,7] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,7] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,8] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,8] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,9] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,9] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,10] / sum(lh[14,3] + rh[14,3])) + sum(rh[14,3] * rh[14,10] / sum(lh[14,3] + rh[14,3])) }
      if ("mt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,2]) }
      if ("mt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3]) }
      if ("mt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,4]) }
      if ("mt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,5] / sum(rh[14,3])) }
      if ("mt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,6] / sum(rh[14,3])) }
      if ("mt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,7] / sum(rh[14,3])) }
      if ("mt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,8] / sum(rh[14,3])) }
      if ("mt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,9] / sum(rh[14,3])) }
      if ("mt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[14,3] * rh[14,10] / sum(rh[14,3])) }
      if ("mt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,2]) }
      if ("mt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3]) }
      if ("mt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,4]) }
      if ("mt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,5] / sum(lh[14,3])) }
      if ("mt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,6] / sum(lh[14,3])) }
      if ("mt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,7] / sum(lh[14,3])) }
      if ("mt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,8] / sum(lh[14,3])) }
      if ("mt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,9] / sum(lh[14,3])) }
      if ("mt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[14,3] * lh[14,10] / sum(lh[14,3])) }
      if ("st_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,2]) + sum(rh[29,2]) }
      if ("st_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3]) + sum(rh[29,3]) }
      if ("st_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,4]) + sum(rh[29,4]) }
      if ("st_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,5] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,5] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,6] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,6] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,7] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,7] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,8] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,8] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,9] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,9] / sum(lh[29,3] + rh[29,3])) }
      if ("st_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,10] / sum(lh[29,3] + rh[29,3])) + sum(rh[29,3] * rh[29,10] / sum(lh[29,3] + rh[29,3])) }
      if ("st_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,2]) }
      if ("st_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3]) }
      if ("st_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,4]) }
      if ("st_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,5] / sum(rh[29,3])) }
      if ("st_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,6] / sum(rh[29,3])) }
      if ("st_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,7] / sum(rh[29,3])) }
      if ("st_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,8] / sum(rh[29,3])) }
      if ("st_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,9] / sum(rh[29,3])) }
      if ("st_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[29,3] * rh[29,10] / sum(rh[29,3])) }
      if ("st_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,2]) }
      if ("st_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3]) }
      if ("st_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,4]) }
      if ("st_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,5] / sum(lh[29,3])) }
      if ("st_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,6] / sum(lh[29,3])) }
      if ("st_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,7] / sum(lh[29,3])) }
      if ("st_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,8] / sum(lh[29,3])) }
      if ("st_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,9] / sum(lh[29,3])) }
      if ("st_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[29,3] * lh[29,10] / sum(lh[29,3])) }
      if ("tt_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,2]) + sum(rh[33,2]) }
      if ("tt_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3]) + sum(rh[33,3]) }
      if ("tt_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,4]) + sum(rh[33,4]) }
      if ("tt_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,5] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,5] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,6] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,6] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,7] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,7] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,8] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,8] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,9] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,9] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,10] / sum(lh[33,3] + rh[33,3])) + sum(rh[33,3] * rh[33,10] / sum(lh[33,3] + rh[33,3])) }
      if ("tt_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,2]) }
      if ("tt_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3]) }
      if ("tt_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,4]) }
      if ("tt_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,5] / sum(rh[33,3])) }
      if ("tt_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,6] / sum(rh[33,3])) }
      if ("tt_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,7] / sum(rh[33,3])) }
      if ("tt_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,8] / sum(rh[33,3])) }
      if ("tt_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,9] / sum(rh[33,3])) }
      if ("tt_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[33,3] * rh[33,10] / sum(rh[33,3])) }
      if ("tt_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,2]) }
      if ("tt_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3]) }
      if ("tt_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,4]) }
      if ("tt_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,5] / sum(lh[33,3])) }
      if ("tt_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,6] / sum(lh[33,3])) }
      if ("tt_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,7] / sum(lh[33,3])) }
      if ("tt_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,8] / sum(lh[33,3])) }
      if ("tt_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,9] / sum(lh[33,3])) }
      if ("tt_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[33,3] * lh[33,10] / sum(lh[33,3])) }
      if ("ph_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,2]) + sum(rh[15,2]) }
      if ("ph_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3]) + sum(rh[15,3]) }
      if ("ph_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,4]) + sum(rh[15,4]) }
      if ("ph_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,5] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,5] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,6] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,6] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,7] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,7] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,8] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,8] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,9] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,9] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,10] / sum(lh[15,3] + rh[15,3])) + sum(rh[15,3] * rh[15,10] / sum(lh[15,3] + rh[15,3])) }
      if ("ph_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,2]) }
      if ("ph_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3]) }
      if ("ph_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,4]) }
      if ("ph_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,5] / sum(rh[15,3])) }
      if ("ph_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,6] / sum(rh[15,3])) }
      if ("ph_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,7] / sum(rh[15,3])) }
      if ("ph_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,8] / sum(rh[15,3])) }
      if ("ph_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,9] / sum(rh[15,3])) }
      if ("ph_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[15,3] * rh[15,10] / sum(rh[15,3])) }
      if ("ph_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,2]) }
      if ("ph_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3]) }
      if ("ph_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,4]) }
      if ("ph_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,5] / sum(lh[15,3])) }
      if ("ph_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,6] / sum(lh[15,3])) }
      if ("ph_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,7] / sum(lh[15,3])) }
      if ("ph_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,8] / sum(lh[15,3])) }
      if ("ph_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,9] / sum(lh[15,3])) }
      if ("ph_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[15,3] * lh[15,10] / sum(lh[15,3])) }
      if ("fus_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,2]) + sum(rh[6,2]) }
      if ("fus_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3]) + sum(rh[6,3]) }
      if ("fus_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,4]) + sum(rh[6,4]) }
      if ("fus_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,5] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,5] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,6] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,6] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,7] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,7] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,8] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,8] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,9] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,9] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,10] / sum(lh[6,3] + rh[6,3])) + sum(rh[6,3] * rh[6,10] / sum(lh[6,3] + rh[6,3])) }
      if ("fus_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,2]) }
      if ("fus_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3]) }
      if ("fus_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,4]) }
      if ("fus_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,5] / sum(rh[6,3])) }
      if ("fus_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,6] / sum(rh[6,3])) }
      if ("fus_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,7] / sum(rh[6,3])) }
      if ("fus_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,8] / sum(rh[6,3])) }
      if ("fus_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,9] / sum(rh[6,3])) }
      if ("fus_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[6,3] * rh[6,10] / sum(rh[6,3])) }
      if ("fus_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,2]) }
      if ("fus_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3]) }
      if ("fus_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,4]) }
      if ("fus_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,5] / sum(lh[6,3])) }
      if ("fus_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,6] / sum(lh[6,3])) }
      if ("fus_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,7] / sum(lh[6,3])) }
      if ("fus_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,8] / sum(lh[6,3])) }
      if ("fus_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,9] / sum(lh[6,3])) }
      if ("fus_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[6,3] * lh[6,10] / sum(lh[6,3])) }
      if ("bsts_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,2]) + sum(rh[1,2]) }
      if ("bsts_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3]) + sum(rh[1,3]) }
      if ("bsts_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,4]) + sum(rh[1,4]) }
      if ("bsts_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,5] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,5] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,6] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,6] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,7] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,7] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,8] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,8] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,9] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,9] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,10] / sum(lh[1,3] + rh[1,3])) + sum(rh[1,3] * rh[1,10] / sum(lh[1,3] + rh[1,3])) }
      if ("bsts_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,2]) }
      if ("bsts_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3]) }
      if ("bsts_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,4]) }
      if ("bsts_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,5] / sum(rh[1,3])) }
      if ("bsts_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,6] / sum(rh[1,3])) }
      if ("bsts_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,7] / sum(rh[1,3])) }
      if ("bsts_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,8] / sum(rh[1,3])) }
      if ("bsts_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,9] / sum(rh[1,3])) }
      if ("bsts_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[1,3] * rh[1,10] / sum(rh[1,3])) }
      if ("bsts_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,2]) }
      if ("bsts_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3]) }
      if ("bsts_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,4]) }
      if ("bsts_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,5] / sum(lh[1,3])) }
      if ("bsts_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,6] / sum(lh[1,3])) }
      if ("bsts_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,7] / sum(lh[1,3])) }
      if ("bsts_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,8] / sum(lh[1,3])) }
      if ("bsts_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,9] / sum(lh[1,3])) }
      if ("bsts_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[1,3] * lh[1,10] / sum(lh[1,3])) }
      if ("er_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,2]) + sum(rh[5,2]) }
      if ("er_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3]) + sum(rh[5,3]) }
      if ("er_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,4]) + sum(rh[5,4]) }
      if ("er_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,5] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,5] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,6] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,6] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,7] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,7] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,8] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,8] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,9] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,9] / sum(lh[5,3] + rh[5,3])) }
      if ("er_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,10] / sum(lh[5,3] + rh[5,3])) + sum(rh[5,3] * rh[5,10] / sum(lh[5,3] + rh[5,3])) }
      if ("er_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,2]) }
      if ("er_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3]) }
      if ("er_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,4]) }
      if ("er_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,5] / sum(rh[5,3])) }
      if ("er_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,6] / sum(rh[5,3])) }
      if ("er_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,7] / sum(rh[5,3])) }
      if ("er_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,8] / sum(rh[5,3])) }
      if ("er_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,9] / sum(rh[5,3])) }
      if ("er_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[5,3] * rh[5,10] / sum(rh[5,3])) }
      if ("er_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,2]) }
      if ("er_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3]) }
      if ("er_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,4]) }
      if ("er_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,5] / sum(lh[5,3])) }
      if ("er_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,6] / sum(lh[5,3])) }
      if ("er_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,7] / sum(lh[5,3])) }
      if ("er_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,8] / sum(lh[5,3])) }
      if ("er_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,9] / sum(lh[5,3])) }
      if ("er_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[5,3] * lh[5,10] / sum(lh[5,3])) }
      if ("tp_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,2]) + sum(rh[32,2]) }
      if ("tp_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3]) + sum(rh[32,3]) }
      if ("tp_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,4]) + sum(rh[32,4]) }
      if ("tp_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,5] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,5] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,6] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,6] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,7] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,7] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,8] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,8] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,9] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,9] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,10] / sum(lh[32,3] + rh[32,3])) + sum(rh[32,3] * rh[32,10] / sum(lh[32,3] + rh[32,3])) }
      if ("tp_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,2]) }
      if ("tp_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3]) }
      if ("tp_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,4]) }
      if ("tp_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,5] / sum(rh[32,3])) }
      if ("tp_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,6] / sum(rh[32,3])) }
      if ("tp_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,7] / sum(rh[32,3])) }
      if ("tp_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,8] / sum(rh[32,3])) }
      if ("tp_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,9] / sum(rh[32,3])) }
      if ("tp_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[32,3] * rh[32,10] / sum(rh[32,3])) }
      if ("tp_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,2]) }
      if ("tp_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3]) }
      if ("tp_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,4]) }
      if ("tp_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,5] / sum(lh[32,3])) }
      if ("tp_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,6] / sum(lh[32,3])) }
      if ("tp_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,7] / sum(lh[32,3])) }
      if ("tp_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,8] / sum(lh[32,3])) }
      if ("tp_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,9] / sum(lh[32,3])) }
      if ("tp_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[32,3] * lh[32,10] / sum(lh[32,3])) }
      if ("cun_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,2]) + sum(rh[4,2]) }
      if ("cun_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3]) + sum(rh[4,3]) }
      if ("cun_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,4]) + sum(rh[4,4]) }
      if ("cun_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,5] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,5] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,6] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,6] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,7] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,7] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,8] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,8] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,9] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,9] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,10] / sum(lh[4,3] + rh[4,3])) + sum(rh[4,3] * rh[4,10] / sum(lh[4,3] + rh[4,3])) }
      if ("cun_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,2]) }
      if ("cun_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3]) }
      if ("cun_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,4]) }
      if ("cun_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,5] / sum(rh[4,3])) }
      if ("cun_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,6] / sum(rh[4,3])) }
      if ("cun_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,7] / sum(rh[4,3])) }
      if ("cun_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,8] / sum(rh[4,3])) }
      if ("cun_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,9] / sum(rh[4,3])) }
      if ("cun_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[4,3] * rh[4,10] / sum(rh[4,3])) }
      if ("cun_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,2]) }
      if ("cun_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3]) }
      if ("cun_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,4]) }
      if ("cun_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,5] / sum(lh[4,3])) }
      if ("cun_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,6] / sum(lh[4,3])) }
      if ("cun_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,7] / sum(lh[4,3])) }
      if ("cun_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,8] / sum(lh[4,3])) }
      if ("cun_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,9] / sum(lh[4,3])) }
      if ("cun_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[4,3] * lh[4,10] / sum(lh[4,3])) }
      if ("lo_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,2]) + sum(rh[10,2]) }
      if ("lo_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3]) + sum(rh[10,3]) }
      if ("lo_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,4]) + sum(rh[10,4]) }
      if ("lo_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,5] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,5] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,6] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,6] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,7] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,7] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,8] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,8] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,9] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,9] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,10] / sum(lh[10,3] + rh[10,3])) + sum(rh[10,3] * rh[10,10] / sum(lh[10,3] + rh[10,3])) }
      if ("lo_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,2]) }
      if ("lo_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3]) }
      if ("lo_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,4]) }
      if ("lo_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,5] / sum(rh[10,3])) }
      if ("lo_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,6] / sum(rh[10,3])) }
      if ("lo_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,7] / sum(rh[10,3])) }
      if ("lo_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,8] / sum(rh[10,3])) }
      if ("lo_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,9] / sum(rh[10,3])) }
      if ("lo_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[10,3] * rh[10,10] / sum(rh[10,3])) }
      if ("lo_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,2]) }
      if ("lo_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3]) }
      if ("lo_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,4]) }
      if ("lo_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,5] / sum(lh[10,3])) }
      if ("lo_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,6] / sum(lh[10,3])) }
      if ("lo_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,7] / sum(lh[10,3])) }
      if ("lo_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,8] / sum(lh[10,3])) }
      if ("lo_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,9] / sum(lh[10,3])) }
      if ("lo_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[10,3] * lh[10,10] / sum(lh[10,3])) }
      if ("ling_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,2]) + sum(rh[12,2]) }
      if ("ling_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3]) + sum(rh[12,3]) }
      if ("ling_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,4]) + sum(rh[12,4]) }
      if ("ling_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,5] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,5] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,6] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,6] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,7] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,7] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,8] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,8] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,9] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,9] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,10] / sum(lh[12,3] + rh[12,3])) + sum(rh[12,3] * rh[12,10] / sum(lh[12,3] + rh[12,3])) }
      if ("ling_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,2]) }
      if ("ling_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3]) }
      if ("ling_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,4]) }
      if ("ling_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,5] / sum(rh[12,3])) }
      if ("ling_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,6] / sum(rh[12,3])) }
      if ("ling_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,7] / sum(rh[12,3])) }
      if ("ling_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,8] / sum(rh[12,3])) }
      if ("ling_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,9] / sum(rh[12,3])) }
      if ("ling_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[12,3] * rh[12,10] / sum(rh[12,3])) }
      if ("ling_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,2]) }
      if ("ling_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3]) }
      if ("ling_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,4]) }
      if ("ling_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,5] / sum(lh[12,3])) }
      if ("ling_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,6] / sum(lh[12,3])) }
      if ("ling_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,7] / sum(lh[12,3])) }
      if ("ling_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,8] / sum(lh[12,3])) }
      if ("ling_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,9] / sum(lh[12,3])) }
      if ("ling_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[12,3] * lh[12,10] / sum(lh[12,3])) }
      if ("peric_t_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,2]) + sum(rh[20,2]) }
      if ("peric_t_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3]) + sum(rh[20,3]) }
      if ("peric_t_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,4]) + sum(rh[20,4]) }
      if ("peric_t_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,5] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,5] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,6] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,6] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,7] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,7] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,8] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,8] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,9] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,9] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_t_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,10] / sum(lh[20,3] + rh[20,3])) + sum(rh[20,3] * rh[20,10] / sum(lh[20,3] + rh[20,3])) }
      if ("peric_r_n" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,2]) }
      if ("peric_r_sa" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3]) }
      if ("peric_r_g" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,4]) }
      if ("peric_r_ta" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,5] / sum(rh[20,3])) }
      if ("peric_r_tsd" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,6] / sum(rh[20,3])) }
      if ("peric_r_mc" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,7] / sum(rh[20,3])) }
      if ("peric_r_gc" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,8] / sum(rh[20,3])) }
      if ("peric_r_fi" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,9] / sum(rh[20,3])) }
      if ("peric_r_ci" %in% var.names ) { out[length(out) + 1] <- sum(rh[20,3] * rh[20,10] / sum(rh[20,3])) }
      if ("peric_l_n" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,2]) }
      if ("peric_l_sa" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3]) }
      if ("peric_l_g" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,4]) }
      if ("peric_l_ta" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,5] / sum(lh[20,3])) }
      if ("peric_l_tsd" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,6] / sum(lh[20,3])) }
      if ("peric_l_mc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,7] / sum(lh[20,3])) }
      if ("peric_l_gc" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,8] / sum(lh[20,3])) }
      if ("peric_l_fi" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,9] / sum(lh[20,3])) }
      if ("peric_l_ci" %in% var.names ) { out[length(out) + 1] <- sum(lh[20,3] * lh[20,10] / sum(lh[20,3])) }
      
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
    write.table(df, file=paste0(save.dir, "/fsurf.summary.", format(Sys.time(), "%Y%m%d"), ".csv"),
                quote=FALSE, row.names=FALSE, col.names=TRUE, sep=",")
  }
  
  
  if (return.df) {
    return(df)
  }
  
}



