icv.fsurf <- function(data.dir) {
 
  icv <- summary.fsurf(data.dir = data.dir,
                       vars = c("g", "w"),
                       sub.vars = "v",
                       rois = c("wb", "sub", "wm", "lv", "ilv", "v3", "v4", "v5", "csf", "ves", "cp"),
                       sjx = "all",
                       hemi = "t",
                       save.csv=FALSE,
                       save.dir = NULL,
                       file.name = NULL,
                       return.df = TRUE)
  
  df <- data.frame(icv$id,
                   icv = rowSums(icv[ ,-1]))
}