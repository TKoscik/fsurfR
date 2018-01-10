parse.surf <- function(root.dir,
                       which.sjx = "all",
                       hemi = c("lh", "rh"),
                       var.name = c("sphere.reg", "curv", "sulc", "thickness",
                                    "volume", "w-g.pct", "jacobian_white")) {
  
  sjx.ls <- list.dirs(path = root.dir, full.names = FALSE, recursive = FALSE)
  if (sjx.ls[length(sjx.ls)] == "fsaverage") {
    sjx.ls <- sjx.ls[-length(sjx.ls)]
  }
  if (length(which.sjx) == 1 & which.sjx[1] == "all") {
  } else {
    sjx.ls <- sjx.ls[which(sjx.ls %in% which.sjx)]
  }
  
  n.sjx <- length(sjx.ls)
  fls <- data.frame(matrix("NA", nrow=n.sjx, ncol=length(hemi)*length(var.name)), stringsAsFactors = FALSE)
  for (i in 1:n.sjx) {
    for (j in 1:length(hemi)) {
      for (k in 1:length(var.name)) {
        fls[i, k+((j-1)*length(var.name))] <- paste0(root.dir, "/", sjx.ls[i], "/surf/", hemi[j], ".", var.name[k])
        colnames(fls)[k+((j-1)*length(var.name))] <- paste0(hemi[j], ".", var.name[k])
      }
    }
  }
  
  return(fls)
}