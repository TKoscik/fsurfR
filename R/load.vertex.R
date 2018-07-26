load.vertex <- function(data.dir,
                        which.sjx = "all",
                        hemi = c("lh", "rh"),
                        var.name = c("curv", "sulc", "thickness",
                                     "volume", "w-g.pct", "jacobian_white"),
                        which.vertex) {
  
  if (length(hemi) != 1) {
    warning("Only one hemisphere can be loaded at a time, using first supplied value")
  }
  stopifnot(is.numeric(which.vertex), length(which.vertex)==1)
  
  n.var <- length(var.name)
  fls <- vector("list", n.var)
  for (i in 1:n.var) {
    fls[[i]] <- paste(data.dir, hemi[1], which.sjx, var.name[i], sep="/")
  }
  n.sjx <- nrow(fls[[i]])
  
  df <- data.frame(matrix(0, nrow=n.sjx, ncol=n.var))
  colnames(df) <- paste(hemi, var.name, sep = ".")
  for (i in 1:n.sjx) {
    for (j in 1:n.var) {
      if (file.exists(fls[[j]][i])) {
        df[i,j] <- read.curv.vertex(fls[[j]][i], n=which.vtx)
      } else {
        df[i,j] <- NA
      }
    }
  }
  return(df)
}