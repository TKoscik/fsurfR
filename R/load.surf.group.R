load.surf.group <- function(data.dir,
                            which.sjx = "all",
                            hemi = c("lh", "rh"),
                            var.name = c("curv", "sulc", "thickness",
                                         "volume", "w-g.pct", "jacobian_white"),
                            which.vertex, tolerance = 1) {
  
  # Debug ----
  # data.dir <- "/Shared/nopoulos/freesurfer/DM1_v6_N4_2018/FreeSurfer_Subjects"
  # which.sjx <- "all"
  # hemi <- "lh"
  # var.name <- c("area", "thickness")
  # which.vertex <- 10
  # tolerance <- 1
  #----
  
  fls <- parse.surf(data.dir, which.sjx, hemi, c("sphere.reg", var.name))
  group.surf <- read.surf(paste0(data.dir, "/fsaverage/surf/", hemi[1], ".sphere.reg"))
  
  n.sjx <- nrow(fls)
  n.var <- length(var.name)
  df <- data.frame(matrix(0, nrow=n.sjx, ncol=n.var))
  colnames(df) <- paste(hemi, var.name, sep = ".")
  for (i in 1:n.sjx) {
    if (file.exists(fls[i,1])) {
      sjx.surf <- read.surf(fls[i,1])
      dist.vtx <- sqrt((group.surf$vertex[which.vertex,1] - sjx.surf$vertex[ ,1])^2 + 
                         (group.surf$vertex[which.vertex,2] - sjx.surf$vertex[ ,2])^2 + 
                         (group.surf$vertex[which.vertex,3] - sjx.surf$vertex[ ,3])^2)
      which.vtx <- which(dist.vtx < tolerance)
      dist.vtx <- tolerance - dist.vtx[which.vtx]
      
      if (length(dist.vtx) != 0) {
        for (j in 1:n.var) {
          curv.val <- read.curv.vertex(fls[i,j+1], n=which.vtx)
          df[i,j] <- sum(curv.val * dist.vtx)/sum(dist.vtx)
        }
      }
    } else {
      df[i, ] <- NA
    }
  }
  return(df)
}