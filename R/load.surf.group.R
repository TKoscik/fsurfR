load.surf.group <- function(data.dir,
                            which.sjx = "all",
                            hemi = c("lh", "rh"),
                            var.name = c("curv", "sulc", "thickness",
                                         "volume", "w-g.pct", "jacobian_white"),
                            which.vertex, tolerance = 1) {
  
  # Debug ----
  data.dir <- "/Shared/shizhan/PNC/55489/koscikt/fsurf"
  which.sjx <- "all"
  hemi <- "lh"
  var.name <- c("curv", "thickness")
  which.vertex <- 10
  tolerance <- 1
  #----
  
  fls <- parse.surf(data.dir, which.sjx, hemi, c("sphere.reg", var.name))
  group.surf <- read.surf(paste0(data.dir, "/fsaverage/surf/", hemi[1], ".sphere.reg"))
  
  n.sjx <- nrow(fls)
  n.var <- length(var.name)
  df <- data.frame(matrix(0, nrow=n.sjx, ncol=n.var))
  colnames(df) <- paste(hemi, var.name, sep = ".")
  for (i in 1:n.sjx) {
    sjx.surf <- read.surf(fls[i,1])
    
    sjx.vtx <- sjx.surf$vertex[sjx.surf$vertex[ ,1] > (group.surf$vertex[which.vertex, 1] - tolerance), ]
    sjx.vtx <- sjx.vtx[sjx.vtx[ ,1] < (group.surf$vertex[which.vertex, 1] + tolerance), ]
    sjx.vtx <- sjx.vtx[sjx.vtx[ ,2] > (group.surf$vertex[which.vertex, 2] - tolerance), ]
    sjx.vtx <- sjx.vtx[sjx.vtx[ ,2] < (group.surf$vertex[which.vertex, 2] + tolerance), ]
    sjx.vtx <- sjx.vtx[sjx.vtx[ ,3] > (group.surf$vertex[which.vertex, 3] - tolerance), ]
    sjx.vtx <- sjx.vtx[sjx.vtx[ ,3] < (group.surf$vertex[which.vertex, 3] + tolerance), ]
    sjx.vtx <- matrix(sjx.vtx, ncol=3)
    
    vtx.temp <- rbind(sjx.vtx, group.surf$vertex[which.vertex, ])
    dist.vec <- as.matrix(dist(vtx.temp))[1:(nrow(vtx.temp)-1),nrow(vtx.temp)]
    dist.vec <- 1 - (dist.vec - mean(dist.vec))
    dist.vec <- dist.vec / sum(dist.vec)
    
    for (j in 1:n.var) {
      val.temp <- read.curv.vertex(curv.file = fls[i, j+1],
                                   surf.file = fls[i, 1],
                                   coords = sjx.vtx)
      df[i,j] <- sum(val.temp * dist.vec)
    }
    print(sprintf("%0.0f of %0.0f", i, n.sjx))
  }
  
}