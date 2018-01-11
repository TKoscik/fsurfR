surf.to.group <- function(data.dir,
                          save.dir,
                          which.sjx = "all",
                          hemi = c("lh", "rh"),
                          var.name = c("curv", "thickness", "volume"),
                          tolerance = 1, verbose = TRUE) {
  
  # Debug ----
  # rm(list=ls())
  # gc()
  # data.dir <- "/Shared/shizhan/PNC/55489/koscikt/fsurf"
  # save.dir <- "/Shared/shizhan/PNC/55489/koscikt/fsurf_resample"
  # which.sjx <- "all"
  # hemi <- c("lh", "rh")
  # var.name <- c("curv", "thickness")
  # tolerance <- 1
  #----
  
  for (i in 1:length(hemi)) {
    file.copy(from = paste0(data.dir, "/fsaverage/surf/", hemi[i], ".sphere.reg"),
              to = save.dir)
    group.surf <- read.surf(paste0(data.dir, "/fsaverage/surf/", hemi[i], ".sphere.reg"))
    fls <- parse.surf(data.dir, which.sjx, hemi[i], c("sphere.reg", var.name))
    n.sjx <- nrow(fls)
    id.ls <- list.dirs(data.dir, full.names = F, recursive = F)[1:n.sjx]
    n.var <- length(var.name)
    n.vtx <- group.surf$n.vertex
    
    for (j in 1:n.sjx) {
      sjx.surf <- read.surf(fls[j,1])
      
      for (k in 1:n.var) {
        init.curv(file.name = paste0(save.dir, "/", id.ls[j], ".", hemi[i], ".", var.name[k]),
                  surf.file = paste0(save.dir, "/", hemi[i], ".sphere.reg"))
        
        sjx.crv <- read.curv.vertex(curv.file = fls[j, k+1], n = "all")
        
        for (l in 1:n.vtx) {
          sjx.vtx <- matrix(sjx.surf$vertex[sjx.surf$vertex[ ,1] > (group.surf$vertex[l, 1] - tolerance), ], ncol=3)
          sjx.vtx <- matrix(sjx.vtx[sjx.vtx[ ,1] < (group.surf$vertex[l, 1] + tolerance), ], ncol=3)
          sjx.vtx <- matrix(sjx.vtx[sjx.vtx[ ,2] > (group.surf$vertex[l, 2] - tolerance), ], ncol=3)
          sjx.vtx <- matrix(sjx.vtx[sjx.vtx[ ,2] < (group.surf$vertex[l, 2] + tolerance), ], ncol=3)
          sjx.vtx <- matrix(sjx.vtx[sjx.vtx[ ,3] > (group.surf$vertex[l, 3] - tolerance), ], ncol=3)
          sjx.vtx <- matrix(sjx.vtx[sjx.vtx[ ,3] < (group.surf$vertex[l, 3] + tolerance), ], ncol=3)
          
          if (length(sjx.vtx) != 0) {
            vtx.temp <- rbind(sjx.vtx, group.surf$vertex[l, ])
            dist.vec <- as.matrix(dist(vtx.temp))[1:(nrow(vtx.temp)-1),nrow(vtx.temp)]
            dist.vec <- 1 - (dist.vec - mean(dist.vec))
            dist.vec <- dist.vec / sum(dist.vec)
            
            write.curv.vertex(
              curv.file = paste0(save.dir, "/", id.ls[j], ".", hemi[i], ".", var.name[k]),
              n = l, values = sum(val.temp * dist.vec))
            if (verbose) {
              print(sprintf("%0.0f of %0.0f Hemis; %0.0f of %0.0f Subjects; %0.0f of %0.0f Variables; %0.0f of %0.0f Vertices",
                            i, length(hemi), j, n.sjx, k, n.var, l, n.vtx))
            }
          }
        }
      }
    }
  }
}
