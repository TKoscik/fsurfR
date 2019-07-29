write.surf <- function(fname,
                       surface.vtx) {

#   # Debug
#   library(fsurfR)
#   surf.file <- "/Shared/koscikt_scratch/fsurf_avg/surf/lh.pial"
#   surf.info <- read.surf(surf.file)
#   
# library(geometry)
# 
# fid <- file(fname, "wb")
# endian <- "big"
# 
# n.vtx <- nrow(surface.vtx)
# faces <- surf.tri(surface.vtx, delaunayn(surface.vtx))
# 
# # Write Version
# writeBin(255, fid, "int", size=1, endian=endian)
# writeBin(255, fid, "int", size=1, endian=endian)
# writeBin(254, fid, "int", size=1, endian=endian)
# 
# writeLines(sprintf("created by fsurfR on %s", Sys.time()), fid)
# writeLines("", fid)
# writeBin(n.vtx, fid, integer(), size=4, endian=endian)
# writeBin(n.vtx, fid, integer(), size=4, endian=endian)
}
                       
