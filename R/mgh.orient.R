mgh.orient <- function(mgh.file) {
  
  stopifnot(file.exists(mgh.file))
  
  hdr <- mgh.hdr(mgh.file, c("width", "height", "depth", "spacing", "x_ras", "y_ras", "z_ras", "c_ras"))
  
  delta <- hdr$spacing
  Mdc <- matrix(c(hdr$x_ras, hdr$y_ras, hdr$z_ras), nrow=3)
  Pxyz_c <- hdr$c_ras
  
  D <- diag(delta)
  
  Pcrs_c <- c(hdr$width/2, hdr$height/2, hdr$depth/2)
  
  Pxyz_0 = Pxyz_c - Mdc%*%D%*%Pcrs_c
  
  tform <- rbind(cbind(Mdc %*% D, Pxyz_0), c(0,0,0,1))
  return(tform)
}