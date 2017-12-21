\name{init.mgh}
\alias{init.mgh}
\title{Initialize MGH File}
\description{}
\usage{
  init.mgh(file.name, dims.whdn, type = c(0,1,3,4), dof = 0, goodRASFlag = 1, spacing = c(1,1,1), x_ras = c(-1,0,0), y_ras = c(0,0,-1), z_ras = c(0,1,0), c_ras = c(0,0,0))
}
\arguments{
  \item{file.name}{Full file path to desire MGH file. Will not overwrite a file if file exists.}
  \item{dims.whdn}{Dimensions of volume to be initialized in number of voxels, width, height, depth, number of frames (x,z,y,t)}
  \item{type}{The type of values stored in each image.}
  \item{dof}{}
  \item{goodRASFlag}{}
  \item{spacing}{}
  \item{x_ras}{}
  \item{y_ras}{}
  \item{z_ras}{}
  \item{c_ras}{}
}
\value{}
\author{
  Timothy R. Koscik <timkoscik+fsurfR@gmail.com>
}
\examples{}