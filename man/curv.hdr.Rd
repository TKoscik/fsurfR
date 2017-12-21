\name{curv.hdr}
\alias{curv.hdr}
\title{Read CURV file header}
\description{}
\usage{
  curv.hdr(curv.file, field="all")
}
\arguments{
  \item{curv.file}{ Full file path to CURV file, including file extension if present.}
  \item{field}{ A character vector corresponding to the desired fields to read from the CURV file. Default is "All". Values: c("version", "num.vertex", "num.face", "num.values"). Note that older CURV files may not have "num.values" }
}
\value{A list object containing the requested fields: hdr$version, hdr$num.vertex, hdr$num.face, hdr$num.values}
\author{
  Timothy R. Koscik <timkoscik+fsurfR@gmail.com>
}
\examples{}