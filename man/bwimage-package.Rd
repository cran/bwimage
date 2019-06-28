\name{bwimage-package}
\alias{bwimage-package}
\alias{bwimage}
\docType{package}
\title{
\packageTitle{bwimage}
}
\description{
\packageDescription{bwimage}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{bwimage}
\packageIndices{bwimage}
A computational tool to describe patterns in black and white images from natural structures. 
}
\author{
\packageAuthor{bwimage}

Maintainer: \packageMaintainer{bwimage}
}
\references{
Nobis M & Hunziker U. (2005). Automatic thresholding for hemispherical canopy-photographs based on edge detection. Agr Forest Meteorol. 2005. doi: 10.1016/j.agrformet.2004.10.002
Zehm A, Nobis M, & Schwabe A. (2003). Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora. doi: 10.1078/0367-2530-00086
}

\concept{animal}
\concept{ecology methods}
\concept{field}
\concept{image analyses}
\concept{image processing}
\concept{vegetation patterns}

\examples{
bush<-system.file("extdata/bush.JPG", package ="bwimage")
bush_imagematrix<-threshold_color(bush, "jpeg", "proportional",compress_rate = 0.1)
aggregation_index(bush_imagematrix)
}