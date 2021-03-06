\name{compress}
\alias{compress}
\title{Compress square to circle}
\usage{
compress(imagematrix, method = "radial", background = NA)
}
\arguments{
\item{imagematrix}{The matrix to be compressed.}

\item{method}{Compress algorithm. Four algorithms (radial, shirley, squircle, and elliptical) are available to stretch the image. The algorithms were adapted from Lambers 2016.}

\item{background}{Code for background cell value. When compressing a squared matrix, corners of the transformed matrix will no have corresponding pixel from original matrix. Thus, the background value will be the value of transformed matrix corners.}
}
\value{
A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
}
\description{
Compress data from square image to circular in binary matrix
}
\examples{
img_location <- system.file("extdata/chesstable.png",package ="bwimage")
image_matrix<- threshold_color(img_location,"png", "frame_fixed",target_width = 50,target_height=50)
compress(image_matrix,method="radial")
}
\references{
Lambers 2016 Mappings between Sphere, Disc, and Square. Journal of Computer Graphics Techniques, 5(2): 1-21.
}
\author{
Carlos Biagolini-Jr.
}
