\name{stretch}
\alias{stretch}
\title{stretch circle to square}
\usage{
stretch(imagematrix, method = "radial")
}
\arguments{
\item{imagematrix}{The matrix to be stretched.}

\item{method}{Stretch algorithm. Four algorithms (radial, shirley, squircle, and elliptical) are available to stretch the image. The algorithms were adapted from Lambers 2016.}
}
\value{
A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
}
\description{
Stretch data from circular image to square in binary matrix
}
\examples{
img_location <- system.file("extdata/chesstable.png",package ="bwimage")
image_matrix<- threshold_color(img_location,"png", "frame_fixed",target_width = 50,target_height=50)
stretch(image_matrix,method="radial")
}
\references{
Lambers 2016 Mappings between Sphere, Disc, and Square. Journal of Computer Graphics Techniques, 5(2): 1-21.
}
\author{
Carlos Biagolini-Jr.
}
