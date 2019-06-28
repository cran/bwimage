\name{heigh_maximum}
\alias{heigh_maximum}
\title{
Height of the highest black pixel in the image
}
\description{
Find the higher black pixel in the whole image.
}
\usage{
heigh_maximum(imagematrix, height_size)
}
\arguments{
  \item{imagematrix}{
The matrix to be analysed.
}
  \item{height_size}{
Real size of image width (in mm, cm, m, etc..).
}
}

\value{
Height of the highest black pixel. It is scaleted for the real size (in mm, cm, m, etc..) based in the information from argument height_size.
}
\references{
Zehm A, Nobis M, & Schwabe A. (2003). Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora. doi: 10.1078/0367-2530-00086
}
\author{
Carlos Biagolini-Jr.
}


\seealso{
threshold_color
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)

# Calculate height of the highest black pixel in the bush image matrix
heigh_maximum(bush_imagematrix,height_size=100)
# Conclusions: The highest vegetation unit ,i.e. highest black pixel, is 84.4 cm above ground.
}