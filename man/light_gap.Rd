\name{light_gap}
\alias{light_gap}
\title{
Light gap
}
\description{
Left and right distances from first black pixel to image edge.
}
\usage{
light_gap(imagematrix, width_size = NA, scale = TRUE)
}

\arguments{
  \item{imagematrix}{
The matrix to be analysed.
}
  \item{width_size}{
Real size of image width (in mm, cm, m, etc..).
}
  \item{scale}{
If FALSE do not ajust the output for real size.
}
}

\value{
Distances without black pixel in each side of the picture
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

# Calculate vegetation Light gap in the bush image matrix
light_gap(bush_imagematrix,width_size=100)
# Conclusion: there is no light gap on both sides of bush image.
}