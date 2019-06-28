\name{heigh_propotion_test}
\alias{heigh_propotion_test}
\title{
Cumulative denseness test
}
\description{
Find the height which a given proportion of black pixel is found.
}
\usage{
heigh_propotion_test(imagematrix, proportion, height_size)
}
\arguments{
  \item{imagematrix}{
The matrix to be analysed.
}
  \item{proportion}{
proportion of denseness to test
}
  \item{height_size}{
Real size of image height (in mm, cm, m, etc..).
}
}

\value{
The height which a given proportion of black pixel is found.
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

# See the proportion of black pixels (1) below each bush image matrix row
heigh_propotion_test(bush_imagematrix,0.75,100)
# Conclusion: in this imagem, 75 percent of the vegetation is hold below 31.2 cm.
}