\name{denseness_total}
\alias{denseness_total}
\title{
Denseness for whole image
}
\description{
Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
}
\usage{
denseness_total(imagematrix)
}
\arguments{
  \item{imagematrix}{
The matrix to be analysed.
}
}

\value{
Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
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
# Get a matrix from your image. Here  examples provided by bwimage package.

# I) Calculate vegetation denseness 
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
denseness_total(bush_imagematrix)

# II) Calculate canopy openness 
# Convert image into binary matrix
canopy<-system.file("extdata/canopy.JPG",package ="bwimage")
canopy_matrix<-threshold_color(canopy,compress_method="proportional",compress_rate=0.1)
1-denseness_total(canopy_matrix) # canopy openness

}