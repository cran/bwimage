\name{denseness_total}
\alias{denseness_total}
\title{Denseness for whole image}
\usage{
denseness_total(imagematrix)
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}
}
\value{
Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
}
\description{
Proportion of black pixels in relation to all pixels. It do not take into account transparent pixels (when present).
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
canopy_matrix<-threshold_color(canopy,"jpeg", compress_method="proportional",compress_rate=0.1)
1-denseness_total(canopy_matrix) # canopy openness
}
\references{
Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
}
\seealso{
threshold_color
}
\author{
Carlos Biagolini-Jr.
}
