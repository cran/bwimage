\name{heigh_propotion}
\alias{heigh_propotion}
\title{
Cumulative denseness for each line
}
\description{
Proportion of black pixel below each matrix line.
}
\usage{
heigh_propotion(imagematrix)
}
\arguments{
  \item{imagematrix}{
The matrix to be analysed.
}
}

\value{
Proportion of black pixel below each matrix line.
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

# Proportion of black pixel below each matrix line.
heigh_propotion(bush_imagematrix)
}