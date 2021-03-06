\name{image_information}
\alias{image_information}
\title{Summary of image information}
\usage{
image_information(imagematrix)
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}
}
\value{
\item{Black}{Number of black pixels}
\item{White}{Number of white pixels}
\item{Transparent}{Number of transparent pixels}
\item{Total}{Total number of pixels}
\item{Height}{Size in height}
\item{Width}{Size in width}
}
\description{
Provide the information of: number of black, white and transparent pixels, total number of pixels, height and width size.
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
image_information(bush_imagematrix)
}
\seealso{
threshold_color
}
\author{
Carlos Biagolini-Jr.
}
