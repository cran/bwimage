\name{denseness_column}
\alias{denseness_column}
\title{Denseness in column sections}
\usage{
denseness_column(imagematrix, n_sections = "all")
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}

\item{n_sections}{Break the image in this number of columns.}
}
\value{
\item{Denseness}{Denseness of each column section.}
\item{Mean}{Mean of column sections denseness.}
\item{SD}{standard deviations of column sections denseness.}
}
\description{
Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of sections (n_sections). n_sections should be set as a number, in this situation denseness_column will break the original matrix in slices, and apply denseness_total function for each section. For instance, in a matrix of 1000x1000 if n_sections = 10, it will break to 10 sections of 1000x100 and analyze it. In other words, the sections will be the following sections of the original matrix [1:1000, 1:100] ,[ 1:1000,101:200] ,[ 1:1000,201:300] ,[ 1:1000,301:400] ,[ 1:1000,401:500] ,[ 1:1000,501:600] ,[ 1:1000,601:700] ,[ 1:1000,701:800] ,[ 1:1000,801:900] ,[ 1:1000,901:1000]. The default for parameter n_sections is "all", it will calculate denseness for each column of pixel. In other words, it will break the image in a number of section equal to the image pixel width.
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)

# Calculate vegetation denseness in 20 column sections
denseness_column(bush_imagematrix,20)
}
\references{
Zehm et al 2003 Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora-Morphology, Distribution, Functional Ecology of Plants, 198: 142-160.
}
\seealso{
denseness_total threshold_color
}
\author{
Carlos Biagolini-Jr.
}
