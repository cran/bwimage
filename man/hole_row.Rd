\name{hole_row}
\alias{hole_row}
\title{Holes description in row sections}
\usage{
hole_row(imagematrix, color = 0, n_sections = "all")
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}

\item{color}{Color of the hole (0 or 1).}

\item{n_sections}{Sample this number of rows.}
}
\value{
\item{N}{Number of sections.}
\item{Mean}{Mean sections size.}
\item{SD}{standard deviations of sections size.}
\item{Min}{Minimum sections size sections size.}
\item{Max}{Maximum sections size.}
\item{LH}{Stratum with largest hole count.}
}
\description{
Summary information of holes in a given number of rows (n_sections). n_sections must be set as a number, in this situation hole_row will sample rows, and apply hole_section_data function for each section. Next, all results will be display on hole_columm output. Example of how row sample works: in a matrix of 250x250 if n_sections =  5 , it will sample rows 1,51,101,151, and 201 and analyze it. In other words, the sections will be following sections of the original matrix [1,1:250] , [51,1:250] , [101,1:250] , [151,1:250] , [201,1:250]. The default for parameter n_sections is "all", it will calculate hole_section_data for each row of pixel. In other words, it will break the image in a number of section equal to the image pixel height.
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)

# Information of white (i.e. 0s in matrix) holes in 10 rows uniformly sample among matrix.
hole_row(bush_imagematrix, n_sections=10)

# Information of black (i.e. 1s in matrix) holes in 15 rows uniformly sample among matrix.
hole_row(bush_imagematrix, n_sections=15)
}
\seealso{
hole_section_data threshold_color
}
\author{
Carlos Biagolini-Jr.
}
