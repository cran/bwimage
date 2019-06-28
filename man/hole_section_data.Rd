\name{hole_section_data}
\alias{hole_section_data}
\title{
Summary of holes information
}
\description{
Summary information of holes of a given color in a given section. Result unit is the number of cell.
}
\usage{
hole_section_data(section, color = 0)
}
\arguments{
  \item{section}{
Section to be analysed.
}
  \item{color}{
Color of the hole (0 or 1).
}
}

\value{
\item{N}{Number of hole sections}
\item{Mean}{Mean size of hole sections}
\item{SD}{Standard deviation of hole sections size}
\item{Min}{Minimum size of hole sections}
\item{Max}{Maximum size of hole sections}
}
\references{
Zehm A, Nobis M, & Schwabe A. (2003). Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora. doi: 10.1078/0367-2530-00086
}
\author{
Carlos Biagolini-Jr.
}

\seealso{
hole_section
threshold_color
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)

# Detail information of white (0) holes sections in the column 200 of bush image
hole_section_data(bush_imagematrix[,200], color = 0)

# Detail information of black (1) holes sections in the row 250 of bush image
 hole_section_data(bush_imagematrix[250,], color = 1)
}