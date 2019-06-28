\name{hole_section}
\alias{hole_section}
\title{
Hole finder
}
\description{
Description of when a sequence of  same color pixel start and end. 
}
\usage{
hole_section(section)
}
\arguments{
  \item{section}{
Section to be analysed.
}
}

\value{
Description of start and end of each same color sequence. Result is given in cell number.
}
\references{
Zehm A, Nobis M, & Schwabe A. (2003). Multiparameter analysis of vertical vegetation structure based on digital image processing. Flora. doi: 10.1078/0367-2530-00086
}
\author{
Carlos Biagolini-Jr.
}



\seealso{
hole_section_data
threshold_color
}
\examples{
# First, get a matrix from your image. Here an example of a bush image is used.
bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush_imagematrix<-threshold_color(bush,  "jpeg",  "proportional", compress_rate = 0.1)

# Find pixel hole sections in the column 200 of bush image
hole_section(bush_imagematrix[,200])

# Find pixel hole sections in the row 250 of bush image
hole_section(bush_imagematrix[250,])
}