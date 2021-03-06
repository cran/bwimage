\name{denseness_sample}
\alias{denseness_sample}
\title{Denseness in samples}
\usage{
denseness_sample(imagematrix, width_size, height_size, sample_width,
  sample_height, method = "random", sample_shape = "rectangle",
  n_samples = 10, n_sample_horizontal = 10, n_sample_vertical = 1,
  proportion_horizontal = 1, proportion_vertical = 1,
  aligin_horizontal = "center", aligin_vertical = "bottom")
}
\arguments{
\item{imagematrix}{The matrix to be analysed.}

\item{width_size}{Real size of image width (in mm, cm, m, etc..).}

\item{height_size}{Real size of image height (in mm, cm, m, etc..).}

\item{sample_width}{Width of sample area.}

\item{sample_height}{Height of sample area.}

\item{method}{Method for sample ("random" or "uniform").}

\item{sample_shape}{The shape of sample unity ("rectangle" or "ellipse"). See plot_samples function.}

\item{n_samples}{Defines the number of samples, when sample_shape="random".}

\item{n_sample_horizontal}{Defines the number of samples column, when sample_shape=" uniform".}

\item{n_sample_vertical}{Defines the number of samples lines, when sample_shape=" uniform".}

\item{proportion_horizontal}{Range from 0 to 1. Represent the proportion of horizontal plane to be sample.  If proportion_horizontal=1 (default) all columns beacome potentially sample.}

\item{proportion_vertical}{Range from 0 to 1. Represent the proportion of vertical plane to be sample.  If proportion_vertical=1 (default) all lines become potentially sample.}

\item{aligin_horizontal}{Define horizontal align.  Three options are available: "center", "left" or "right".}

\item{aligin_vertical}{Define vertical align.  Three options are available: "middle","bottom" or "top".}
}
\value{
\item{Sample_denseness}{Proportion of black pixels in samples. It do not take into account transparent pixels (when present)..}
\item{Height}{Height of each sample (in mm, cm, m, etc. ..). Central point used as reference.}
\item{Distance(left)}{Distance ti the left side of each sample (in mm, cm, m, etc. ..). Central point used as reference.}
\item{Matrix(line)}{Imagem matrix  line coordinates.}
\item{Matrix(column)}{Imagem matrix column coordinates.}
}
\description{
Calculate the denseness (proportion of black pixel in relation to the total number of pixels) for a given number of samples.
}
\examples{
# Get a matrix from your image. Here  examples provided by bwimage package.

bush<-system.file("extdata/bush.JPG",package ="bwimage")
bush<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
denseness_sample(bush, width_size=100, height_size=100, sample_width=5, sample_height=5)
}
\seealso{
plot_samples
}
\author{
Carlos Biagolini-Jr.
}
