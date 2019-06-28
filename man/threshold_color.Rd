\name{threshold_color}
\alias{threshold_color}
\title{
Image to matrix - Single
}
\description{
Convert a single image into a matrix
}
\usage{
threshold_color(filename, filetype = "jpeg", compress_method = "none", 
compress_rate = 1, target_width = 100, target_height = 100, 
black_regulation = 0.5, transparency_regulation = 0.5)
}

\arguments{
  \item{filename}{
Name of the file to be load - ex: "Figure01.JPG".
}
  \item{filetype}{
Type of the file to be load. Compatible file types: ".JPGE", ".JPG" or ".PNG".
}
  \item{compress_method}{
For high resolution files, i.e. numbers of pixels in width and height, it is suggested to reduce the resolution to create a smaller matrix, it strongly reduce GPU usage and time necessary to run analyses. On the other hand, by reducing resolution, it will also reduce the accuracy of data description. Methods for image compression: (i) frame_fixed: compress images to a desired target width and height; (ii) proportional:  compress the image by a given ratio provide in the argument "proportion"; (iii) width_fixed: compress images to get a target width. The image height will also be reduced, in the same intensity. For instance, if the original file had 1000 pixel in width, and width_fixed was set to 100, the height reduction will reduce in a rate of 0.1 (100/1000) ; (iv) height_fixed: analogous to width_fixed, but assumes height as reference.
}
  \item{compress_rate}{
Compress rate to by apply if compress_method=proportional. Note:  it should be ser as number range from 0 to 1 .
}
  \item{target_width}{
Target width to be used if compress_method=frame_fixed or compress_method= width_fixed.
}
  \item{target_height}{
Target height to be used if compress_method=frame_fixed or compress_method= height_fixed.
}
  \item{black_regulation}{
For each pixel, the intensity of red, green and blue is averaged and compared to a black_regulation (threshold).
If the average intensity is less than the black_regulation (default is 0.5) the pixel will be set as black, otherwise it will be white.
}
  \item{transparency_regulation}{
For PNG images, the alpha channel is used to set transparent pixels, i.e. alpha channel values above transparency_regulation (a threshold) will set the pixel as transparent, default is 0.5.  NOTE: In the data matrix the value 1 represents black pixels, 0 represents white pixels and NA represents transparent pixels.
}
}


\value{
A matrix of 0, 1 and NA representing white, black and transparent pixels, respectively.
}

\author{
Carlos Biagolini-Jr.
}


\examples{
bush<-system.file("extdata/bush.JPG",package ="bwimage")
threshold_color(bush,"jpeg", "frame_fixed",target_width = 15,target_height=15)

# For your images, if the file is in the working directory type:
# threshold_color("FILE_NAME.EXTENSION", filetype ="FILE_EXTENSION")
# or, if the file is in the other directory:
# threshold_color("C:/PATH TO FILE FOLDER/YOUR_FILE_NAME.EXTENSION", "FILE_EXTENSION")
}