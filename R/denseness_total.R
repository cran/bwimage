denseness_total <-
function(imagematrix) {
  dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
  n_black_pixels <- sum(dados_pixeis)
  n_pixels <- length(dados_pixeis)
  p_black_pixels <- n_black_pixels/n_pixels
  return(p_black_pixels)}
