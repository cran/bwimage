altitudinal_profile <-
function(imagematrix, n_sections, height_size) {
  if ((length(imagematrix[1, ])/2) < n_sections) {
    stop(paste("Sections width must have at least two cells. Please choose a number of n_sections lower or equal to ",
               length(imagematrix[1, ])/2))
  }

  tamanho_coluna <- floor(ncol(imagematrix)/n_sections)
  dist_de_baixo <- NULL
  intervalos <- seq(from = 1, to = ncol(imagematrix), by = tamanho_coluna)

  for (i in 1:n_sections) {
    sub_amostra <- imagematrix[, intervalos[i]:(intervalos[i] + tamanho_coluna - 1)]
    aux_maximums <- heigh_maximum(sub_amostra, height_size = height_size)
    dist_de_baixo[i] <- aux_maximums
  }

  media <- mean(dist_de_baixo)
  desvio <- sd(dist_de_baixo)

  resposta <- list(media, desvio, dist_de_baixo)
  names(resposta) <- c("Mean", "SD","Size")
  return(resposta)}
