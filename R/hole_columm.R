hole_columm <-
function(imagematrix, color = 0, n_sections = "all") {
  numeros <- NULL
  medias <- NULL
  desvios <- NULL
  minimos <- NULL
  maximos <- NULL
  if (n_sections == "all") {
    sections <- 1:length(imagematrix[1, ])
    n_sections <- length(imagematrix[1, ])
  } else {
    intervalos <- floor(ncol(imagematrix)/n_sections)
    sections <- seq(from = 1, to = length(imagematrix[1, ]), by = intervalos)
  }

  for (i in 1:n_sections) {
    aup <- sections[i]
    aux <- hole_section_data(imagematrix[, aup], color = color)
    numeros[i] <- aux[1]
    medias[i] <- aux[2]
    desvios[i] <- aux[3]
    minimos[i] <- aux[4]
    maximos[i] <- aux[5]
  }
  maiores_estratos <- which(numeros == max(numeros[!is.na(numeros)]))
  resposta <- list(numeros, medias, desvios, minimos, maximos, maiores_estratos)
  names(resposta) <- c("N", "Mean", "SD", "Min","Max", "LH")
  return(resposta)
}




