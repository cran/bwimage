heigh_propotion <-
function(imagematrix) {
  total_row <- NULL
  for (i in 1:length(imagematrix[, 1])) {
    dados_pixeis <- subset(as.vector(imagematrix[i, ]), !is.na(as.vector(imagematrix[i, ])))
    total_row[i] <- sum(dados_pixeis)
  }
  n_pixels <- sum(total_row)
  porporcoes <- total_row/n_pixels
  acumulado <- rep(0, length(porporcoes))
  acumulado[length(porporcoes)] <- porporcoes[length(porporcoes)]

  for (i in seq(from = (length(porporcoes) - 1), to = 1)) {
    acumulado[i] <- porporcoes[i] + acumulado[i + 1]
  }
  return(acumulado)
}
