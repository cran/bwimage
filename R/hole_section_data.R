hole_section_data <-
function(section, color = 0) {
  matrix_burcaos <- hole_section(section)
  matrix_burcaos_limpa <- subset(matrix_burcaos, matrix_burcaos[, 4] == color)
  if (is.na(matrix_burcaos_limpa[1])) {
    numero <- 0
    media <- 0
    desvio <- NA
    minimo <- 0
    maximo <- 0
  } else {
    numero <- length(matrix_burcaos_limpa[, 3])
    media <- mean(matrix_burcaos_limpa[, 3])
    desvio <- sd(matrix_burcaos_limpa[, 3])
    minimo <- min(matrix_burcaos_limpa[, 3])
    maximo <- max(matrix_burcaos_limpa[, 3])
  }
  resposta <- c(numero, media, desvio, minimo, maximo)
  names(resposta) <- c("N", "Mean", "SD", "Min","Max")
  return(resposta)
}
