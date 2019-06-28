light_gap <-
function(imagematrix, width_size = NA, scale = TRUE) {
  if (scale == T & (is.na(width_size) | width_size <= 0)) {
    stop("width_size not found")
  } else {
    correcao <- width_size/length(imagematrix[1, ])
  }
  primeira <- 0
  ultima <- 0
  teste <- 0

  # Encontrar primeira coluna com dados
  coluna_rodada <- 1
  while (teste == 0) {
    coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,                                            coluna_rodada])))
    aux_test <- sum(coluna_dados)
    if (aux_test > 0) {
      teste <- 1
      primeira <- coluna_rodada
    } else {
      coluna_rodada <- coluna_rodada + 1}}

  # Encontrar ultima coluna com dados
  coluna_rodada <- length(imagematrix[1, ])
  teste <- 0
  while (teste == 0) {
    coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,
                                                                                                 coluna_rodada])))
    aux_test <- sum(coluna_dados)
    if (aux_test > 0) {
      teste <- 1
      ultima <- coluna_rodada
    } else {
      coluna_rodada <- coluna_rodada - 1
    }
  }
  primeira <- primeira - 1
  ultima <- length(imagematrix[1, ]) - ultima

  if (scale == T) {
    primeira <- primeira * correcao
    ultima <- ultima * correcao
  }

  resposta <- c(primeira, ultima)
  names(resposta) <- c("Left gap size", "Right gap size")
  return(resposta)
}
