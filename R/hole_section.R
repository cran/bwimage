hole_section <-
function(section) {
  sequencia_de_modificacao <- diff(section)
  nao_nas_original <- which(!is.na(sequencia_de_modificacao))

  comecou <- section[which(!is.na(section))[1]]
  if (sum(sequencia_de_modificacao[nao_nas_original]^2) == 0) {
    matrix_secoes <- matrix(c(min(which(!is.na(section) == T)), max(which(!is.na(section) == T),
                                                                    2), 0, comecou), ncol = 4, nrow = 1)
    colnames(matrix_secoes) <- c("Start", "End", "Size", "Color")
  } else {
    transicoes_para_preto <- which(sequencia_de_modificacao == 1)
    transicoes_para_branco <- which(sequencia_de_modificacao == -1)
    transicoes <- sort(c(transicoes_para_preto, transicoes_para_branco))
    n_seccoes <- length(transicoes) + 1
    matrix_secoes <- matrix(NA, ncol = 4, nrow = n_seccoes)
    colnames(matrix_secoes) <- c("Start", "End", "size", "Color")

    # qual e a cor do primeiro nao NA
    matrix_secoes[1, 4] <- comecou
    inicios <- c(which(!is.na(section))[1], transicoes)
    matrix_secoes[, 1] <- inicios

    for (i in 2:length(matrix_secoes[, 1])) {
      if (comecou == 0) {
        comecou <- 1
      } else {
        comecou <- 0
      }
      matrix_secoes[i, 4] <- comecou
    }

    for (i in 1:(length(matrix_secoes[, 1]) - 1)) {
      valor <- matrix_secoes[i + 1, 1]
      matrix_secoes[i, 2] <- valor
    }

    matrix_secoes[length(matrix_secoes[, 1]), 2] <- max(which(!is.na(section) == T))
  }

  matrix_secoes[, 3] <- matrix_secoes[, 2] - matrix_secoes[, 1] + 1
  return(matrix_secoes)
}
