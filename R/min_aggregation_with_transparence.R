min_aggregation_with_transparence <-
function(imagematrix) {
  largura <- length(imagematrix[1, ])
  altura <- length(imagematrix[, 1])
  pixeis_org <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
  if (mean(pixeis_org) > 0.5) {
    mudar_de <- 0
    mudar_para <- 1
  } else {
    mudar_de <- 1
    mudar_para <- 0
  }
  px_preto_org <- sum(pixeis_org)
  px_branco_org <- length(pixeis_org) - sum(pixeis_org)
  dif_org <- px_preto_org - px_branco_org

  if (!altura%%2 == 0 & !largura%%2 == 0) {
    matrix_agr_min <- matrix(c(0, 1), ncol = largura + 1, nrow = altura)
    matrix_agr_min <- matrix_agr_min[, -length(matrix_agr_min[1, ])]
  } else {
    if (altura%%2 == 0 & !largura%%2 == 0) {
      matrix_agr_min <- matrix(c(0, 1), ncol = largura + 1, nrow = altura + 1)
      matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), -length(matrix_agr_min[1,
                                                                                            ])]
    } else {
      if (altura%%2 == 0) {
        matrix_agr_min <- matrix(c(0, 1), ncol = largura, nrow = altura + 1)
        matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), ]
      } else {
        matrix_agr_min <- matrix(c(0, 1), ncol = largura, nrow = altura)
      }
    }
  }  # Criar uma matrix de 1,0 em padrao de tabuleiro de xadrez - poderia fazer uma linha de comando mais curta, mas toda vez que temos uma matriz com numero de celular impar ou com numero de linhas par surgem problemas. Quando surge algum problema desses colocamos mais linhas ou colunas de forma que a matriz acabe sendo uma matriz de n de linhas impar e n de colunas par
  # gerar uma matrix auxiliar para calculo da agregacao
  for (c in 1:largura) {
    for (l in 1:altura) {
      if (is.na(imagematrix[l, c])) {
        matrix_agr_min[l, c] <- NA
      }
    }
  }  # Fazemos o espelhamento dos pontos de transparencia da imagem original
  pixeis_min <- subset(as.vector(matrix_agr_min), !is.na(as.vector(matrix_agr_min)))
  px_preto_min <- sum(pixeis_min)
  px_branco_min <- length(pixeis_min) - sum(pixeis_min)
  dif_min <- px_preto_min - px_branco_min
  if (dif_org < 0 & dif_min < 0) {
    if (!altura%%2 == 0 & !largura%%2 == 0) {
      matrix_agr_min <- matrix(c(1, 0), ncol = largura + 1, nrow = altura)
      matrix_agr_min <- matrix_agr_min[, -length(matrix_agr_min[1, ])]
    } else {
      if (altura%%2 == 0 & !largura%%2 == 0) {
        matrix_agr_min <- matrix(c(1, 0), ncol = largura + 1, nrow = altura + 1)
        matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), -length(matrix_agr_min[1,
                                                                                              ])]
      } else {
        if (altura%%2 == 0) {
          matrix_agr_min <- matrix(c(1, 0), ncol = largura, nrow = altura + 1)
          matrix_agr_min <- matrix_agr_min[-length(matrix_agr_min[, 1]), ]
        } else {
          matrix_agr_min <- matrix(c(1, 0), ncol = largura, nrow = altura)
        }
      }
    }
    for (c in 1:largura) {
      for (l in 1:altura) {
        if (is.na(imagematrix[l, c])) {
          matrix_agr_min[l, c] <- NA
        }
      }
    }  # Fazemos o espelhamento dos pontos de
    pixeis_min <- subset(as.vector(matrix_agr_min), !is.na(as.vector(matrix_agr_min)))
    px_preto_min <- sum(pixeis_min)
    px_branco_min <- length(pixeis_min) - sum(pixeis_min)
    dif_min <- px_preto_min - px_branco_min
  }
  n_pixeis_mudar <- abs(dif_org - dif_min)/2  # N de pixeis que vao mudar

  falta_modificar <- n_pixeis_mudar

  # Pintar os pixeis das quinas mais externos
  if (!is.na(matrix_agr_min[1, 1])) {
    if (falta_modificar > 0 & matrix_agr_min[1, 1] == mudar_de) {
      matrix_agr_min[1, 1] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_min[altura, 1])) {
    if (falta_modificar > 0 & matrix_agr_min[altura, 1] == mudar_de) {
      matrix_agr_min[altura, 1] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_min[1, largura])) {
    if (falta_modificar > 0 & matrix_agr_min[1, largura] == mudar_de) {
      matrix_agr_min[1, largura] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_min[altura, largura])) {
    if (falta_modificar > 0 & matrix_agr_min[altura, largura] == mudar_de) {
      matrix_agr_min[altura, largura] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }

  ###################################################### Pintado os pixeis mais externos que nao estao diretamente no calculo da agregacao #
  cabe_l1 <- length(which(matrix_agr_min[1, ] == mudar_de))  # quanto cabe na 1 linha - 18
  if (falta_modificar > 0) {
    if (falta_modificar <= cabe_l1) {
      # cabe tudo na primeira linha?
      matrix_agr_min[1, which(matrix_agr_min[1, ] == mudar_de)[1:falta_modificar]] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
    } else {
      # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
      # linhas mais extremas
      falta_modificar <- falta_modificar - cabe_l1
      matrix_agr_min[1, which(matrix_agr_min[1, ] == mudar_de)] <- mudar_para
    }
  }
  cabe_lu <- length(which(matrix_agr_min[altura, ] == mudar_de))  # quanto cabe na ultima linha - 23
  if (falta_modificar > 0) {
    if (falta_modificar <= cabe_lu) {
      # do que falta cabe tudo na ultima linha?
      matrix_agr_min[altura, which(matrix_agr_min[altura, ] == mudar_de)[1:falta_modificar]] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
    } else {
      # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
      # linhas mais extremas
      falta_modificar <- falta_modificar - cabe_lu
      matrix_agr_min[altura, which(matrix_agr_min[altura, ] == mudar_de)] <- mudar_para
    }
  }
  cabe_c1 <- length(which(matrix_agr_min[, 1] == mudar_de))  # quanto cabe na 1 linha - 2
  if (falta_modificar > 0) {
    if (falta_modificar <= cabe_c1) {
      # do que falta cabe tudo na primeira coluna?
      matrix_agr_min[which(matrix_agr_min[, 1] == mudar_de)[1:falta_modificar], 1] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
    } else {
      # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
      # linhas mais extremas
      falta_modificar <- falta_modificar - cabe_c1
      matrix_agr_min[which(matrix_agr_min[, 1] == mudar_de), 1] <- mudar_para
    }
  }
  cabe_cu <- length(which(matrix_agr_min[, largura] == mudar_de))  # quanto cabe na ultima linha - 3
  if (falta_modificar > 0) {
    if (falta_modificar <= cabe_cu) {
      # do que falta cabe tudo na ultima coluna?
      matrix_agr_min[which(matrix_agr_min[, largura] == mudar_de)[1:falta_modificar], largura] <- mudar_para  # se sim resolve tudo aqui e termina de montar a figura
    } else {
      # se nao modifica todos da primeira linha e vamos para proxima, a logica vai se repetira nas
      # linhas mais extremas
      falta_modificar <- falta_modificar - cabe_cu
      matrix_agr_min[which(matrix_agr_min[, largura] == mudar_de), largura] <- mudar_para
    }
  }

  ###################################################### Pintado as laterais, falta pintar as bordas com NA #
  localizacao_linha <- NULL
  localizacao_coluna <- NULL
  for (c in 2:(largura - 1)) {
    for (l in 2:(altura - 1)) {
      if (falta_modificar > 0 & (!is.na(matrix_agr_min[l, c]) & (matrix_agr_min[l, c] == mudar_de &
                                                                 (is.na(matrix_agr_min[l - 1, c]) | is.na(matrix_agr_min[l + 1, c]) | is.na(matrix_agr_min[l,
                                                                                                                                                           c - 1]) | is.na(matrix_agr_min[l, c + 1]))))) {
        localizacao_linha <- c(localizacao_linha, l)
        localizacao_coluna <- c(localizacao_coluna, c)
        falta_modificar <- falta_modificar - 1
      }
    }
  }
  if (length(localizacao_linha) > 0) {
    for (i in 1:length(localizacao_linha)) {
      matrix_agr_min[localizacao_linha[i], localizacao_coluna[i]] <- mudar_para
    }
  }

  ###################################################### Preencher o centro com pixeis pretos circuladamente # while que vai preenchendo pelo meio
  rodada <- 0
  while (falta_modificar > 0) {
    rodada <- rodada + 1
    cabe_l1 <- length(which(matrix_agr_min[rodada + 1, ] == mudar_de))
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_l1) {
        matrix_agr_min[rodada + 1, which(matrix_agr_min[rodada + 1, ] == mudar_de)[1:falta_modificar]] <- mudar_para
        falta_modificar <- 0
      } else {
        falta_modificar <- falta_modificar - cabe_l1
        matrix_agr_min[rodada + 1, which(matrix_agr_min[rodada + 1, ] == mudar_de)] <- mudar_para
      }
    }

    cabe_lu <- length(which(matrix_agr_min[altura - rodada, ] == mudar_de))
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_lu) {
        matrix_agr_min[altura - rodada, which(matrix_agr_min[altura - rodada, ] == mudar_de)[1:falta_modificar]] <- mudar_para
        falta_modificar <- 0
      } else {
        falta_modificar <- falta_modificar - cabe_lu
        matrix_agr_min[altura - rodada, which(matrix_agr_min[altura - rodada, ] == mudar_de)] <- mudar_para
      }
    }

    cabe_c1 <- length(which(matrix_agr_min[, rodada + 1] == mudar_de))
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_c1) {
        matrix_agr_min[which(matrix_agr_min[, rodada + 1] == mudar_de)[1:falta_modificar],
                       rodada + 1] <- mudar_para
        falta_modificar <- 0
      } else {
        falta_modificar <- falta_modificar - cabe_c1
        matrix_agr_min[which(matrix_agr_min[, rodada + 1] == mudar_de), rodada + 1] <- mudar_para
      }
    }

    cabe_cu <- length(which(matrix_agr_min[, largura - rodada] == mudar_de))
    if (falta_modificar > 0) {
      if (falta_modificar <= cabe_cu) {
        matrix_agr_min[which(matrix_agr_min[, largura - rodada] == mudar_de)[1:falta_modificar],
                       largura - rodada] <- mudar_para
        falta_modificar <- 0
      } else {
        falta_modificar <- falta_modificar - cabe_cu
        matrix_agr_min[which(matrix_agr_min[, largura - rodada] == mudar_de), largura - rodada] <- mudar_para
      }
    }
  }

  min.index <- basic_aggregation_index(matrix_agr_min)
  return(min.index)
}
