max_aggregation_with_transparence <-
function(imagematrix) {
  largura <- length(imagematrix[1, ])
  altura <- length(imagematrix[, 1])
  if (altura > largura) {
    imagematrix <- apply(imagematrix, 1, rev)
    largura <- length(imagematrix[1, ])
    altura <- length(imagematrix[, 1])
  }
  pixeis_org <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
  if (mean(pixeis_org) > 0.5) {
    mudar_de <- 1
    mudar_para <- 0
  } else {
    mudar_de <- 0
    mudar_para <- 1
  }

  px_preto_org <- sum(pixeis_org)
  px_branco_org <- length(pixeis_org) - sum(pixeis_org)
  dif_org <- px_preto_org - px_branco_org
  matrix_agr_max <- matrix(mudar_de, ncol = largura, nrow = altura)
  # gerar uma matrix auxiliar para calculo da agregacao maxima
  for (c in 1:largura) {
    for (l in 1:altura) {
      if (is.na(imagematrix[l, c])) {
        matrix_agr_max[l, c] <- NA
      }
    }
  }  # Fazemos o espelhamento dos pontos de transparencia da imagem original


  falta_modificar <- min(px_preto_org, px_branco_org)
  # pintar as quinas
  if (!is.na(matrix_agr_max[1, 1])) {
    if (falta_modificar > 0 & matrix_agr_max[1, 1] == mudar_de) {
      matrix_agr_max[1, 1] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_max[altura, 1])) {
    if (falta_modificar > 0 & matrix_agr_max[altura, 1] == mudar_de) {
      matrix_agr_max[altura, 1] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_max[1, largura])) {
    if (falta_modificar > 0 & matrix_agr_max[1, largura] == mudar_de) {
      matrix_agr_max[1, largura] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }
  if (!is.na(matrix_agr_max[altura, largura])) {
    if (falta_modificar > 0 & matrix_agr_max[altura, largura] == mudar_de) {
      matrix_agr_max[altura, largura] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }

  # se ainda sobrou fazer pintar as colunas internas
  if (falta_modificar > 0) {
    pixeis_para_mudar_por_coluna <- NULL  # objeto que vai contar quantos pixeis tem disponivel para mudar de  cor em cada coluna
    for (i in 1:largura) {
      aux_pixeis <- subset(as.vector(matrix_agr_max[, i]), !is.na(as.vector(matrix_agr_max[,
                                                                                           i])))
      pixeis_para_mudar_por_coluna[i] <- length(aux_pixeis) - sum(aux_pixeis == mudar_para)
    }

    acumulado_pixeis_para_mudar <- NULL
    for (i in 1:largura) {
      acumulado_pixeis_para_mudar[i] <- sum(pixeis_para_mudar_por_coluna[1:i])
    }


    coluna_completa <- which(acumulado_pixeis_para_mudar > falta_modificar)[1] - 1  # colunas que tem que estar com 1 em todo o espaco
    coluna_completa <- as.numeric(coluna_completa)

    if (!coluna_completa == 0) {
      # temos mais de 1 coluna
      total_preenchido <- acumulado_pixeis_para_mudar[coluna_completa]  # total de pixeis que vao ser preenchidos com as colunas completas
      px_ult_coluna <- falta_modificar - total_preenchido  #pixeis da proxima coluna que vao ter que ser preenchidos
      for (c in 1:coluna_completa) {
        for (l in 1:altura) {
          if (!is.na(imagematrix[l, c])) {
            if (matrix_agr_max[l, c] == mudar_de) {
              matrix_agr_max[l, c] <- mudar_para
              falta_modificar <- falta_modificar - 1
            }
          }
        }
      }
    } else {
      # caso especial onde vai ser pintado so a primeira coluna
      px_ult_coluna <- falta_modificar  #pixeis da primeira coluna que vao ter que ser pintados
    }
  }
  if (falta_modificar > 0) {
    pixeis_para_modificar <- which(matrix_agr_max[, coluna_completa + 1] == mudar_de)[1:px_ult_coluna]  # numero dos pixeis que vao mudar
    for (i in 1:px_ult_coluna) {
      matrix_agr_max[pixeis_para_modificar[i], coluna_completa + 1] <- mudar_para
      falta_modificar <- falta_modificar - 1
    }
  }

  max.index <- basic_aggregation_index(matrix_agr_max)
  return(max.index)
}
