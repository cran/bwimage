min_aggregation_wo_transparence <-
function(imagematrix) {
  height <- length(imagematrix[, 1])
  width <- length(imagematrix[1, ])
  total_px <- (height) * (width)
  px_analisados <- (height - 2) * (width - 2)
  total_preto <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))
  total_branco <- total_px - total_preto
  cor_rara <- min(total_preto, total_branco)
  custo1 <- NULL
  custo2 <- NULL
  custo3 <- NULL
  custo4 <- NULL
  if (!height%%2 == 0 & !width%%2 == 0) {
    # Condicao matrix impar x impar
    sobra <- ((total_px - 1)/2) - cor_rara  # calcul de quantos pixeis tem que mudar de cor
    if (sobra == 0) {
      calculo_minimo <- 0
    } else {
      cabe_fora <- (height - 1) + (width - 1)  # Calcula quantos pixeis cabem do lado de fora da imagem
      cabe_1camada <- (height - 3) + (width - 3)  # Calcula quantos pixeis cabem na primeira camada de pixeis dentro da imagem
      if (sobra <= cabe_fora) {
        # o que sobrou cabe na camada mais externa
        calculo_minimo <- sobra * 0.25
      } else {
        if (sobra <= (cabe_fora + cabe_1camada)) {
          # o que sobrou cabe na camada mais externa e primeira camada
          custo1 <- cabe_fora * 0.25
          custo2 <- (sobra - cabe_fora) * 1.75
          calculo_minimo <- custo1 + custo2
        } else {

          custo1 <- cabe_fora * 0.25
          custo2 <- cabe_1camada * 1.75
          custo3 <- (sobra - cabe_fora - cabe_1camada) * 2
          calculo_minimo <- custo1 + custo2 + custo3
        }
      }
    }
  } else {
    # Condicao matrix par x impar ou par x par
    sobra <- (total_px/2) - cor_rara
    if (sobra <= 2) {
      calculo_minimo <- 0
    } else {
      cabe_fora <- (height - 2) + (width - 2)  # Calcula quantos pixeis cabem do lado de fora da matriz de analise - as quinas
      cabe_1camada_interno <- (height - 4) + (width - 4)  # Calcula quantos pixeis cabem do lado na primeira camada
      cabe_1camada_quina <- 2

      if (sobra <= (2 + cabe_fora)) {
        # o que sobrou cabe nos espacos da camada mais externa?
        calculo_minimo <- (sobra - 2) * 0.25
      } else {
        if (sobra <= (2 + cabe_fora + cabe_1camada_quina)) {
          # o que sobrou cabe nos espacos da camada mais externa + quina da primeira camada?
          custo1 <- cabe_fora * 0.25
          custo2 <- (sobra - 2 - cabe_fora) * 1.5
          calculo_minimo <- custo1 + custo2
        } else {
          if (sobra <= (2 + cabe_fora + cabe_1camada_quina + cabe_1camada_interno)) {
            # o que sobrou cabe nos espacos da camada mais externa + quina+ porcao interna da primeira camada?
            custo1 <- cabe_fora * 0.25
            custo2 <- cabe_1camada_quina * 1.5
            custo3 <- (sobra - 2 - cabe_fora - cabe_1camada_quina) * 1.75
            calculo_minimo <- custo1 + custo2 + custo3

          } else {
            # Aqui, foram usados todos os pixeis mais baratos e o restante caiu no mais caro

            custo1 <- cabe_fora * 0.25
            custo2 <- cabe_1camada_quina * 1.5
            custo3 <- cabe_1camada_interno * 1.75
            custo4 <- (sobra - 2 - cabe_fora - cabe_1camada_quina - cabe_1camada_interno) *
2
            calculo_minimo <- custo1 + custo2 + custo3 + custo4
          }
        }
      }
    }
  }
  pixeis_usados_org <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), !is.na(as.vector(imagematrix[-c(1,
                                                                                                                          height), -c(1, width)]))))
  pixeis_usados_NAs <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), is.na(as.vector(imagematrix[-c(1,
                                                                                                                         height), -c(1, width)]))))
  px_analisados <- (pixeis_usados_org - pixeis_usados_NAs)
  resposta <- calculo_minimo/px_analisados
  return(resposta)
}
