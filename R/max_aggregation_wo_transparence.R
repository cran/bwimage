max_aggregation_wo_transparence <-
function(imagematrix) {
  height <- length(imagematrix[, 1])
  width <- length(imagematrix[1, ])
  total_px <- height * width
  px_analisados <- (height - 2) * (width - 2)
  total_preto <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))
  total_branco <- total_px - total_preto
  cor_rara <- min(total_preto, total_branco)

  if (height > width) {
    menor_lado <- width
  } else {
    menor_lado <- height
  }
  sobra <- cor_rara

  if (sobra <= 4) {
    custo_maximo <- 0
  } else {
    # 4 pixeis podem ir nass quinas e sairem de graca se cor rara cabe no menor lado + quinas do outro
    # lado
    if (cor_rara - 2 <= menor_lado) {
      custo_maximo <- 0.25 * (sobra - 4)
    } else {
      rebarba1 <- cor_rara%%menor_lado
      rebarba2 <- (cor_rara - 1)%%menor_lado
      rebarba3 <- (cor_rara - 2)%%menor_lado

      if (rebarba1 == 0 | rebarba2 == 0 | rebarba3 == 0) {
        custo_maximo <- (menor_lado - 2) * 0.5
      } else {
        if (rebarba1 == 0 | rebarba2 == 0 | rebarba3 == 0 | rebarba1 == menor_lado - 1 | rebarba2 ==
            menor_lado - 1 | rebarba3 == menor_lado - 1) {
          custo_maximo <- ((menor_lado - 2) * 0.5) + 0.25
        } else {
          custo_maximo <- ((menor_lado - 2) * 0.5) + 0.5
        }
      }
    }
  }


  pixeis_usados_org <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), !is.na(as.vector(imagematrix[-c(1,
                                                                                                                          height), -c(1, width)]))))
  pixeis_usados_NAs <- length(subset(as.vector(imagematrix[-c(1, height), -c(1, width)]), is.na(as.vector(imagematrix[-c(1,
                                                                                                                         height), -c(1, width)]))))
  px_analisados <- (pixeis_usados_org - pixeis_usados_NAs)
  calculo_maximo <- (px_analisados - custo_maximo)/px_analisados


  return(calculo_maximo)
}
