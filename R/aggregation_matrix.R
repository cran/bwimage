aggregation_matrix <-
function(imagematrix) {
  linhas_imagem <- nrow(imagematrix)
  colunas_imagem <- ncol(imagematrix)
  matrix_agregacao <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
  for (c in 2:(colunas_imagem - 1)) {
    for (l in 2:(linhas_imagem - 1)) {
      if (is.na(imagematrix[l, c]) | is.na(imagematrix[l - 1, c]) | is.na(imagematrix[l + 1,
                                                                                      c]) | is.na(imagematrix[l, c - 1]) | is.na(imagematrix[l, c + 1])) {
        aux.indice <- NA
      } else {
        soma.cell.visinha <- sum(imagematrix[l - 1, c], imagematrix[l + 1, c], imagematrix[l,
                                                                                           c - 1], imagematrix[l, c + 1])
        if (imagematrix[l, c] == 1) {
          aux.indice <- soma.cell.visinha/4
        } else {
          aux.indice <- (4 - soma.cell.visinha)/4
        }
        matrix_agregacao[l, c] <- aux.indice
      }
      matrix_agregacao[l, c] <- aux.indice
    }
  }
  # values<-subset(as.vector(matrix_agregacao), !is.na(as.vector(matrix_agregacao))) # Jogando todos
  # os dados para um unico objeto average_aggregation<-mean(values)
  return(matrix_agregacao)
}
