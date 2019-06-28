heigh_maximum <-
function(imagematrix, height_size) {
  if (height_size <= 0) {
    stop("height_size must be greater than zero")
  }
  # Criar funcao para achar a maior coluna
  alt_max <- function(coluna) {
    if (sum(coluna == 1) > 0) {
      resposta <- which(coluna == 1)[1]
    } else {
      resposta <- length(coluna)
    }
    return(resposta)
  }
  alturas <- apply(imagematrix, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
  dist_de_baixo <- length(imagematrix[, 1]) - alturas
  dist_de_baixo[!dist_de_baixo == 0] <- dist_de_baixo[!dist_de_baixo == 0] + 1  # somar 1 para corrigir que apenas as que n?o tem nenhum pixel preto vao ter altura de zero
  maior_coluna <- which(dist_de_baixo == max(dist_de_baixo))[1]  #qual coluna tem o pixel mais alto (se for mais de uma como mesmo maximo pega so a primeira)
  altura_maxima <- dist_de_baixo[maior_coluna]
  altura_maxima <- altura_maxima * (height_size/length(imagematrix[, 1]))

  names(altura_maxima) <- c("Height")
  return(altura_maxima)
}
