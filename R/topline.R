topline <-
function(imagematrix, height_size = NA, width_size = NA) {
  if (is.na(height_size) | height_size <= 0) {
    stop("height_size not found")
  }
  if (is.na(width_size) | width_size <= 0) {
    stop("width_size not found")
  }

  escala_horizontal <- width_size/length(imagematrix[1, ])
  escala_vertical <- height_size/length(imagematrix[, 1])

  # Recortar a parte da imagem com pixeis
  corte <- light_gap(imagematrix, scale = F)
  corte[1] <- corte[1] + 1
  corte[2] <- length(imagematrix[1, ]) - corte[2]
  imagem_trabalho <- imagematrix[, corte[1]:corte[2]]

  # Criar fun??o para achar a maior coluna
  alt_max <- function(coluna) {
    if (sum(coluna == 1) > 0) {
      resposta <- which(coluna == 1)[1]
    } else {
      resposta <- length(coluna)
    }
    return(resposta)
  }

  # Perfil de altura
  alturas <- apply(imagem_trabalho, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
  dist_de_baixo <- length(imagematrix[, 1]) - alturas + 1

  altura_primeira_coluna <- dist_de_baixo[1]
  altura_ultimaa_coluna <- dist_de_baixo[length(dist_de_baixo)]

  meio <- NULL
  for (i in 1:(length(dist_de_baixo) - 1)) {
    meio[i] <- abs(dist_de_baixo[i] - dist_de_baixo[i + 1])
  }

  total_vertical <- altura_primeira_coluna + altura_ultimaa_coluna + sum(meio)  # quantos pixeis foram caminhados na horizontal
  total_vertical <- total_vertical * escala_vertical  # converter para a escala do estudo

  total_horizontal <- length(dist_de_baixo)
  total_horizontal <- total_horizontal * escala_horizontal  # converter para a escala do estudo

  resposta <- total_horizontal + total_vertical
  names(resposta) <- "topline"
  return(resposta)
}
