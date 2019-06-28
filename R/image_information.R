image_information <-
function(imagematrix) {
  resultado_em_processo <- NULL
  linhas_imagem <- nrow(imagematrix)
  colunas_imagem <- ncol(imagematrix)
  dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
  n_black_pixels <- sum(dados_pixeis)
  n_pixels <- length(dados_pixeis)
  n_white_pixels <- n_pixels - n_black_pixels
  n_transparent <- (linhas_imagem * colunas_imagem) - n_pixels
  width_BW_picture <- nrow(imagematrix)
  height_BW_picture <- ncol(imagematrix)

  resultado_em_processo[1] <- n_black_pixels
  resultado_em_processo[2] <- n_white_pixels
  resultado_em_processo[3] <- n_transparent
  resultado_em_processo[4] <- n_pixels
  resultado_em_processo[5] <- height_BW_picture
  resultado_em_processo[6] <- width_BW_picture

  names(resultado_em_processo) <- c("Black", "White", "Transparent", "Total","Height", "Width")
  return(resultado_em_processo)}




