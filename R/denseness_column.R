denseness_column <-
function(imagematrix, n_sections = "all") {
  if (n_sections == "all") {
    n_sections <- ncol(imagematrix)
  } else {
    if (ncol(imagematrix) < n_sections) {
      n_sections <- ncol(imagematrix)}}
  tamanho_coluna <- floor(ncol(imagematrix)/n_sections)
  densidades <- NULL
  intervalos <- seq(from = 1, to = ncol(imagematrix), by = tamanho_coluna)

  for (i in 1:n_sections) {
    sub_amostra <- imagematrix[, intervalos[i]:(intervalos[i] + tamanho_coluna - 1)]
    densidades[i] <- denseness_total(sub_amostra)
  }
  media <- mean(densidades)
  desvio <- sd(densidades)
  resposta <- list(densidades, media, desvio)
  names(resposta) <- c("Denseness", "Mean", "SD")
  return(resposta)
}
