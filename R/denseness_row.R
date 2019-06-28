denseness_row <-
function(imagematrix, n_sections = "all") {
  if (n_sections == "all") {
    n_sections <- nrow(imagematrix)
  } else {
    if (nrow(imagematrix) < n_sections) {
      n_sections <- nrow(imagematrix)
    }}
  tamanho_linha <- floor(nrow(imagematrix)/n_sections)
  densidades <- NULL
  intervalos <- seq(from = 1, to = nrow(imagematrix), by = tamanho_linha)
  for (i in 1:n_sections) {
    sub_amostra <- imagematrix[intervalos[i]:(intervalos[i] + tamanho_linha - 1), ]
    densidades[i] <- denseness_total(sub_amostra)
  }
  media <- mean(densidades)
  desvio <- sd(densidades)
  resposta <- list(densidades, media, desvio)
  names(resposta) <-c("Denseness", "Mean", "SD")
  return(resposta)}
