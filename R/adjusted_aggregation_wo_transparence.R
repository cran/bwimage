adjusted_aggregation_wo_transparence <-
function(imagematrix) {
  valor_corrigido <- NULL
  observed <- basic_aggregation_index(imagematrix)
  height1 <- length(imagematrix[, 1])
  width1 <- length(imagematrix[1, ])
  pixels1 <- sum(subset(as.vector(imagematrix), !is.na(as.vector(imagematrix))))

  max.index <- max_aggregation_wo_transparence(imagematrix)
  min.index <- min_aggregation_wo_transparence(imagematrix)
  range <- max.index - min.index
  obs.corrigido <- observed - min.index
  valor_corrigido <- obs.corrigido/range
  return(valor_corrigido)
}
