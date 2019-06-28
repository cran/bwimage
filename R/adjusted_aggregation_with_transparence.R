adjusted_aggregation_with_transparence <-
function(imagematrix) {
  min.index <- min_aggregation_with_transparence(imagematrix)
  max.index <- max_aggregation_with_transparence(imagematrix)
  observed <- basic_aggregation_index(imagematrix)
  range <- max.index - min.index
  obs.corrigido <- observed - min.index
  valor_corrigido <- obs.corrigido/range
  return(valor_corrigido)
}
