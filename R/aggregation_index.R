aggregation_index <-
function(imagematrix) {
  if (length(subset(as.vector(imagematrix), is.na(as.vector(imagematrix)))) > 1) {adjusted_aggregation <- adjusted_aggregation_with_transparence(imagematrix)
    non_adjusted_aggregation <- basic_aggregation_index(imagematrix)
  } else {
    adjusted_aggregation <- adjusted_aggregation_wo_transparence(imagematrix)
    non_adjusted_aggregation <- basic_aggregation_index(imagematrix)}

  resposta <- c(adjusted_aggregation, non_adjusted_aggregation)
  names(resposta) <- c("adjusted_aggregation", "non_adjusted_aggregation")
  return(resposta)}
