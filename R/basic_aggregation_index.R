basic_aggregation_index <-
function(imagematrix) {
  matrix_agregacao <- aggregation_matrix(imagematrix)
  values <- subset(as.vector(matrix_agregacao), !is.na(as.vector(matrix_agregacao)))  # Jogando todos os dados para um unico objeto
  average_aggregation <- mean(values)
  return(average_aggregation)}
