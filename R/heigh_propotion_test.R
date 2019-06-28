heigh_propotion_test <-
function(imagematrix, proportion, height_size) {
  if (is.na(height_size) | height_size <= 0) {
    stop("height_size must be a numeric value grater than zero")
  }
  if (is.na(proportion) | proportion < 0 | proportion > 1) {
    stop("You must set a proportion value ranging from 0-1 to be tested")
  }
  total_acumulado <- heigh_propotion(imagematrix)
  altura <- sum(proportion > round(total_acumulado, 5)) + 1  # Como e um acomulado, quando voce obseva o primeiro 1 isso quer dizer que ali estava a ultima celula com dado, e foi la que foi a partir daquela linha que foi observado esse valor
  altura <- altura * (height_size/length(total_acumulado))  # colocar na escala

  names(altura) <- c(paste("Height below whitch", proportion, "of the vegetation denseness is located"))
  return(altura)
}
