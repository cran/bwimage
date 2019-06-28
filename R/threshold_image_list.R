threshold_image_list <-
function(list_names, filetype = "jpeg", compress_method = "none", compress_rate = 1,
                                 target_width = 100, target_height = 100, black_regulation = 0.5, transparency_regulation = 0.5) {
  pb <- txtProgressBar(min = 0, max = length(list_names), style = 3)
  lista_de_saida <- list(NA)
  for (i in 1:length(list_names)) {
    matrix_para_analise <- threshold_color(filename = list_names[i], filetype = filetype, compress_method = compress_method,
                                           compress_rate = compress_rate, black_regulation = black_regulation, target_width = target_width,
                                           target_height = target_height, transparency_regulation = transparency_regulation)
    lista_de_saida[[i]] <- matrix_para_analise
    setTxtProgressBar(pb, i)
  }
  close(pb)
  resposta <- lista_de_saida
  return(resposta)}
