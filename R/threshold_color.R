threshold_color <-
function(filename, filetype = "jpeg", compress_method = "none", compress_rate = 1,
                            target_width = 100, target_height = 100, black_regulation = 0.5, transparency_regulation = 0.5) {
  # Validando os dados
  if (filetype == "png"|filetype == "PNG") {
    requireNamespace("png")
  }
  if (filetype == "jpeg"|filetype =="jpg"|filetype =="JPEG"|filetype =="JPG") {
    requireNamespace("jpeg")
  }
  if (!(filetype == "png"|filetype == "PNG") & !(filetype == "jpeg"|filetype =="jpg"|filetype =="JPEG"|filetype =="JPG")) {
    stop("Provide a valid file type - i.e. png or jpeg")
  }
  if (compress_rate <= 0 | compress_rate > 1) {
    stop("compress_rate must be a number between 0 and 1 ")
  }
  if (black_regulation >= 1 | black_regulation < 0) {
    stop("black_regulation must be a number between 0 and 1")
  }
  if (!(compress_method == "none" | compress_method == "frame_fixed" | compress_method == "proportional" |
        compress_method == "width_fixed" | compress_method == "height_fixed")) {
    stop("Provide a valid reduction method")
  }

  if (filetype == "jpeg"|filetype == "jpg"|filetype == "JPEG"|filetype == "JPG") {
    imagematrix <- readJPEG(filename)
  }
  if (filetype == "png"|filetype == "PNG") {
    imagematrix <- readPNG(filename)
  }
  # Calcular o fator de correcao para a figura produzida
  if (compress_method == "none") {
    espaco_entre_pixeis_linha <- 1
    espaco_entre_pixeis_coluna <- 1
    linhas_imagem <- nrow(imagematrix)
    colunas_imagem <- ncol(imagematrix)
  } else {
    if (compress_method == "frame_fixed") {
      linhas_imagem <- target_height  # Quantas linhas vao ter na nova figura
      colunas_imagem <- target_width  # Quantas colunas vao ter na nova figura
      espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
      espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
    } else {
      if (compress_method == "proportional") {
        linhas_imagem <- floor(nrow(imagematrix) * compress_rate)  # Quantas linhas vao ter na nova figura
        colunas_imagem <- floor(ncol(imagematrix) * compress_rate)  # Quantas colunas vao ter na nova figura
        espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
        espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
      } else {
        if (compress_method == "width_fixed") {
          colunas_imagem <- target_width
          linhas_imagem <- floor((nrow(imagematrix) * target_width)/ncol(imagematrix))
          espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
          espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal

        } else {
          if (compress_method == "height_fixed") {
            linhas_imagem <- target_height  # Quantas linhas vao ter na nova figura
            colunas_imagem <- floor((ncol(imagematrix) * target_height)/nrow(imagematrix))  # Quantas colunas vao ter na nova figura
            espaco_entre_pixeis_linha <- nrow(imagematrix)/linhas_imagem  # espaco entre os pixeis das linhas na figura orginal
            espaco_entre_pixeis_coluna <- ncol(imagematrix)/colunas_imagem  # espaco entre os pixeis das colunas na figura orginal
          }}}}}
  matrix_cores <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
if (filetype == "jpeg" | filetype == "jpg") {
    for (c in 1:ncol(matrix_cores)) {
      for (l in 1:nrow(matrix_cores)) {
        if (sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                            1:3]) < ((black_regulation * 1.5) + 1.5)) {
          matrix_cores[l, c] <- 1
        } else {
          matrix_cores[l, c] <- 0}}}}
  if (filetype == "png") {
    for (c in 1:ncol(matrix_cores)) {
      for (l in 1:nrow(matrix_cores)) {
        if (imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                        4] < transparency_regulation) {
          matrix_cores[l, c] <- NA
        } else {
          # deixando as caixas
          if (sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),
                              1:3]) < ((black_regulation * 1.5) + 1.5)) {
            matrix_cores[l, c] <- 1
          } else {
            matrix_cores[l, c] <- 0}}}}}
  return(matrix_cores)}
