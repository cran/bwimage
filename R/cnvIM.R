threshold_color <-
  function(filename, filetype = "jpeg", compress_method = "none", compress_rate = 1,
           target_width = 100, target_height = 100, threshold_value = 0.5, transparency_regulation = 0.5,channel="rgb") {
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
    if (threshold_value > 1 | threshold_value < 0) {
      stop("threshold_value must be a number between 0 and 1")
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

    #Definir range das cores que serao utilizadas e maximo da soma
    if(channel=="rgb"|channel=="RGB"){range<-1:3;max.soma<-3
    }else{
      if(channel=="r"|channel=="R"){range<-1;max.soma<-1
      }else{
        if(channel=="g"|channel=="G"){range<-2;max.soma<-1
        }else{
          if(channel=="b"|channel=="B"){range<-3;max.soma<-1
          }else{ stop("Provide a valid channel")}}}}
    # Cria uma matriz de responta e pinta as celulas
    matrix_cores <- matrix(NA, nrow = linhas_imagem, ncol = colunas_imagem)
    if (filetype == "jpeg" | filetype == "jpg") {
      for (c in 1:ncol(matrix_cores)) {
        for (l in 1:nrow(matrix_cores)) {
          if ((sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c *espaco_entre_pixeis_coluna),range])/max.soma) < threshold_value ) {
            matrix_cores[l, c] <- 1
          } else {
            matrix_cores[l, c] <- 0}}}}
    if (filetype == "png") {
      for (c in 1:ncol(matrix_cores)) {
        for (l in 1:nrow(matrix_cores)) {
          if (imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c * espaco_entre_pixeis_coluna),4] < transparency_regulation) {matrix_cores[l, c] <- NA
          } else {
            # deixando as caixas
            if ((sum(imagematrix[floor(l * espaco_entre_pixeis_linha), floor(c *espaco_entre_pixeis_coluna),range])/max.soma)<threshold_value) {matrix_cores[l, c] <- 1
            } else {
              matrix_cores[l, c] <- 0}}}}}
    return(matrix_cores)}


threshold_image_list <-
  function(list_names, filetype = "jpeg", compress_method = "none", compress_rate = 1,
           target_width = 100, target_height = 100, threshold_value = 0.5, transparency_regulation = 0.5,channel="rgb") {
    pb <- txtProgressBar(min = 0, max = length(list_names), style = 3)
    lista_de_saida <- list(NA)
    for (i in 1:length(list_names)) {
      matrix_para_analise <- threshold_color(filename = list_names[i], filetype = filetype, compress_method = compress_method,
                                             compress_rate = compress_rate, threshold_value = threshold_value, target_width = target_width,
                                             target_height = target_height, transparency_regulation = transparency_regulation,channel=channel)
      lista_de_saida[[i]] <- matrix_para_analise
      setTxtProgressBar(pb, i)
    }
    close(pb)
    resposta <- lista_de_saida
    return(resposta)}

stretch<-function(imagematrix,method="radial"){
  if (!(method == "radial" | method == "shirley" | method == "squircle" | method == "elliptical")) {
    stop("Provide a valid stretch method")}
  matrix_resposta<-matrix(1,ncol=length(imagematrix[1,]),nrow=length(imagematrix[,1]))
  altura<-floor(length(imagematrix[,1])/2) # Altura da imagem dividido por 2
  largura<-floor(length(imagematrix[1,])/2) # Largura da imagem dividido por 2
  pb <- txtProgressBar(min = 0, max = length(imagematrix[,1]), style = 3)
  for(l in 1: length(imagematrix[,1])){ # linhas = y
    for(c in 1: length(imagematrix[1,])){ # colunas = x
      co1<-correcao_ida(l,c,altura,largura) # Coordenada do pixel a ser pintado em escala -1:1
      if (method == "radial") {
        # funcao radial
        co2<-radial(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
      }else{
        if (method == "shirley"){
          # funcao shirley
          co2<-shirley(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
        }else{
          if (method == "squircle") {
            # funcao squircle
            co2<-squircle(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
          }else{
            if (method == "elliptical") {
              # funcao elliptical
              co2<-elliptical(co1[1],co1[2],altura,largura)  # Coordenada do pixel que vai ter a cor copida na escala -1:1
            }else{stop("Provide a valid stretch method")}}}}
      co3<-correcao_volta(co2[1],co2[2],altura,largura) # Coordenada do pixel que vai ter a cor copida sem escala
      matrix_resposta[l,c]<- imagematrix[floor(co3[1]),floor(co3[2])]
      co1<-co2<-co3<-NULL}
    setTxtProgressBar(pb, l)}
  return(matrix_resposta)}

compress<-function(imagematrix,method="radial",background=NA){
  if (!(method == "radial" | method == "shirley" | method == "squircle" | method == "elliptical")) {
    stop("Provide a valid compress method")}
  matrix_resposta<-matrix(background,ncol=length(imagematrix[1,]),nrow=length(imagematrix[,1]))
  altura<-floor(length(imagematrix[,1])/2) # Altura da imagem dividido por 2
  largura<-floor(length(imagematrix[1,])/2) # Largura da imagem dividido por 2
  pb <- txtProgressBar(min = 0, max = length(imagematrix[,1]), style = 3)
  for(l in 1: length(imagematrix[,1])){ # linhas = y
    for(c in 1: length(imagematrix[1,])){ # colunas = x
      co1<-correcao_ida(l,c,altura,largura) # Coordenada do pixel a ser pintado em escala -1:1
      if (method == "radial") {
        # funcao radial
        co2<-cradial(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
      }else{
        if (method == "shirley"){
          # funcao shirley
          co2<-cshirley(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
        }else{
          if (method == "squircle") {
            # funcao squircle
            co2<-csquircle(co1[1],co1[2],altura,largura) # Coordenada do pixel que vai ter a cor copida na escala -1:1
          }else{
            if (method == "elliptical") {
              # funcao elliptical
              co2<-celliptical(co1[1],co1[2],altura,largura)  # Coordenada do pixel que vai ter a cor copida na escala -1:1
            }else{
              stop("Provide a valid compress method")
              }}}}
      if(!is.na(co2[1])){
        # primeiro testa de e NA
        if(altura>abs(co2[1])&largura>abs(co2[2])) { # Testar se a coordenda e valida
          co3<-correcao_volta(trunc(co2[1]),trunc(co2[2]) ,altura,largura) # Coordenada do pixel que vai ter a cor copida sem escala
          matrix_resposta[l,c]<- imagematrix[co3[1],co3[2] ]}}
      co1<-co2<-co3<-NULL}
    setTxtProgressBar(pb, l)}
  return(matrix_resposta)}