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

#' @title Summary of image information
#'
#' @description Provide the information of: number of black, white and transparent pixels, total number of pixels, height and width size.
#' @param imagematrix The matrix to be analysed.
#' @return
#' \item{Black}{Number of black pixels}
#' \item{White}{Number of white pixels}
#' \item{Transparent}{Number of transparent pixels}
#' \item{Total}{Total number of pixels}
#' \item{Height}{Size in height}
#' \item{Width}{Size in width}
#' @author Carlos Biagolini-Jr.
#' @seealso threshold_color
#' @examples
#' # First, get a matrix from your image. Here an example of a bush image is used.
#' bush<-system.file("extdata/bush.JPG",package ="bwimage")
#' bush_imagematrix<-threshold_color(bush,  "jpeg", "proportional",compress_rate = 0.1)
#' image_information(bush_imagematrix)
#' @export
image_information <-
  function(imagematrix) {
    resultado_em_processo <- NULL
    linhas_imagem <- nrow(imagematrix)
    colunas_imagem <- ncol(imagematrix)
    dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    n_black_pixels <- sum(dados_pixeis)
    n_pixels <- length(dados_pixeis)
    n_white_pixels <- n_pixels - n_black_pixels
    n_transparent <- (linhas_imagem * colunas_imagem) - n_pixels
    width_BW_picture <- nrow(imagematrix)
    height_BW_picture <- ncol(imagematrix)
    resultado_em_processo[1] <- n_black_pixels
    resultado_em_processo[2] <- n_white_pixels
    resultado_em_processo[3] <- n_transparent
    resultado_em_processo[4] <- n_pixels
    resultado_em_processo[5] <- height_BW_picture
    resultado_em_processo[6] <- width_BW_picture

    names(resultado_em_processo) <- c("Black", "White", "Transparent", "Total","Height", "Width")
    return(resultado_em_processo)}

denseness_total <-
  function(imagematrix) {
    dados_pixeis <- subset(as.vector(imagematrix), !is.na(as.vector(imagematrix)))
    n_black_pixels <- sum(dados_pixeis)
    n_pixels <- length(dados_pixeis)
    p_black_pixels <- n_black_pixels/n_pixels
    return(p_black_pixels)}

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

hole_section <-
  function(section) {
    sequencia_de_modificacao <- diff(section)
    nao_nas_original <- which(!is.na(sequencia_de_modificacao))

    comecou <- section[which(!is.na(section))[1]]
    if (sum(sequencia_de_modificacao[nao_nas_original]^2) == 0) {
      matrix_secoes <- matrix(c(min(which(!is.na(section) == T)), max(which(!is.na(section) == T),
                                                                      2), 0, comecou), ncol = 4, nrow = 1)
      colnames(matrix_secoes) <- c("Start", "End", "Size", "Color")
    } else {
      transicoes_para_preto <- which(sequencia_de_modificacao == 1)
      transicoes_para_branco <- which(sequencia_de_modificacao == -1)
      transicoes <- sort(c(transicoes_para_preto, transicoes_para_branco))
      n_seccoes <- length(transicoes) + 1
      matrix_secoes <- matrix(NA, ncol = 4, nrow = n_seccoes)
      colnames(matrix_secoes) <- c("Start", "End", "size", "Color")

      # qual e a cor do primeiro nao NA
      matrix_secoes[1, 4] <- comecou
      inicios <- c(which(!is.na(section))[1], transicoes)
      matrix_secoes[, 1] <- inicios

      for (i in 2:length(matrix_secoes[, 1])) {
        if (comecou == 0) {
          comecou <- 1
        } else {
          comecou <- 0
        }
        matrix_secoes[i, 4] <- comecou
      }

      for (i in 1:(length(matrix_secoes[, 1]) - 1)) {
        valor <- matrix_secoes[i + 1, 1]
        matrix_secoes[i, 2] <- valor
      }

      matrix_secoes[length(matrix_secoes[, 1]), 2] <- max(which(!is.na(section) == T))
    }

    matrix_secoes[, 3] <- matrix_secoes[, 2] - matrix_secoes[, 1] + 1
    return(matrix_secoes)
  }

hole_section_data <-
  function(section, color = 0) {
    matrix_burcaos <- hole_section(section)
    matrix_burcaos_limpa <- subset(matrix_burcaos, matrix_burcaos[, 4] == color)
    if (is.na(matrix_burcaos_limpa[1])) {
      numero <- 0
      media <- 0
      desvio <- NA
      minimo <- 0
      maximo <- 0
    } else {
      numero <- length(matrix_burcaos_limpa[, 3])
      media <- mean(matrix_burcaos_limpa[, 3])
      desvio <- sd(matrix_burcaos_limpa[, 3])
      minimo <- min(matrix_burcaos_limpa[, 3])
      maximo <- max(matrix_burcaos_limpa[, 3])
    }
    resposta <- c(numero, media, desvio, minimo, maximo)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max")
    return(resposta)
  }

hole_columm <-
  function(imagematrix, color = 0, n_sections = "all") {
    numeros <- NULL
    medias <- NULL
    desvios <- NULL
    minimos <- NULL
    maximos <- NULL
    if (n_sections == "all") {
      sections <- 1:length(imagematrix[1, ])
      n_sections <- length(imagematrix[1, ])
    } else {
      intervalos <- floor(ncol(imagematrix)/n_sections)
      sections <- seq(from = 1, to = length(imagematrix[1, ]), by = intervalos)
    }

    for (i in 1:n_sections) {
      aup <- sections[i]
      aux <- hole_section_data(imagematrix[, aup], color = color)
      numeros[i] <- aux[1]
      medias[i] <- aux[2]
      desvios[i] <- aux[3]
      minimos[i] <- aux[4]
      maximos[i] <- aux[5]
    }
    maiores_estratos <- which(numeros == max(numeros[!is.na(numeros)]))
    resposta <- list(numeros, medias, desvios, minimos, maximos, maiores_estratos)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max", "LH")
    return(resposta)
  }

hole_row <-
  function(imagematrix, color = 0, n_sections = "all") {
    numeros <- NULL
    medias <- NULL
    desvios <- NULL
    minimos <- NULL
    maximos <- NULL

    if (n_sections == "all") {
      sections <- 1:length(imagematrix[, 1])
      n_sections <- length(imagematrix[, 1])
    } else {
      intervalos <- floor(nrow(imagematrix)/n_sections)
      sections <- seq(from = 1, to = length(imagematrix[, 1]), by = intervalos)
    }

    for (i in 1:n_sections) {
      aup <- sections[i]
      aux <- hole_section_data(imagematrix[aup, ], color = color)
      numeros[i] <- aux[1]
      medias[i] <- aux[2]
      desvios[i] <- aux[3]
      minimos[i] <- aux[4]
      maximos[i] <- aux[5]
    }
    maiores_estratos <- which(numeros == max(numeros[!is.na(numeros)]))
    resposta <- list(numeros, medias, desvios, minimos, maximos, maiores_estratos)
    names(resposta) <- c("N", "Mean", "SD", "Min","Max", "LH")
    return(resposta)
  }

light_gap <-
  function(imagematrix, width_size = NA, scale = TRUE) {
    if (scale == T & (is.na(width_size) | width_size <= 0)) {
      stop("width_size not found")
    } else {
      correcao <- width_size/length(imagematrix[1, ])
    }
    primeira <- 0
    ultima <- 0
    teste <- 0

    # Encontrar primeira coluna com dados
    coluna_rodada <- 1
    while (teste == 0) {
      coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,                                            coluna_rodada])))
      aux_test <- sum(coluna_dados)
      if (aux_test > 0) {
        teste <- 1
        primeira <- coluna_rodada
      } else {
        coluna_rodada <- coluna_rodada + 1}}

    # Encontrar ultima coluna com dados
    coluna_rodada <- length(imagematrix[1, ])
    teste <- 0
    while (teste == 0) {
      coluna_dados <- subset(as.vector(imagematrix[, coluna_rodada]), !is.na(as.vector(imagematrix[,
                                                                                                   coluna_rodada])))
      aux_test <- sum(coluna_dados)
      if (aux_test > 0) {
        teste <- 1
        ultima <- coluna_rodada
      } else {
        coluna_rodada <- coluna_rodada - 1
      }
    }
    primeira <- primeira - 1
    ultima <- length(imagematrix[1, ]) - ultima

    if (scale == T) {
      primeira <- primeira * correcao
      ultima <- ultima * correcao
    }

    resposta <- c(primeira, ultima)
    names(resposta) <- c("Left gap size", "Right gap size")
    return(resposta)
  }

heigh_maximum <-
  function(imagematrix, height_size) {
    if (height_size <= 0) {
      stop("height_size must be greater than zero")
    }
    # Criar funcao para achar a maior coluna
    alt_max <- function(coluna) {
      if (sum(coluna == 1) > 0) {
        resposta <- which(coluna == 1)[1]
      } else {
        resposta <- length(coluna)
      }
      return(resposta)
    }
    alturas <- apply(imagematrix, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
    dist_de_baixo <- length(imagematrix[, 1]) - alturas
    dist_de_baixo[!dist_de_baixo == 0] <- dist_de_baixo[!dist_de_baixo == 0] + 1  # somar 1 para corrigir que apenas as que n?o tem nenhum pixel preto vao ter altura de zero
    maior_coluna <- which(dist_de_baixo == max(dist_de_baixo))[1]  #qual coluna tem o pixel mais alto (se for mais de uma como mesmo maximo pega so a primeira)
    altura_maxima <- dist_de_baixo[maior_coluna]
    altura_maxima <- altura_maxima * (height_size/length(imagematrix[, 1]))

    names(altura_maxima) <- c("Height")
    return(altura_maxima)
  }

altitudinal_profile <-
  function(imagematrix, n_sections, height_size) {
    if ((length(imagematrix[1, ])/2) < n_sections) {
      stop(paste("Sections width must have at least two cells. Please choose a number of n_sections lower or equal to ",
                 length(imagematrix[1, ])/2))
    }

    tamanho_coluna <- floor(ncol(imagematrix)/n_sections)
    dist_de_baixo <- NULL
    intervalos <- seq(from = 1, to = ncol(imagematrix), by = tamanho_coluna)

    for (i in 1:n_sections) {
      sub_amostra <- imagematrix[, intervalos[i]:(intervalos[i] + tamanho_coluna - 1)]
      aux_maximums <- heigh_maximum(sub_amostra, height_size = height_size)
      dist_de_baixo[i] <- aux_maximums
    }

    media <- mean(dist_de_baixo)
    desvio <- sd(dist_de_baixo)

    resposta <- list(media, desvio, dist_de_baixo)
    names(resposta) <- c("Mean", "SD","Size")
    return(resposta)}

heigh_propotion <-
  function(imagematrix) {
    total_row <- NULL
    for (i in 1:length(imagematrix[, 1])) {
      dados_pixeis <- subset(as.vector(imagematrix[i, ]), !is.na(as.vector(imagematrix[i, ])))
      total_row[i] <- sum(dados_pixeis)
    }
    n_pixels <- sum(total_row)
    porporcoes <- total_row/n_pixels
    acumulado <- rep(0, length(porporcoes))
    acumulado[length(porporcoes)] <- porporcoes[length(porporcoes)]

    for (i in seq(from = (length(porporcoes) - 1), to = 1)) {
      acumulado[i] <- porporcoes[i] + acumulado[i + 1]
    }
    return(acumulado)
  }

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

    names(altura) <- c(paste("Height below which", proportion, "of the vegetation denseness is located"))
    return(altura)
  }

topline <-
  function(imagematrix, height_size = NA, width_size = NA) {
    if (is.na(height_size) | height_size <= 0) {
      stop("height_size not found")
    }
    if (is.na(width_size) | width_size <= 0) {
      stop("width_size not found")
    }

    escala_horizontal <- width_size/length(imagematrix[1, ])
    escala_vertical <- height_size/length(imagematrix[, 1])

    # Recortar a parte da imagem com pixeis
    corte <- light_gap(imagematrix, scale = F)
    corte[1] <- corte[1] + 1
    corte[2] <- length(imagematrix[1, ]) - corte[2]
    imagem_trabalho <- imagematrix[, corte[1]:corte[2]]

    # Criar funcao para achar a maior coluna
    alt_max <- function(coluna) {
      if (sum(coluna == 1) > 0) {
        resposta <- which(coluna == 1)[1]
      } else {
        resposta <- length(coluna)
      }
      return(resposta)
    }
    # Perfil de altura
    alturas <- apply(imagem_trabalho, 2, alt_max)  # encontre o primeiro pixel preto mais alto em cada coluna
    dist_de_baixo <- length(imagematrix[, 1]) - alturas + 1

    altura_primeira_coluna <- dist_de_baixo[1]
    altura_ultimaa_coluna <- dist_de_baixo[length(dist_de_baixo)]

    meio <- NULL
    for (i in 1:(length(dist_de_baixo) - 1)) {
      meio[i] <- abs(dist_de_baixo[i] - dist_de_baixo[i + 1])
    }

    total_vertical <- altura_primeira_coluna + altura_ultimaa_coluna + sum(meio)  # quantos pixeis foram caminhados na horizontal
    total_vertical <- total_vertical * escala_vertical  # converter para a escala do estudo

    total_horizontal <- length(dist_de_baixo)
    total_horizontal <- total_horizontal * escala_horizontal  # converter para a escala do estudo

    resposta <- total_horizontal + total_vertical
    names(resposta) <- "topline"
    return(resposta)
  }