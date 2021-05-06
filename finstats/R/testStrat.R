#' Testes t (médias) e F (variâncias) Entre Retornos
#'
#'  Avalia a relação (maior, menor ou igual) entre pares de rentabilidades, em termos de média
#'  dos retornos (associada à esperança de retorno futura) e variância dos retornos (associada
#'  ao risco da estratégia).
#'
#'  Os teste são realizados com nível de confiança de 95%.
#'
#' @param rend Matriz em que cada coluna representa as rentabilidades de uma estratégia ou
#' ativo individual. As colunas devem ser nomeadas.
#'
#' @return Lista contendo os seguintes campos:
#'
#' 1. relações entre os retornos;
#'
#' 2. relações entre as variâncias.
#'
#' Os elementos dessa matriz são os termos "greater", "less" e "equal", indicando se a primeira
#' medida testada é maior que, menor que ou igual à, em termos de média e variância, segunda.
#'
#' Sendo 'n' o número de colunas de \code{rend}, os dois elemento dessa lista são matrizes
#' triangulares (n-1) x (n-1). A ordem de leitura é, para o elemento \eqn{[i,j]} da matriz de
#' saída M: "retorno/volatilidade da estratégia i é maior que/menor que/igual a (de acordo com
#' \eqn{M[i,j]}) retorno/volatilidade da estratégia j".
#'
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' mat_rend <- cbind(estrat1 = rnorm(300,0.08,0.2),
#'                   estrat2 = rnorm(300,0.08,0.4),
#'                   estrat3 = rnorm(300,0.04,0.2))
#'
#' print(testStrat(mat_rend))

testStrat <- function(rend){
  # rend: matriz com os rendimentos das estratégias a serem avaliadas
  # value: lista de matrizes com os valores 'greater', 'less' e 'equal'
  ## [[1]] - relações entre os retornos
  ## [[2]] - relações entre as volatilidades

  num_estrat <- ncol(rend)

  mat_t <- mat_F <- matrix(NA,
                           ncol = num_estrat,
                           nrow = num_estrat)
  for (i in 1:(num_estrat-1)) {
    for (j in (i+1):num_estrat) {
      t_g <- t.test(rend[,i],rend[,j],alternative = "greater")
      t_l <- t.test(rend[,i],rend[,j],alternative = "less")

      mat_t[i,j] <- ifelse(t_g$p.value<0.05,"greater",
                           ifelse(t_l$p.value<0.05,"less",
                                  "equal"))

      F_g <- var.test(rend[,i],rend[,j],alternative = "greater")
      F_l <- var.test(rend[,i],rend[,j],alternative = "less")

      mat_F[i,j] <- ifelse(F_g$p.value<0.05,"greater",
                           ifelse(F_l$p.value<0.05,"less",
                                  "equal"))

    }
  }

  colnames(rend) ->
    colnames(mat_t) -> colnames(mat_F) ->
    rownames(mat_t) -> rownames(mat_F)

  return(list(retornos = mat_t[-num_estrat,-1],
              volatilidade = mat_F[-num_estrat,-1]))
}
