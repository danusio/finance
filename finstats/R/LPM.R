#' Lower Partial Moments
#'
#' Momentos (estatísticos centrais) Parciais Inferiores. LPM são generalizações dos momentos
#' estatísticos centrais que substituem a média pela Taxa Livre de Risco (\code{rf}), no
#' cômputo dos desvios, e a variável pelos elementos da variável que são menores que \code{rf}.
#'
#' O primeiro momento parcial inferior é o equivalente ao valor zero do 1º Momento Central de
#' um conjunto de dados. O segundo momento parcial inferior é equivalente à variância.
#'
#' @param rend Vetor de rendimentos.
#' @param rf Taxa Livre de Riscos, relativa ao período sobre o qual foram calculados os
#' rendimentos.
#' @param ord Ordem do momento central.
#'
#' @return Momento Parcial Inferior.
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' y <- rnorm(100,0.08,0.30)
#' rf <- 0.05
#'
#' # 1º LPM
#' print(LPM(y,rf,1))
#'
#' # 2º LPM
#' print(LPM(y,rf,2))
#' # Variância
#' print(var(y))
#'
#' # 3º LPM
#' print(LPM(y,rf,3))
#' # 3º Momento Central
#' print(sum((y-mean(y))^3)/(length(y-1)))
#'
#' # 4º LPM
#' print(LPM(y,rf,4))
#' # 4º Momento Central
#' print(sum((y-mean(y))^4)/(length(y-1)))

LPM <- function(rend,rf,ord){
  if (ord <= 0) {
    moment <- sum((rend[rend<rf]-rf)^ord)/sum(rend<rf)
  }else{
    moment <- sum((rend[rend<rf]-rf)^ord)/(sum(rend<rf)-1)
  }
  moment
}
