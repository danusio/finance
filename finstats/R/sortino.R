#' Sortino Ratio
#'
#' Calcula o índice de Sortino de uma série de retornos. A principal diferença entre este e o
#' Índice de Sharpe é que Sortino considera somente o desvio padrão dos retornos abaixo da
#' Taxa Livre de Risco (i.e., não "pune" o investimento pelo risco benéfico).
#'
#' @param x Vetor com os rendimentos.
#' @param rf Taxa livre de risco relativa ao período dos rendimentos. Se os retornos foram
#' calculados com um lag de 120 dias, a rf deverá ser relativa a um período de 120 dias.
#'
#' @return Sortino Ratio dos retornos presentes em \code{x}.
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' rend <- rnorm(300,0.08,0.15)
#' print(sortino(rend,0.03))

sortino <- function(x,rf=0){
  mi <- mean(x,na.rm = T)
  sig <- sd(x[x<rf])

  return((mi - rf)/sig)
}
