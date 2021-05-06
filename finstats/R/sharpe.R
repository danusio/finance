#' Sharpe Ratio
#'
#' Calcula o índice de Sharpe de uma série de retornos.
#'
#' @param x Vetor com os rendimentos.
#' @param rf Taxa livre de risco relativa ao período dos rendimentos. Se os retornos foram
#' calculados com um lag de 120 dias, a rf deverá ser relativa a um período de 120 dias.
#'
#' @return SR dos retornos presentes em \code{x}.
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' rend <- rnorm(300,0.08,0.15)
#' print(sharpe(rend,0.03))

sharpe <- function(x,rf=0){
  return((mean(x,na.rm = T) - rf)/sd(x,na.rm = T))
}
