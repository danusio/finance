#' Expected Shortfall
#'
#' Calcula o Déficit Esperado dado um vetor de rendimentos e um quantil. Enquanto o Value at
#' Risk (VaR) representa o retorno equivalente a um quantil \code{alpha}, o ES é a área sob a
#' curva de densidade de probabilidade dos retornos entre os quantis 0 e \code{alpha}.
#'
#' O VaR apresenta o melhor retorno entre os piores cenários possíveis e o ES, o valor esperado
#' (soma ponderada pelas probabilidades de ocorrência) de todos os piores cenários possíveis.
#'
#' @param rend Vetor de rendimentos.
#' @param alpha Quantil do VaR (default = 0.05).
#'
#' @return Déficit Esperado
#' @export
#'
#' @seealso \code{\link{VaRfunc}}
#'
#' @examples
#'
#' require(pracma)
#' require(magrittr)
#' require(DescTools)
#'
#' set.seed(1)
#'
#' y <- rnorm(100,0.08,0.30)
#'
#' alfa <- 0.1
#'
#' print(expShortfall(y,alfa))

expShortfall <- function(rend,alpha=0.05){
  func <- function(x) VaRfunc(rend,x)

  alfa_x <- seq(0,alpha,length.out = 100)
  VaR_y <- alfa_x %>% sapply(func,simplify = T)

  ES <- (alfa_x %>% AUC(VaR_y))/alpha

  ES
}
