#' Value at Risk
#'
#' Calcula o Valor em Risco dado um vetor de rendimentos e um quantil. O VaR é
#' equivalente à melhor taxa de retorno dentre os \code{alpha} piores cenários futuros. A
#' expectativa é que \code{1-alpha} retornos futuros superem o VaR.
#'
#' O valor do quantil é equivalente a 1 - Nível de Confiança.
#'
#' @param rend Vetor de rendimentos.
#' @param alpha Quantil do VaR (default = 0.05).
#'
#' @return Valor em Risco.
#' @export
#'
#' @examples
#'
#' require(pracma)
#'
#' set.seed(1)
#'
#' y <- rnorm(100,0.08,0.30)
#'
#' alfa <- 0.1
#'
#' print(VaRfunc(y,alfa))

VaRfunc <- function(rend,alpha = 0.05){
  # fdp
  dens <- density(rend)
  fdp <- approxfun(dens$x,dens$y,yleft = 0,yright = 0)

  LI <- min(dens$x)
  LS <- max(dens$x)

  # otimização
  area <- function(x) integral(fdp,LI,x)

  fobj <- function(x) abs(area(x) - alpha)

  ans <- optimize(fobj,interval = c(LI,LS),tol = 1e-8)

  out <- ans[[1]]

  out
}
