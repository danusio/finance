#' Otimização de Carteiras via Modelo de Fator Único
#'
#' Encontra as proporções ótimas dos ativos na composição de uma carteira de investimentos,
#' usando os coeficientes "beta" dos ativos e seus riscos específico e sistemático. Considera a
#' possibilidade de posições compradas e vendidas e alavancagem.
#'
#' @param rend Matriz com os rendimentos do mercado e de cada componente da carteira, nomeada.
#' @param param \code{list} com os seguintes campos:
#'
#' 1: nº da coluna com rendimentos do mercado
#'
#' 2: máxima alavancagem. Se |alavancagem| <= 1, não há operações short
#'
#' @return Vetor com as proporções de cada ativo na carteira ótima. Proporções negativas
#' indicam posições vendidas.
#' @export
#'
#' @examples
#'
#' require(magrittr)
#'
#' set.seed(1)
#'
#' # vetor com rendimento do mercado
#' mkt <- rnorm(100,0.05,0.13)
#'
#' # otimização com 4 ativos
#' stk1 <- rnorm(100,0.08,0.2)
#' stk2 <- rnorm(100,-0.03,0.05)
#' stk3 <- rnorm(100,0.13,0.45)
#' stk4 <- rnorm(100,0.02,0.1)
#'
#' # matriz de rendimentos
#' mat_rend <- cbind(mkt,stk1,stk2,stk3,stk4)
#'
#' # otimização: mercado na coluna 1
#' wopt <- sfm(mat_rend,list(1,2))
#'
#' # valores em %
#' print(round(100*wopt,3))

sfm <- function(rend,param){
  # rend: matriz com os rendimentos de cada componente da carteira, nomeada
  # param[[1]]: nº da coluna com rendimentos do mercado
  # param[[2]]: máxima alavancagem. Se |'max.lever'| <= 1, não há operações short
  # value: composição da carteira ótima

  bench <- rend[,param[[1]]]
  rend1 <- rend[,-param[[1]]]

  num_acoes <- ncol(rend1)
  max.lever <- param[[2]]

  # 1. Regressão Linear com o mercado ----
  sigma2m <- var(bench)
  Rm <- mean(bench)

  Beta <- alfa <- NULL
  for (i in 1:num_acoes) {
    y <- rend1[,i]
    betai <- cov(bench,y)/sigma2m

    Beta <- c(Beta,
              betai)
    alfa <- c(alfa,
              mean(y) - betai*Rm)
  }

  # 2. Pesos da carteira de ativos ----
  # riscos sistemáticos
  sigma2 <- rend1 %>% apply(2,var)

  # riscos específicos
  sigma2e <- sigma2 - (Beta^2)*sigma2m

  w0 <- alfa/sigma2e
  w <- w0/sum(w0)

  # 3. Pesos globais ----
  alfaA <- sum(w*alfa)
  sigma2eA <- sum((w^2)*sigma2e)
  BetaA <- sum(w*Beta)

  w0A <- (alfaA/sigma2eA)/(Rm/sigma2m)

  wA <- w0A/(1 + (1-BetaA)*w0A)
  # limitação da alavancagem
  wA <- ifelse(wA>=0,
               min(wA,max.lever),
               max(wA,1-max.lever))

  w_opt <- c(1 - wA, # peso do benchmark
             wA*w) # pesos dos ativos
  names(w_opt) <- c(colnames(rend)[param[[1]]],colnames(rend1))

  w_opt
}
