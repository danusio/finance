#' Otimização de Carteiras via Markowitz
#'
#' Encontra as proporções ótimas dos ativos na composição de uma carteira de investimentos,
#' usando as médias históricas de retorno e a covariância entre os retornos dos ativos.
#'
#' @param rend Matriz com os rendimentos do mercado e de cada componente da carteira, nomeada.
#' O vetor com rendimentos do mercado não entra no cálculo da otimização, mas é necessário para
#' manter a padronização de cabeçalho entre as funções de otimização de carteira presentes na
#' biblioteca.
#' @param param \code{list} com os seguintes campos:
#'
#' 1: nº da coluna com rendimentos do mercado
#'
#' @return Vetor com as proporções de cada ativo na carteira ótima. A proporção do ativo
#' correspondente ao mercado será 0.
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
#' wopt <- markowitz(mat_rend,list(1))
#'
#' # valores em %
#' print(round(100*wopt,3))

markowitz <- function(rend,param){
  # rend: matriz com os rendimentos de cada componente da carteira, nomeada
  # param[[1]]: nº da coluna com rendimentos do mercado
  # value: composição da carteira ótima

  rend1 <- rend[,-param[[1]]]

  Er <- colMeans(rend1)
  mat_cov <- cov(rend1)

  fobj <- function(w){
    if (sum(w<0)>0) {
      return(-1e16*sum(w<0))
    }

    w <- w/sum(w)
    Er_med <- sum(w*Er)
    S <- (matrix(w,nrow = 1) %*% mat_cov %*% matrix(w,ncol = 1)) %>%
      sqrt

    return(Er_med/S)
  }

  ans <- optim(rep(1,ncol(rend1)),
               fobj,
               control = list(fnscale=-1,
                              maxit = 1e4))

  w_opt <- ans$par/sum(ans$par)
  w_opt <- c(0,w_opt)
  names(w_opt) <- c(colnames(rend)[param[[1]]],
                    colnames(rend1))

  w_opt
}
