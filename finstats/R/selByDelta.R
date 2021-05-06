#' Seleção de Ativos Pelo Delta
#'
#' Delta é o desempenho acima do previsto pela regressão linear dos retornos do ativo com os
#' retornos do Mercado. A diferença entre os retornos real e previsto é tomada no último preço
#' disponível.
#'
#' @param rend Matriz em que cada coluna representa as rentabilidades de um ativo individual.
#' As colunas devem ser nomeadas.
#' @param num_acoes Quantas ações devem constar na carteira?
#' @param param \code{list} contendo os seguintes campos:
#'
#' 1. nº da coluna com rendimentos do mercado.
#'
#' @return Vetor com os \code{num_acoes} tickers (nomes dos ativos) com maior valor de Delta,
#' além do ticker equivalente ao Mercado.
#' @export
#'
#' @examples
#'
#' require(magrittr)
#'
#' set.seed(1)
#'
#' mi <- runif(11,-0.2,0.2)
#' sig <- runif(11,0.3,0.9)
#'
#' mat_rend <- NULL
#' # 10 ativos + Mercado
#' for (i in 1:11){
#'    mat_rend <- cbind(mat_rend,
#'                      rnorm(300,mi,sig))
#' }
#' colnames(mat_rend) <- paste0("acao",1:11)
#'
#' print(selByDelta(mat_rend,6,list(1)))

selByDelta <- function(rend,num_acoes,param){
  # rend: matriz com os rendimentos de cada componente da carteira, nomeada
  # num_acoes: nº de ações para a carteira
  # param[[1]]: nº da coluna com os rendimentos do mercado
  # value: tickers selecionados por ordem de maiores desempenhos

  bench <- rend[,param[[1]]]
  rend1 <- rend[,-param[[1]]]

  tickers <- colnames(rend1)

  x <- bench
  varx <- var(x)
  meanx <- mean(x)

  delta <- NULL
  for (i in 1:ncol(rend1)) {
    y <- rend1[,i]
    # coef de reg lin
    A <- cov(x,y)/varx
    b <- mean(y) - A*meanx
    # valor prev pela reg lin
    y1 <- A*tail(x,1) + b

    delta <- c(delta,
               tail(y,1)/y1)
  }

  tickers_sel <- tickers[delta %>% order(decreasing = T)]
  tickers_sel <- c(colnames(rend)[param[[1]]],
                   tickers_sel[1:num_acoes])

  tickers_sel
}
