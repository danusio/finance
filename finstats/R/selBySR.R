#' Seleção de Ativos Pela Relação Risco-Retorno
#'
#' Ordena os ativos cujos retornos são passados para a função por ordem decrescente da razão
#' RETORNO/RISCO.
#'
#' @param rend Matriz em que cada coluna representa as rentabilidades de um ativo individual.
#' As colunas devem ser nomeadas.
#' @param num_acoes Quantas ações devem constar na carteira?
#' @param param \code{list} contendo os seguintes campos:
#'
#' 1. nº da coluna com rendimentos do mercado;
#'
#' 2. taxa livre de risco para o lag sobre o qual foram calculados os retorno.
#'
#' @return Vetor com os \code{num_acoes} tickers (nomes dos ativos) com maior razão
#' RETORNO/RISCO, além do ticker equivalente ao Mercado.
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
#' print(selBySR(mat_rend,6,list(1,0.05)))

selBySR <- function(rend,num_acoes,param){
  # rend: matriz com os rendimentos de cada componente da carteira, nomeada
  # num_acoes: nº de ações para a carteira
  # param[[1]]: nº da coluna com os rendimentos do mercado
  # param[[2]]: taxa livre de risco
  # value: tickers selecionados por ordem de maiores SR

  rf <- param[[2]]
  rend1 <- rend[,-param[[1]]]

  tickers <- colnames(rend1)
  SR <- rend1 %>%
    apply(2,sharpe,rf = rf)

  tickers_sel <- tickers[SR %>% order(decreasing = T)]
  tickers_sel <- c(colnames(rend)[param[[1]]],
                   tickers_sel[1:num_acoes])

  tickers_sel
}
