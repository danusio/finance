#' Seleção de Ativos e Otimização de Carteira
#'
#' Dada uma função de otimização de carteira e uma função de seleção de ativos, retorna a
#' composição da carteira ótima em uma matriz de rendimentos. Simplifica a aplicação de
#' metodologias de formação de portfólios.
#'
#' A função de otimização deve ter 2 argumentos: a matriz de rendimentos e uma lista de
#' parâmetros (veja as funções \code{markowitz} e \code{sfm}).
#'
#' A função de seleção de ativos deve ter 3 argumentos: a matriz de rendimentos, o número de
#' ações requeridas na carteira e uma lista de parâmetros (veja as funções \code{selBySR} e
#' \code{selByDelta}).
#'
#' @param rend Matriz em que cada coluna representa as rentabilidades de um ativo individual.
#' As colunas devem ser nomeadas.
#' @param num_acoes Número de ativos que devem constar na carteira.
#' @param fun.otim Função de otimização de carteira.
#' @param fun.sel Função de seleção de ativos.
#' @param param.otim Parâmetros da função de otimização.
#' @param param.sel Parâmetros da função de seleção.
#'
#' @return \code{list} com os seguintes campos:
#'
#' 1. tickers: nomes dos ativos selecionados;
#'
#' 2. w: porcentagem de cada ativo selecionado na carteira ótima.
#'
#' @export
#'
#' @examples
#'
#' require(magrittr)
#'
#' # matriz de rendimentos
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
#' # aplicação
#' result <- optimSel(mat_rend,5,
#'                    markowitz,selBySR,
#'                    list(1),list(1,0.05))
#' print(result)

optimSel <- function(rend,num_acoes,
                     fun.otim,fun.sel,
                     param.otim,param.sel){
  # rend: matriz com os rendimentos de cada componente da carteira, nomeada
  # num_acoes: nº de ações para a carteira
  # fun.otim: função de otimização
  # fun.sel: função de seleção dos ativos
  # param.otim: parâmetros da função de otimização
  # param.sel: parâmetros da função de seleção dos ativos
  # value: lista contendo:
  ## - tickers selecionados
  ## - pesos de cada ticker

  # 1. Seleção de Ativos ----
  tickers_sel <- do.call(fun.sel,
                         list(rend=rend,
                              num_acoes=num_acoes,
                              param=param.sel))

  # 2. Otimização da Carteira
  w <- do.call(fun.otim,
               list(rend=rend[,tickers_sel],
                    param=param.otim))

  return(list(tickers=tickers_sel,
              w=w))
}
