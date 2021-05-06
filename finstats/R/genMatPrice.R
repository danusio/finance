#' Matriz de Preços Sincronizados
#'
#' Dada uma lista cujos elementos são data.frames com duas colunas (a primeira
#' contendo as datas dos pregões e a segunda, os preços correspondentes) para cada ativo, é
#' gerada uma matriz de preços em que cada linha corresponde a uma data de pregão e cada
#' coluna, a um ativo.
#'
#' @param prices_list \code{list} com data.frames no formato "data-preço"
#'
#' @return Matriz com os preços associados a cada pregão disponível
#' @export
#'
#' @examples
#'
#' require(quantmod)
#' require(magrittr)
#'
#' precos1 <- getSymbols("petr4.sa",
#'                        from="2020-01-01",to="2020-12-31",
#'                        auto.assign=FALSE)
#' precos2 <- getSymbols("vale3.sa",
#'                        from="2020-01-01",to="2020-12-31",
#'                        auto.assign=FALSE)
#'
#' datas1 <- index(precos1)
#' datas2 <- index(precos2)
#'
#' close1 <- precos1[,4]
#' close2 <- precos2[,4]
#'
#' lista_precos <- list()
#' lista_precos[["PETR4"]] <- data.frame(data = datas1,fechamento = close1)
#' lista_precos[["VALE3"]] <- data.frame(data = datas2,fechamento = close2)
#'
#' mat_precos <- genMatPrice(lista_precos)

genMatPrice <- function(prices_list){
  num_tick <- length(prices_list)
  tickers <- names(prices_list)

  # Sincronização ----
  datas <- prices_list[[1]][,1] %>%
    as.character
  for (i in 2:num_tick) {
    datas <- c(datas,
               prices_list[[i]][,1] %>%
                 as.character) %>%
      unique
  }
  datas <- datas %>%
    as.Date %>%
    sort %>%
    as.character

  # Merging ----
  mat_prices_list <- NULL
  for (i in 1:num_tick) {
    mat_prices_list <- cbind(mat_prices_list,
                             ifelse(datas %in% (prices_list[[i]][,1] %>%
                                                  as.character),
                                    prices_list[[i]][datas,2],
                                    NA))
  }

  rownames(mat_prices_list) <- datas
  colnames(mat_prices_list) <- tickers

  # Atribuição de valores faltantes ----
  mat_prices_list <- mat_prices_list %>% apply(2,attrNA)
  mat_prices_list <- mat_prices_list %>% na.omit

  datas <- rownames(mat_prices_list)

  mat_prices_list
}
