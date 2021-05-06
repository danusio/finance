#' @title
#' Atribuição de Preços Faltantes
#'
#' @description
#' Em uma série temporal \code{x}, atribui o valor de \code{x[i-1]} a \code{x[i]}, caso
#' \code{is.na(x[i])} seja \code{TRUE} e \code{is.na(x[i-1])} seja \code{FALSE}.
#'
#' @param x Vetor organizado temporalmente.
#'
#' @return Vetor, do mesmo comprimento de \code{x}, com os valores faltantes
#' atribuídos, se possível.
#' @export
#'
#' @examples
#'
#' x <- c(NA,NA,1,3,NA,7,-5,NA,NA,13)
#' print(attrNA(x))

attrNA <- function(x){
  # atribui o elemento imediatamente anterior a um valor faltante
  # x: vetor
  # value: vetor com os NA's substituidos, exceto os primeiros 'missing values'.

  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}
