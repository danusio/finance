#' Sumarização das Métricas de Desempenho
#'
#' Dado um vetor de retornos (principalmente de estratégias), exibe métricas de desempenho
#' anualizadas.
#'
#' @param vec_rend Vetor com os rendimentos, organizados temporalmente.
#' @param s Lag, em dias corridos, sobre o qual foram calculados os rendimentos.
#' @param rf Taxa livre de risco relativa ao período dos rendimentos. Se os retornos foram
#' calculados com um lag de 120 dias, a rf deverá ser relativa a um período de 120 dias.
#'
#' @return Data frame contendo:
#'
#' 1. Effective Annual Rate (EAR);
#'
#' 2. Desvio-padrão anualizado dos retornos;
#'
#' 3. Sharpe Ratio anualizado;
#'
#' 4. Drawdown em uma base de 30 dias. Se \code{s} é de 180 dias, por exemplo, o drawdown será
#' dividido por 6. O valor é correspondente ao quantil 5% do vetor de rendimentos
#' \code{vec_rend}.
#'
#' 5. Taxa de períodos com retorno positivo.
#'
#' @export
#'
#' @examples
#'
#' set.seed(1)
#'
#' rend <- rnorm(300,0.08,0.15)
#' print(finSummary(rend,180,0.03))

finSummary <- function(vec_rend,s,rf=0){
  # vec_rend: vetor com os rendimentos
  # s: lag, em dias corridos, em que foram tomados os rendimentos
  # value: sumarização das métricas de desempenho

  met1 <- mean(vec_rend,na.rm = T)*365/s # média dos retornos anualizada
  met2 <- sharpe(vec_rend,rf)*sqrt(365/s) # SR
  met3 <- quantile(vec_rend,0.05,names = F)*30/s # Drawdown mensalizado
  met4 <- sum(vec_rend>0,na.rm = T)/length(vec_rend) # % de períodos positivos

  # exibição no console
  out <- data.frame(x1=met1,
                    x2=sd(vec_rend,na.rm = T)*sqrt(365/s),
                    x3=met2,
                    x4=met3,
                    x5=met4)
  colnames(out) <- c("EAR","desvpad_ano","SR","drawdown_mes","tax_posit")
  out
}
