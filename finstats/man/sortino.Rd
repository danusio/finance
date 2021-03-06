% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sortino.R
\name{sortino}
\alias{sortino}
\title{Sortino Ratio}
\usage{
sortino(x, rf = 0)
}
\arguments{
\item{x}{Vetor com os rendimentos.}

\item{rf}{Taxa livre de risco relativa ao período dos rendimentos. Se os retornos foram
calculados com um lag de 120 dias, a rf deverá ser relativa a um período de 120 dias.}
}
\value{
Sortino Ratio dos retornos presentes em \code{x}.
}
\description{
Calcula o índice de Sortino de uma série de retornos. A principal diferença entre este e o
Índice de Sharpe é que Sortino considera somente o desvio padrão dos retornos abaixo da
Taxa Livre de Risco (i.e., não "pune" o investimento pelo risco benéfico).
}
\examples{

set.seed(1)

rend <- rnorm(300,0.08,0.15)
print(sortino(rend,0.03))
}
