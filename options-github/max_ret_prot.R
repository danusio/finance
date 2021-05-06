#######################################################\#
# ANTES DE INICIAR, EXECUTAR:
# 1. ETL_stocks_options.R
# 2. ETL_options.R
#######################################################\#

rm(list = ls())

ini_time <- Sys.time()

library(magrittr)
library(bizdays)
library(stringr)
library(TTR)
library(pracma)
library(data.table)

# FUNÇÕES AUXILIARES ----
port2eng <- function(x){
  x <- x %>% str_remove_all("\\.")
  x <- x %>% str_replace_all(",","\\.")
  x <- x %>% as.numeric
  x
}

# SETUP ----
setwd("~/Documentos/APP BOLSA")
tickers <- list.files("csv_options/")
venc <- "2021-01-18" %>% as.Date
spread <- bizdays(Sys.Date(),venc,"Brazil/ANBIMA") + 1

# WEB SCRAPING ----
tabs <- fread("PROJETO 1/info_options",sep = ",",
              showProgress = F,verbose = F)

# PROCESSAMENTO ----
R2_cart <- Er <- pr <- st <- last_price_vec <- NULL
for (ticker1 in tickers) {
  precos <- fread(paste0("csv_options/",ticker1),sep = ",",
                  showProgress = F,verbose = F)
  # FDP ----
  y <- precos$close.adj
  deltay <- y %>% log %>% diff(lag=spread) %>% exp - 1
  dens_deltay <- deltay %>% density
  fdp <- approxfun(dens_deltay$x,dens_deltay$y)
  lim <- dens_deltay$x %>% range
  At <- integral(fdp,lim[1],lim[2])
  
  probF <- function(x){integral(fdp,x,lim[2])/At}
  
  # otimização ----
  last_price <- y %>% tail(1)
  last_price_vec <- c(last_price_vec,last_price)
  
  ticker <- str_remove(ticker1,"\\.SA")
  
  # seleção das PUT's relativas ao ticker, com vencimento 'venc'
  sel_tick <- tabs$ativo == ticker
  df <- tabs[sel_tick,]
  df <- df[df$strike %>% order,]
  df <- df %>% na.omit
  
  Prmax <- max(df$premio) + 1e-2
  
  df$y <- log(Prmax/df$premio - 1)
  
  LM <- lm(y ~ strike,df)
  descr <- LM %>% summary
  
  A <- LM$coefficients[2]
  b <- LM$coefficients[1]
  
  names(A) <- names(b) <- NULL
  
  PrFunc <- function(x){Prmax/(1+exp(A*x+b))}
  
  fobj <- function(x){
    Pr <- PrFunc(x)
    delta <- x/last_price - 1
    Z <- Pr/x * (last_price - x + Pr)/last_price * probF(delta)
    Z
  }
  
  f <- 15/100
  ans <- optimize(fobj,interval = c(min(df$strike),(1+f)*last_price),
                  maximum = T)
  str_opt_cont <- ans[[1]]
  
  dif <- (df$strike - str_opt_cont) %>% abs
  
  str_opt <- df$strike[which(dif == min(dif))]
  pr_opt <- df$premio[which(dif == min(dif))]
  
  pr <- c(pr,pr_opt)
  st <- c(st,str_opt)
  
  tick_opt <- df$ticker[which(dif == min(dif))]
  R2_adj <- descr$adj.r.squared
  rate_PrSt <- pr_opt/str_opt
  rate_PrSt_teor <- PrFunc(str_opt_cont)/str_opt_cont
  
  R2_cart <- c(R2_cart,R2_adj)
  Er <- c(Er,rate_PrSt)
  
  cat("Ativo: ");cat(ticker);cat("\n")
  cat("R² ajustado: ");cat(R2_adj);cat("\n")
  cat("Opção selecionada: ");cat(tick_opt);cat("\n")
  cat("Strike ótimo: ");cat(str_opt);cat("\n")
  cat("Prêmio: ");cat(pr_opt);cat("\n")
  cat("Pr/St: ");cat((rate_PrSt*100) %>% round(2));cat(" %\n")
  cat("Pr/St teórico: ");cat((rate_PrSt_teor*100) %>% round(2));cat(" %\n")
  cat("----------------------------------\n\n")
}

# OTIMIZAÇÃO DE CARTEIRA ----
fobj <- function(w){
  if (sum(w<0)>0) {
    return(-1e16*sum(w<0))
  }
  
  fp <- (last_price_vec - st + pr)/last_price_vec
  
  w <- w/sum(w)
  Z <- sum(w*pr*R2_cart)/sum(w*st) * sum(w*fp*R2_cart) 
  Z
}

w0 <- rep(1/length(tickers),length(tickers))
ans <- optim(w0,fobj,control = list(fnscale=-1,maxit=10000))

w_opt <- ans$par/sum(ans$par)
names(w_opt) <- tickers

cat("Composição ótima da carteira (%): \n")
print((w_opt*100) %>% round(2));cat("\n")

end_time <- Sys.time()
print(end_time - ini_time)