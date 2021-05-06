# OTIMIZAÇÃO PARA A VENDA DE PUT

rm(list = ls())

ini_time <- Sys.time()

library(magrittr)
library(bizdays)
library(rvest)
library(stringr)
library(TTR)
library(pracma)

# SETUP ----
setwd("~/Documentos/APP BOLSA")
tickers <- list.files("csv_options/")
venc <- "2021-01-18" %>% as.Date
spread <- bizdays(Sys.Date(),venc,"Brazil/ANBIMA") + 1
# url_base <- "https://opcoes.net.br/matriz-opcoes-strike-x-vencimento/PUTs/"
aim_price <- "high"
capital <- 10000

num_acoes <- length(tickers)

# FUNÇÕES ----
predFunc1 <- function(y,shift=1){
  N <- length(y)
  x <- 1:N
  modelo <- lm(y ~ x)
  x0 <- data.frame(x = N+shift)
  
  out <- predict(modelo,x0)
  names(out) <- NULL
  
  out
}

movMeas <- function(x,f,n,...){
  N <- length(x)
  
  out <- rep(NA,n-1)
  for (i in n:N) {
    v <- matrix(x[(i-n+1):i],nrow = 1)
    out <- c(out,
             apply(v,1,f,...))
  }
  out
}

predSt <- function(tickers,venc,aim_price){
  strike <- NULL
  for (papel in tickers) {
    dados_acao <- read.csv(paste0("csv/",papel))
    
    last_date <- dados_acao$date %>% tail(1)
    spread <- bizdays(from = last_date,to = venc,cal = "Brazil/ANBIMA")
    m <- max(20,(1.5*spread) %>% round)
    
    N <- nrow(dados_acao)
    y <- dados_acao[,aim_price]
    
    bbands <- BBands(dados_acao[,c("high","low","close.adj")],
                     n = 2*spread)
    y1 <- c(rep(NA,spread), # o elemento 'i' de y1 é a predição para o elemento 'i+spread' de y
            movMeas(y,predFunc1,132,shift = spread))
    
    df <- data.frame(outcome = y[(1+spread):N],
                     outcomeD0 = y[1:(N-spread)],
                     bbands[1:(N-spread),1:3],
                     outpred1 = y1[(1+spread):N])
    df <- df[complete.cases(df),]
    
    A <- tail(df[,-1],m) %>% as.matrix
    b <- tail(df[,1],m) %>% as.matrix(ncol = 1)
    
    fobj <- function(w){
      if (sum(w<0)>0) {
        return(1e6)
      }
      w <- matrix(w,ncol = 1)
      
      f <- (b - A %*% w)/b
      
      if (sum(f<0)>0) {
        return(1e6)
      }
      
      # return((f^2) %>% mean %>% sqrt)
      return(f %>% mean)
    }
    
    npred <- ncol(A)
    w0 <- rep(1/npred,npred)*0.5
    
    ans <- optim(w0,fobj,control = list(maxit = 2000))
    w_opt <- ans$par
    res <- ans$value*100
    
    xpred <- c(y[N],
               bbands[N,1:3],
               y1[N+spread])
    ypred <- sum(w_opt*xpred)
    
    strike <- c(strike,ypred)
  }
  names(strike) <- tickers
  strike
}

genFDP <- function(tickers,spread){
  fdp <- list()
  rg <- NULL
  for (ticker in tickers) {
    X <- read.csv(paste0("csv/",ticker))
    
    y <- X[,6]
    delta <- y %>% log %>% diff(lag=spread) %>% exp - 1
    delta <- delta %>% na.omit
    
    dens_delta <- density(delta)
    
    gFunc <- approxfun(x = dens_delta$x,y = dens_delta$y,rule = 1)
    g <- function(x){ifelse(is.na(gFunc(x)),0,gFunc(x))}
    fdp[[ticker]] <- g
    rg <- rbind(rg,
                range(dens_delta$x))
  }
  out <- list(func = fdp,
              range = rg)
  out
}

# STRIKES PREDITOS ----
st_pred <- predSt(tickers,venc,aim_price)

# STRIKES REAIS
tab_opcoes <- read.csv("PROJETO 1/info_options")

st_real <- tickers_opt <- NULL
for (i in 1:num_acoes) {
  ticker <- tickers[i] %>% str_remove_all("\\.SA")
  df <- tab_opcoes[tab_opcoes$ativo == ticker,]
  
  difs <- (st_pred[i] - df[,"strike"]) %>% abs
  sel <- which(difs == min(difs))
  st_real <- c(st_real,df[sel,"strike"])
  tickers_opt <- c(tickers_opt,df[sel,"ticker"])
}

names(st_real) <- tickers_opt
names(tickers_opt) <- tickers

cat("Strikes selecionados: \n")
st_real %>% print
cat("\n")

# PRÊMIOS ----
Pr <- NULL
for (i in 1:num_acoes) {
  Pr <- c(Pr,
          tab_opcoes$premio[tab_opcoes$ticker == tickers_opt[i]])
}
names(Pr) <- tickers_opt

# VALUE AT RISK ----
list_VaR <- genFDP(tickers,spread)
FDP <- list_VaR$func
limits <- list_VaR$range

VaR_tick <- NULL
for (i in 1:length(FDP)) {
  g <- FDP[[i]]
  rg <- limits[i,]
  
  precos <- read.csv(paste0("csv/",tickers[i]))
  P_N <- precos$close.adj %>% tail(1)
  delta_S <- st_real[i]/P_N - 1
  
  # cálculo delta_h ----
  f_ls <- function(x){integral(g,rg[1],x)}
  f_rs <- function(x){integral(g,x,delta_S)}
  
  fobj <- function(x){(f_ls(x) - f_rs(x)) %>% abs}
  
  ans <- optimize(fobj,lower = -1,upper = delta_S)
  delta_H <- ans$minimum
  
  # cálculo VaR ----
  At <- integral(g,rg[1],rg[2])
  prob_risk <- integral(g,rg[1],delta_S)/At
  VaR <- ((1+delta_H)*P_N + Pr[i] - st_real[i])*prob_risk
  VaR_tick <- c(VaR_tick,-VaR)
}

# RETORNOS ----
Er_tick <- NULL
for (i in 1:length(FDP)) {
  g <- FDP[[i]]
  rg <- limits[i,]
  
  precos <- read.csv(paste0("csv/",tickers[i]))
  P_N <- precos$close.adj %>% tail(1)
  delta_S <- st_real[i]/P_N - 1
  
  # cálculo Er ----
  prob_suc <- integral(g,delta_S,rg[2])
  Er <- (Pr[i])*prob_suc
  Er_tick <- c(Er_tick,Er)
}

# OTIMIZAÇÃO ----
# risco-retorno
fobj <- function(w){
  if (sum(w<0)>0) {
    return(-1e6)
  }
  
  w <- w/sum(w)
  R <- (w*Er_tick) %>% sum
  S <- (w*VaR_tick) %>% sum
  out <- R/S
  out
}

w0 <- rep(1/num_acoes,num_acoes)
ans <- optim(w0,fobj,
             control = list(fnscale=-1,
                            maxit=1e4))
w_rr <- ans$par/sum(ans$par)
names(w_rr) <- tickers

cat("Composição da carteira - razão retorno/risco (%): \n")
(100*w_rr) %>% round(2) %>% print
cat("\nQtdes: \n")
(w_rr*capital/st_real/0.6) %>% round %>% print
cat("\n\n")

# utilidade
alfa <- seq(0,1,length.out = 500)

earn <- risk <- NULL
for (alfa1 in alfa) {
  alfa2 <- 1 - alfa1
  
  fobj <- function(w){
    if (sum(w<0)>0) {
      return(-1e6)
    }
    
    w <- w/sum(w)
    R <- (w*Er_tick) %>% sum
    S <- (w*VaR_tick) %>% sum
    out <- alfa1*R - alfa2*S
    out
  }
  
  ans <- optim(w0,fobj,
               control = list(fnscale=-1,
                              maxit=1e4))
  w_ut <- ans$par/sum(ans$par)
  
  earn <- c(earn,(w_ut*Er_tick) %>% sum)
  risk <- c(risk,(w_ut*VaR_tick) %>% sum)
}

# plotagem
plot(risk,earn - risk,pch = 16,
     ylab = "retorno útil",xlab = "valor em risco")

# alfa ótimo
pos <- which((earn-risk)==max(earn-risk))
alfa1_opt <- alfa[pos]

cat("Alfa ótimo - utilidade: ");cat(alfa1_opt);cat("\n")

alfa1 <- alfa1_opt[1]
alfa2 <- 1 - alfa1

fobj <- function(w){
  if (sum(w<0)>0) {
    return(-1e6)
  }
  
  w <- w/sum(w)
  R <- (w*Er_tick) %>% sum
  S <- (w*VaR_tick) %>% sum
  out <- alfa1*R - alfa2*S
  out
}

ans <- optim(w0,fobj,
             control = list(fnscale=-1,
                            maxit=1e4))
w_ut <- ans$par/sum(ans$par)
names(w_ut) <- tickers

cat("Composição da carteira - utilidade (%): \n")
(100*w_ut) %>% round(2) %>% print
cat("\nQtdes: \n")
(w_ut*capital/st_real/0.6) %>% round %>% print
cat("\n\n")

end_time <- Sys.time()

(end_time - ini_time) %>% print
