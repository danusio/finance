# OTIMIZAÇÃO PARA A VENDA DE CALL

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
tickers <- list.files("csv/")
venc <- "2020-12-21" %>% as.Date
spread <- bizdays(Sys.Date(),venc,"Brazil/ANBIMA") + 1
url_base <- "https://opcoes.net.br/matriz-opcoes-strike-x-vencimento/CALLs/"
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
            movMeas(y,predFunc1,2*spread,shift = spread))
    
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
      
      f <- (A %*% w - b)/b
      
      if (sum(f<0)>0) {
        return(1e6)
      }
      
      # return((f^2) %>% mean %>% sqrt)
      return(f %>% mean)
    }
    
    npred <- ncol(A)
    w0 <- rep(1/npred,npred)*2
    
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

genListSt <- function(tickers,url_base,venc){
  tickers1 <- str_split(tickers,"\\.",simplify = T)[,1]
  
  out <- list()
  for (i in 1:length(tickers1)) {
    URL <- paste0(url_base,tickers1[i])
    
    obj_html <- read_html(URL)
    tab <- html_table(obj_html,fill = T)[[1]]
    names(tab)[1] <- "strikes"
    
    tab[,1] <- tab[,1] %>% str_replace_all("\\.","")
    tab[,1] <- tab[,1] %>% str_replace_all("\\,","\\.")
    tab[,1] <- tab[,1] %>% as.numeric
    
    data_col <- format(venc,"%d/%m/%Y")
    lista_st <- tab[,c("strikes",data_col)]
    
    sel_rm <- which(lista_st[,2] == "")
    lista_st <- lista_st[-sel_rm,]
    
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("FM")
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("\t")
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("\n")
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("\r")
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("\v")
    lista_st[,2] <- lista_st[,2] %>% str_remove_all("\f")
    lista_st[,2] <- lista_st[,2] %>% str_trim(side = "both")
    
    out[[tickers[i]]] <- lista_st
  }
  out
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

prFunc <- function(tickers_opt){
  url_opt <- "https://opcoes.net.br/"
  
  premios <- NULL
  for (i in 1:length(tickers_opt)) {
    URL <- paste0(url_opt,tickers_opt[i])
    obj_html <- read_html(URL)
    tab <- html_table(obj_html,fill = T)
    Pr <- tab[[1]][3,5]
    Pr <- Pr %>% str_remove_all("\\.")
    Pr <- Pr %>% str_replace_all(",","\\.")
    Pr <- Pr %>% as.numeric
    premios <- c(premios,Pr)
  }
  names(premios) <- names(tickers_opt)
  premios
}

# STRIKES PREDITOS ----
st_pred <- predSt(tickers,venc,aim_price)

# STRIKES REAIS
st_list <- genListSt(tickers,url_base,venc)

st_real <- tickers_opt <- NULL
for (i in 1:length(tickers)) {
  lista <- st_list[[names(st_pred)[i]]]
  difs <- (st_pred[i] - lista[,1]) %>% abs
  sel <- which(difs == min(difs))
  st_real <- c(st_real,lista[sel,1])
  tickers_opt <- c(tickers_opt,lista[sel,2])
}

names(st_real) <- tickers_opt
names(tickers_opt) <- tickers

cat("Strikes selecionados: \n")
st_real %>% print
cat("\n")

# PRÊMIOS ----
Pr <- prFunc(tickers_opt)

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
