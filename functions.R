Rcpp::sourceCpp("SBM.cpp")
library(dplyr)
library(casabourse)
library(plotly)
library(lubridate)
#Importation des noms des instruments et de leurs codes-------
# ticker  <- tickers()
#
# instrument = instruments()
#
# names(instrument)[1]=names(ticker)[2]
#
# asset = merge(ticker,instrument)
# names(asset)[3] = "Instruments"
# asset = asset %>%
#   arrange(Instruments)
# write.csv(asset,"asset.csv",row.names = F)
asset = read.csv("asset.csv")
# Liste des instruments et des tickers------
list_instrument <- function(){
  asset$Instruments
}

list_ticker <- function(){
  asset$tick
}

# Retrouver le ticker d'un instrument donné---
get_ticker <- function(Nom){
  asset$tick[which(asset$Instruments == Nom)]
}

# Trapezoid mean -------
trapezoidal <-  function(x){
  0.5*(1/length(x))*(sum(x[c(1,length(x))]) + 2*sum(x[-c(1,length(x))]))
}

# Brownian motions-------

# Standard
SBM <- function(t0 = 0,T_ = 1,N = 10000,nsim = 100){
  X <- Map(
    function(x) {c(0,rnormCpp(n= N,mu = 0,sigma = sqrt((T_ -t0)/N))) %>%
        cumsumCpp()},
    x = as.list(rep(N,nsim))
  ) %>%
    bind_cols() %>%
    `names<-`(paste0("X",1:nsim))
  X <- X %>%
    mutate(t=seq(t0,T_,length.out = N +1) ) %>%
    relocate(t)

  plt <- X %>%
    reshape2::melt(id="t",value.name = "Brownian") %>%
    ggplot(aes(x=t, y=Brownian, colour=variable)) +
    geom_line(show.legend = F) +
    ylab("Brownian motion")+
    hrbrthemes::theme_ipsum(grid = "")

  list(data = X,plots =plt )

}

# Geometric
GBM <- function(S0=100,mu = .05,sigma =.4,t0=0,T_= 1,N = 10000,nsim = 100){
  sbm <- SBM(t0 = t0,T_ = T_,N = N,nsim = nsim)
  t <- sbm$data$t
  listSBM <-  as.list(sbm$data[,-1])
  S = Map(function(x){
    S0*exp(t*(mu-0.5*sigma^2)+sigma*listSBM[[x]])
  },x=1:nsim) %>%
    bind_cols() %>%
    `names<-`(paste0("X",1:nsim)) %>%
    mutate(t=t) %>%
    relocate(t)
  plt <- S %>%
    reshape2::melt(id="t",value.name = "GBM") %>%
    ggplot(aes(x=t, y=GBM, colour=variable)) +
    geom_line(show.legend = F) +
    ylab("Geometric Brownian Motion")+
    hrbrthemes::theme_ipsum(grid = "XY")
  mean_Schema1 = apply(S[,-1],2,mean)
  mean_Schema2 = apply(S[,-1],2,trapezoidal)
  mean_path = apply(S[,-1],1,mean)
  ST = mean_path[length(mean_path)]
  plt_mean <- ggplot(data = NULL,aes(x=t, y=mean_path,color = "red")) +
    geom_line(show.legend = F) +
    ylab("Mean price")+
    hrbrthemes::theme_ipsum(grid = "XY")
  list(data = S,plots =plt ,
       mean_Schema1 =mean_Schema1 ,mean_Schema2 = mean_Schema2,
       ST_schema1 = mean(mean_Schema1),ST_schema2 = mean(mean_Schema2),
       mean_path = mean_path,
       ST = ST,
       plt_mean = plt_mean)
}

# Formating date-----
My_format <- function(Date){
  paste0(rev(unlist(strsplit(as.character(Date),"-"))),collapse = "-")
}

# Ploting-----
plot_df <- function(df0){
  actif = names(df0)[2]
  names(df0)[2] = "asset"
  df0 %>%
    plot_ly(x =~Date, y = ~asset, type = "scatter",mode = "lines") %>%
    layout(yaxis = list(title = actif))
}

# Drift & volatily -------
historical <- function(df0){
  x = df0[[2]]
  rent = (x[-1]/x[-length(x)] -1)
  drift = 252*mean(rent)
  volatilty = sqrt(252)*sd(rent)
  list(drift = drift, sigma = volatilty)

}

# Geometric asian option ------------
geom_asian <- function(S0,K,T_,r,sigma){
  d1 = 0.5*(r - (sigma^2)/6)*T_
  d2 = (1/(sigma*sqrt(T_/3)))*(log(S0/K)+0.5*T_*(r+(sigma^2)/6))
  call = S0*exp(d1)*pnorm(d2) - K*pnorm(d2-sigma*sqrt(T_/3))
  put = -S0*exp(d1)*pnorm(-d2) + K*pnorm(-d2+sigma*sqrt(T_/3))
  list(call = call, put = put)
}
# Pricing--------------
pricing <- function(Averaging,Strike_type,Schema,gbm,S0,K=0,r,T_,sigma){
  if(Averaging == "Arithmetic"){
    switch(Schema,
           "Riemann" = switch (
             Strike_type,
             Fixed = list(
               call =exp(-r*T_)*mean(ifelse(K < gbm$mean_Schema1, -K + gbm$mean_Schema1, 0 )),
               put =exp(-r*T_)*mean(ifelse(K > gbm$mean_Schema1, K - gbm$mean_Schema1, 0 ))
             ),
             Floting = list(
               call =exp(-r*T_)*mean(ifelse(gbm$ST > gbm$mean_Schema1,gbm$ST - gbm$mean_Schema1,0)),
               put =exp(-r*T_)*mean(ifelse(gbm$ST < gbm$mean_Schema1,-gbm$ST + gbm$mean_Schema1,0))
             )
           ),
           "Trapezoid" = switch (
             Strike_type,
             Fixed = list(
               call =exp(-r*T_)*mean(ifelse(K < gbm$mean_Schema2, -K + gbm$mean_Schema2, 0 )),
               put =exp(-r*T_)*mean(ifelse(K > gbm$mean_Schema2, K - gbm$mean_Schema2, 0 ))
             ),
             Floting = list(
               call = exp(-r*T_)*mean(ifelse(gbm$ST > gbm$mean_Schema2,gbm$ST - gbm$mean_Schema2,0)),
               put = exp(-r*T_)*mean(ifelse(gbm$ST < gbm$mean_Schema2,-gbm$ST + gbm$mean_Schema2,0))
             )
           )

           )
  }
  else{
    K = switch(
      Strike_type,
      "Fixed" = K,
      "Floting" = switch (
        Schema,
        "Riemann" = gbm$ST_schema1,
        "Trapezoid" = gbm$ST_schema2
      )
      )
    geom_asian(S0,K,T_,r,sigma)
  }
}
