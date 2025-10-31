rm(list=ls())

# Lista de pacotes necessários
pacotes <- c(
  "tseries", "Mcomp", "forecast", "dplyr", "ggplot2", "tidyverse",
  "lubridate", "xgboost", "elmNNRcpp", "wavelets", "gbm",
  "pls", "earth", "stringr", "randomForest", "seasonal"
)

# Função para instalar e carregar automaticamente
instalar_carregar <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Pacote", pkg, "não encontrado. Instalando..."))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
    message(paste("Pacote", pkg, "já está instalado e foi carregado."))
  }
}

# Aplicar a função a todos os pacotes
invisible(lapply(pacotes, instalar_carregar))

# função que converte data de formato ym do lubridate para formato de vetor com ano e mes
ym_to_vec = function(date){return(as.numeric(stringr::str_split(as.character(date),'-',simplify = T)[,1:2]))}
# função que converte data de vetor com ano e mes para formato de ym do lubridate
vec_to_ym = function(ym){return(lubridate::ym(paste0(ym,collapse = "-")))}

# função que calcula mae
mae = function(obs,prev){
  e = obs-prev
  a = abs(e)
  m = mean(a)
  return(m)
}
# função que calcula mape
mape = function(obs,prev){
  pe = (obs-prev)/obs
  a = abs(pe)
  m = 100*mean(a)
  return(m)
}
# função que calcula smape
smape = function(obs,prev){
  n=length(obs)
  sape = abs((obs-prev))/((abs(obs)+abs(prev))/2)
  m = (100/n)*sape
  return(m)
}


# função para previsoes do sarima
f_arima = function(y,h, dif){
  
  if(dif==TRUE){
    serie_ori = y
    y = diff(y)}
  
  set.seed(0123456789)
  fct = forecast(auto.arima(y),h=h)
  pred = as.numeric(fct$mean)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  
  return(pred)
}


# função para previsoes do ets
f_ets <- function(y, h, dif){
  
  if(dif==TRUE){
    serie_ori = y
    y = diff(y)}
  
  set.seed(0123456789)
  fct = forecast(ets(y),h=h)
  pred = as.numeric(fct$mean)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  
  return(pred)
}

# função para previsoes do tbats
f_tbats = function(y,h, dif){
  
  if(dif==TRUE){
    serie_ori = y
    y = diff(y)}
  
  set.seed(0123456789)
  fct = forecast(tbats(y),h=h)
  pred = as.numeric(fct$mean)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  
  return(pred)
}

# função para previsoes do RF
f_rf = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  data_ = data.frame(cbind(y,xreg))
  names(data_) = str_replace_all(names(data_),"xreg.","")
  variaveis_preditoras <- setdiff(names(data_), c("y"))
  
  set.seed(0123456789)
  rf.fit <- randomForest(
    formula = y ~ .,
    data = data_,
    ntree = 500,  
    mtry = floor(length(variaveis_preditoras)/3),
    importance = TRUE
  )
  

  pred = predict(rf.fit, newdata = newxreg)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  list_output = list(rf_fct = pred, modelo_ajustado =  rf.fit)
  return(list_output)
}

# função para previsoes do xgboost
f_xgboost = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(xreg), label = y)
  
  set.seed(0123456789)
  xgb.fit <- xgboost::xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
  

  pred = predict(xgb.fit, newdata = as.matrix(newxreg))
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  return(pred)
}

# função para previsoes do elm
f_elm = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  
  set.seed(0123456789)
  elm.fit <- elmNNRcpp::elm_train(x = as.matrix(xreg), y = as.matrix(y), nhid = 50, actfun = "sig")
  

  pred = elmNNRcpp::elm_predict(elm.fit, as.matrix(newxreg))
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  return(pred)
}

# função para previsoes do gbm
f_gbm = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  data_ = data.frame(y = y, xreg)
  set.seed(0123456789)
  gbm.fit <- gbm::gbm(
    y ~ .,
    data = data_,
    distribution = "gaussian",
    n.trees = 100,
    interaction.depth = 3,
    shrinkage = 0.1,
    verbose = FALSE
  )
  

  pred = predict(gbm.fit, newdata = newxreg, n.trees = 100)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
  }else{
    pred = data.frame(mean = pred)
  }
  return(pred)
}

# função para previsoes do pcr
f_pcr = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  data_ = data.frame(y = y, xreg)
  set.seed(0123456789)
  pcr.fit <- pls::pcr(y ~ ., data = data_, scale = TRUE, validation = "CV")
  ncomp_opt <- which.min(pcr.fit$validation$PRESS)

  pred = predict(pcr.fit, newdata = newxreg, ncomp = ncomp_opt)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
    names(pred)[1]='mean'
  }else{
    pred = data.frame(mean = pred)
    names(pred)[1]='mean'
  }
  return(pred)
}

# função para previsoes do mars
f_mars = function(y, h, xreg, newxreg, dif){
  
  if(dif==TRUE){
    serie_ori = y
    xreg_newxreg = ts(rbind(xreg,newxreg),start = start(xreg),end = end(newxreg),frequency = 12)
    diff_xreg_newxreg = diff(xreg_newxreg)
    xreg = window(diff_xreg_newxreg,start=start(xreg),end=end(xreg))
    newxreg = window(diff_xreg_newxreg,start=start(newxreg),end=end(newxreg))
    y = diff(y)}
  
  xreg=data.frame(xreg);newxreg=data.frame(newxreg)
  
  data_ = data.frame(y = y, xreg)
  set.seed(0123456789)
  mars.fit <- earth::earth(y ~ ., data = data_)
  

  pred = predict(mars.fit, newdata = newxreg)
  
  if(dif==TRUE){
    dif_serie_pred = c(as.numeric(y),pred) 
    recuperada <- c(serie_ori[1], serie_ori[1] + cumsum(dif_serie_pred))
    recuperada_pred =  recuperada[(length(recuperada)-(length(pred)-1)):length(recuperada)]
    pred = data.frame(mean = recuperada_pred)
    names(pred)[1]='mean'
  }else{
    pred = data.frame(mean = pred)
    names(pred)[1]='mean'
  }
  return(pred)
}


# função para previsoes dos modelos
f_modelo = function(modelo, y, h, xreg = NULL, newxreg = NULL, dif_ml=TRUE){
  
  
  modelo = toupper(modelo)
  msg_stop = paste0("Modelo",modelo,"não reconhecido.")
  
  if(modelo == "RF"){
    rf_fct  = f_rf(y, h, xreg, newxreg, dif=dif_ml)$rf_fct
    return(rf_fct)
  } else if(modelo == "XGBOOST"){
    return(f_xgboost(y, h, xreg, newxreg, dif=dif_ml))
  } else if(modelo == "ELM"){
    return(f_elm(y, h, xreg, newxreg, dif=dif_ml))
  } else if(modelo == "GBM"){
    return(f_gbm(y, h, xreg, newxreg, dif=dif_ml))
  } else if(modelo == "PCR"){
    return(f_pcr(y, h, xreg, newxreg, dif=dif_ml))
  } else if(modelo == "MARS"){
    return(f_mars(y, h, xreg, newxreg, dif=dif_ml))
  } else if(modelo == "SARIMA"){
    return(f_arima(y, h, dif=dif_ml))
  } else if(modelo == "ETS"){
    return(f_ets(y, h, dif=dif_ml))
  } else if(modelo == "TBATS"){
    return(f_tbats(y, h, dif=dif_ml))
  } else {
    stop(msg_stop)
  }
}

# função para estudo de validacao cruzada por janela deslizante
swcv_pinned = function(modelo,y,h,window_,initial,xreg = NULL,dif_ml=TRUE){
  n <- length(y)
  Hor.MAEs <- c();Hor.MAPEs = c();Hor.sMAPEs = c()
  Hors = c()
  for (i in 1:(h)) {
    mes_zero= vec_to_ym(start(y)) %m-% months(1)
    start_treino = mes_zero %m+% months(initial+i)
    end_treino = start_treino %m+% months(window_)
    dados_treinoi <- window(y,start = ym_to_vec(start_treino),end = ym_to_vec(end_treino)) 
    dados_testei <- window(y,start = ym_to_vec(end_treino %m+% months(1)),end = ym_to_vec(end_treino %m+% months(h)) )
    covs_treinoi <- window(xreg,start = ym_to_vec(start_treino),end = ym_to_vec(end_treino)) 
    covs_testei <- window(xreg,start = ym_to_vec(end_treino %m+% months(1)),end = ym_to_vec(end_treino %m+% months(h)) )
    predicoes_modelo <- f_modelo(modelo, dados_treinoi, h, xreg = covs_treinoi, newxreg = covs_testei,dif_ml)$mean
    
    Hor.MAEs[i] <- mae(obs=dados_testei,prev=predicoes_modelo)
    Hor.MAPEs[i] <- mape(obs=dados_testei,prev=predicoes_modelo)
    Hor.sMAPEs[i] <- smape(obs=dados_testei,prev=predicoes_modelo)
    print(paste('H',i,'previsto com treino de',start_treino,'a',end_treino,'e teste de',end_treino %m+% months(1),'a',end_treino %m+% months(h)))
  }
  Hors = c(1:h)
  return(data.frame(MAEs = Hor.MAEs,
                    MAPEs = Hor.MAPEs,
                    sMAPEs = Hor.sMAPEs))
}

# função para aplicar o estudo de validacao cruzada por janela deslizante em todos os modelos
f_cv_models = function(modelos,y,h,wind,covs_aval,dif_ml=TRUE){
  maes_swcv = data.frame(matrix(NA,nrow = h,ncol = 3 ))
  inicialize = length(y) - 2*h - wind
  for(i in 1:length(modelos)){
    mae_swcv = swcv_pinned(modelos[i],y,h,window_ = wind,initial = inicialize,covs_aval,dif_ml=dif_ml)
    names(mae_swcv) = paste0(names(mae_swcv),'.',modelos[i])
    maes_swcv = cbind(maes_swcv,mae_swcv)
    print(mae_swcv)
  }
  names(maes_swcv)[1] = 'H'
  maes_swcv$H = c(1:h)
  return(maes_swcv)
}

# No Rstudio, no menu de opções, ir em: 'Session' -> 'Set Working Directory' e clicar em 'The Source File Location' 
dir = getwd()
setwd(dir) # Para confirmar diretório correto

# Carregando as bases
load(paste0(dir,'/Dados/bases.RData'))

dessaz = c(0,1)
dif    = c(TRUE,FALSE)
lags   = c(0,1)

maes = data.frame(
  dessaz = c(),
  dif = c(),
  lags = c(),
  modelo = c(),
  H = c(),
  MAE = c()
)

for(i in dessaz){
  for(j in dif){
    for(k in lags){
      h=18
      modelos = c("SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS")
      wind_len = 100
      
      y = bases[[paste0('y_',i)]][['treino']]
      covs = bases[[paste0('covs_0_',i,'_',k)]][['treino']]
      
      result = f_cv_models(modelos,y,h,wind=wind_len,covs,dif_ml=j)
      
      resultpivot = result %>% pivot_longer(colnames(result)[2:ncol(result)],values_to = 'MAE',names_to = 'modelo')
      resultpivot = na.omit(resultpivot)
      resultpivot$dessaz = i
      resultpivot$dif = ifelse(j==TRUE,1,0)
      resultpivot$lags = k
      
      maes = bind_rows(maes,resultpivot)
      
    }
  }
}






maes_ = maes
maes = maes_

maes$dessaz = ifelse(maes$dessaz == 1,'Sim','Não')
maes$dif= ifelse(maes$dif==1,'Sim','Não')
maes$lags = ifelse(maes$lags==1,'Sim','Não')

maes$metrica = str_split(maes$modelo,"\\.",simplify = T)[,1]
maes$modelo = str_split(maes$modelo,"\\.",simplify = T)[,2]

maes = maes %>% filter(metrica == "MAPEs")

maes = maes %>% select(c("dessaz","dif","lags","modelo","H","MAE"))
names(maes) = c("dessaz","dif","lags","modelo","H","MAPE")

maes$MAPE = round(maes$MAPE,2)
situacoes = maes %>% select(c("dessaz","dif","lags")) %>% distinct_all()
situacoes$Situacao = seq(1:8)

maes = maes %>% left_join(situacoes,by = c("dessaz","dif","lags"))


names(situacoes) = c("Dessaz","Dif","Lags","Situação")
print(xtable(situacoes,caption = 'Situações',align = 'ccccc'),include.rownames = FALSE)

maes$modelo = factor(maes$modelo,levels = c("SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS"))


for(situ in unique(maes$Situacao)){
  maes_loop = maes %>% filter(Situacao == situ)
  
  modelos = unique(maes$modelo)
  maes_loop_piv = maes_loop %>% 
    pivot_wider(names_from = 'modelo',values_from = 'MAPE') %>% 
    select(c('H',modelos))
  situcapt = paste0("MAPEs do estudo de validação cruzada para os modelos calculados na situação ",situ,".")
  print(xtable(maes_loop_piv,caption = situcapt,align = 'ccccccccccc'),include.rownames = FALSE)


  cores <- c(
    "#0072B2",  
    "#009E73",  
    "#56B4E9",  
    "#F0E442",  
    "#CC79A7",  
    "#D55E00",  
    "#8c564b",  
    "#7f7f7f",  
    "#6A0DAD"   
  )
  
  


  g1 = ggplot(maes_loop, aes(x = H, y = MAPE, color = modelo)) +
    geom_line(size = .5) +
    geom_point(size = 1.0) +
    scale_color_manual(values = cores) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = NULL,
      x = "Horizonte (H)",
      y = "MAPE"
    )
  
  ggsave(
    paste0(dir,"/Graficos_MAEs_SWCV/",
           situ,".png"),
    plot = g1, width = 7, height = 5, dpi = 300
    )

}





# Carregando as bases
load(paste0(dir,'/Dados/bases.RData'))

dessaz = c(0,1)
dif    = c(TRUE,FALSE)
lags   = c(0,1)
covs_prev = c(0,1)

erros = data.frame(
  dessaz = c(),
  dif = c(),
  lags = c(),
  covs_prev = c(),
  modelo = c(),
  H = c(),
  mes=c(),
  obs=c(),
  mean = c(),
  erro = c()
)
modelos = c("SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS")

h=5


for(i in dessaz){
  for(j in dif){
    for(k in lags){
      for(p in covs_prev){
        for(m in modelos){
          y = bases[[paste0('y_',i)]][['treino']]
          y_obs = bases[[paste0('y_',i)]][['teste']]
          covs = bases[[paste0('covs_',p,'_',i,'_',k)]][['treino']]
          covs_new = bases[[paste0('covs_',p,'_',i,'_',k)]][['teste']]
      
          result = f_modelo(modelo = m, y, h = h, xreg = covs, newxreg = covs_new, dif_ml=j)
          result$obs = y_obs
          result$erro = 100*(abs((y_obs-result$mean)/y_obs))
          result$H = c(1:h)
          result$mes = seq(ym(paste0(start(covs_new)[1],"-",start(covs_new)[2])),ym(paste0(end(covs_new)[1],"-",end(covs_new)[2])),by="month")
          result$covs_prev = p
          result$dessaz = i
          result$dif = ifelse(j==TRUE,1,0)
          result$lags = k
          result$modelo = m
          erros = bind_rows(erros,result)
          print(paste0(names(result),collapse = ' | '))
          print(paste0(result[nrow(result),],collapse = ' | '))
        }
      }
    }
  }
}


erros_ = erros
erros = erros_
erros$dessaz = ifelse(erros$dessaz == 1,'Sim','Não')
erros$dif = ifelse(erros$dif==1,'Sim','Não')
erros$lags = ifelse(erros$lags==1,'Sim','Não')
erros$covs_prev = ifelse(erros$covs_prev==1,'Sim','Não')

erros = erros %>% select(c("dessaz","dif","lags","covs_prev","modelo","H","erro")) %>% filter(!is.na(H))
erros$erro = round(erros$erro,2)
erros = erros %>% pivot_wider(names_from = "H",values_from = "erro")

erros$MAPE = (erros$`1` + erros$`2` + erros$`3` + erros$`4` + erros$`5`)/5

situacoes = erros %>% select(c("covs_prev","dessaz","dif","lags")) %>% distinct_all()
situacoes = situacoes %>% arrange(desc(covs_prev))
situacoes$situ = seq(1:16)

erros = erros %>% left_join(situacoes,by = c("dessaz","dif","lags","covs_prev"))

names(situacoes) = c("Covs_previstas","Dessaz","Dif","Lags","Situação")

print(xtable(situacoes,caption = "Situações de pré-processamento do estudo de previsão de 5 passos a frente.",align = 'cccccc'),include.rownames = FALSE)

erros = erros %>% arrange(situ)

for(sit in unique(erros$situ)){
  erros_loop = erros %>% filter(situ == sit)

  erros_loop_ = erros_loop %>% 
    select(c('modelo','1','2','3','4','5','MAPE'))
  names(erros_loop_) = c("Modelo","H = 1",'H = 2','H = 3','H = 4','H = 5','MAPE')
  situcapt = paste0("Erros e MAPEs do estudo de previsão de 5 passos a frente na situação ",sit,".")
  print(xtable(erros_loop_,caption = situcapt,align = 'cccccccc'),include.rownames = FALSE)
}


y_0 = bases$y_0$treino
y_1 = bases$y_1$treino

previsoes = erros_
rownames(previsoes)=seq(1:nrow(previsoes))



serie_ori = data.frame(mean= c(as.numeric(bases$y_0$teste),as.numeric(bases$y_1$teste)),
                       erro = rep(0,10),covs_prev=rep(0,10),dif=rep(0,10),lags=rep(0,10),H = rep(c(1:5),2),dessaz = rep(c(0,1),each = 5),
                       mes = previsoes$mes[1:10],modelo = rep("Observado",10))

previsoes = bind_rows(serie_ori,previsoes)

y_menos_12 = c()
y_menos_1 = c()



for(i in 1:nrow(previsoes)){
  prev_i = previsoes$mean[i]
  
  prevs_i = previsoes %>% 
    filter(covs_prev == previsoes$covs_prev[i]) %>% 
    filter(dessaz == previsoes$dessaz[i]) %>% 
    filter(dif == previsoes$dif[i]) %>% 
    filter(lags == previsoes$lags[i])%>% 
    filter(modelo == previsoes$modelo[i])  
  
  if(previsoes$dessaz[i]==0){
    y = ts(c(as.numeric(y_0),prevs_i$mean),start = start(y_0),end = ym_to_vec(max(prevs_i$mes)),frequency = 12)
    y_menos_1[i] = window(y,
                          start = ym_to_vec(previsoes$mes[i] %m-% months(1)),
                          end = ym_to_vec(previsoes$mes[i] %m-% months(1)))[1]
    y_menos_12[i] = window(y,
                           start = ym_to_vec(previsoes$mes[i] %m-% months(12)),
                           end = ym_to_vec(previsoes$mes[i] %m-% months(12)))[1]
    }
  if(previsoes$dessaz[i]==1){
    y = ts(c(as.numeric(y_1),prevs_i$mean),start = start(y_1),end = ym_to_vec(max(prevs_i$mes)),frequency = 12)
    y_menos_1[i] = window(y,
                          start=ym_to_vec(previsoes$mes[i] %m-% months(1)),
                          end=ym_to_vec(previsoes$mes[i] %m-% months(1)))[1]
    y_menos_12[i] = window(y,
                           start = ym_to_vec(previsoes$mes[i] %m-% months(12)),
                           end = ym_to_vec(previsoes$mes[i] %m-% months(12)))[1]
  }
}

previsoes$y_menos_1 = y_menos_1
previsoes$y_menos_12 = y_menos_12

previsoes$Variacao_1 = (previsoes$mean - previsoes$y_menos_1)/previsoes$y_menos_1
previsoes$Variacao_12 = (previsoes$mean - previsoes$y_menos_12)/previsoes$y_menos_12


previsoes$Situacao = paste0("covs_prev",previsoes$covs_prev,"_dessaz",previsoes$dessaz,"_dif",previsoes$dif,"_lags",previsoes$lags)
previsoes$Situacao[1:10] = ifelse(previsoes$dessaz[1:10] == 1,"Observada_1","Observada_0")

previsoes$modelo = factor(previsoes$modelo,levels = c("Observado", "SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS"))



for(situ in unique(previsoes$Situacao)[3:18]){
  dessaz_i = str_replace(str_split(situ,"_",simplify = T)[,3],"dessaz","")
  previsoes_loop = previsoes %>% filter(Situacao %in% c(situ,paste0("Observada_",dessaz_i)))
  
  cores <- c(
    "#000000", 
    "#0072B2",  
    "#009E73",  
    "#56B4E9",  
    "#F0E442",  
    "#CC79A7",  
    "#D55E00",  
    "#8c564b",  
    "#7f7f7f",  
    "#6A0DAD"   
  )
  
  
  g1 = ggplot(previsoes_loop, aes(x = H, y = Variacao_1, color = modelo)) +
    geom_line(size = .5) +
    geom_point(size = 1.0) +
    scale_color_manual(values = cores) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),    
      axis.title = element_text(size = 12)            
    ) +
    labs(
      title = NULL,
      x = "Horizonte (H)",
      y = "Variação do PIB em relação ao mês anterior"
    )
  
  g12 = ggplot(previsoes_loop, aes(x = H, y = Variacao_12, color = modelo)) +
    geom_line(size = .5) +
    geom_point(size = 1.0) +
    scale_color_manual(values = cores) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),    
      axis.title = element_text(size = 12)
    ) +
    labs(
      title = NULL,
      x = "Horizonte (H)",
      y = "Variação do PIB em relação ao mesmo mês em 2024"
    )
  
  ggsave(
    paste0(dir,"/Graficos_Variacoes_FORECASTS/",
           situ,"g1.png"),
    plot = g1, width = 7, height = 5, dpi = 300
  )
  ggsave(
    paste0(dir,"/Graficos_Variacoes_FORECASTS/",
           situ,"g12.png"),
    plot = g12, width = 7, height = 5, dpi = 300
  )
}



par(mfrow = c(1,2))

serie_treino <- bases$y_0$treino  
datas_treino <- seq.Date(from = as.Date("2012-03-01"), 
                         by = "month", 
                         length.out = length(serie_treino))


serie_teste <- bases$y_0$teste
datas_teste <- seq.Date(from = as.Date("2025-01-01"), 
                        by = "month", 
                        length.out = length(serie_teste))


df_treino <- data.frame(data = datas_treino,
                        valor = as.numeric(serie_treino),
                        Dados = "Treino")

df_teste <- data.frame(data = datas_teste,
                       valor = as.numeric(serie_teste),
                       Dados = "Teste")


df_total <- rbind(df_treino, df_teste)
df_total$Dados = factor(df_total$Dados,c("Treino","Teste"))

pib = ggplot(df_total, aes(x = data, y = valor, color = Dados)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "1 year",   
    date_labels = "%Y"       
  )+
  scale_color_manual(values = c("Treino" = "black", "Teste" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),    
    axis.title = element_text(size = 12)
  ) +
  labs(
    title = "PIB",
    x = "Data",
    y = "Valor"
  )

ggsave(
  paste0(dir,"pib.png"),
  plot = pib, width = 7, height = 5, dpi = 300
)





serie_treino <- bases$y_1$treino  
datas_treino <- seq.Date(from = as.Date("2012-03-01"), 
                         by = "month", 
                         length.out = length(serie_treino))


serie_teste <- bases$y_1$teste
datas_teste <- seq.Date(from = as.Date("2025-01-01"), 
                        by = "month", 
                        length.out = length(serie_teste))

df_treino <- data.frame(data = datas_treino,
                        valor = as.numeric(serie_treino),
                        Dados = "Treino")

df_teste <- data.frame(data = datas_teste,
                       valor = as.numeric(serie_teste),
                       Dados = "Teste")


df_total <- rbind(df_treino, df_teste)

df_total$Dados = factor(df_total$Dados,c("Treino","Teste"))

pib_dessaz = ggplot(df_total, aes(x = data, y = valor, color = Dados)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "1 year",   
    date_labels = "%Y"       
  )+
  scale_color_manual(values = c("Treino" = "black", "Teste" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.title = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),    
    axis.title = element_text(size = 12)
  ) +
  labs(
    title = "PIB Dessazonalizado",
    x = "Data",
    y = "Valor"
  )
ggsave(
  paste0(dir,"pib_dessaz.png"),
  plot = pib_dessaz, width = 7, height = 5, dpi = 300
)




erros_wider = erros %>% select(modelo,MAPE,situ) %>% pivot_wider(names_from = "situ",values_from = "MAPE")

tab1 = erros_wider %>% select(c("modelo","2","7")) %>% mutate(dif27 = round(round(`7`,2)-round(`2`,2),2)) %>% mutate(pct = 100*round(1-(round(`7`,2)/round(`2`,2)),4))

print(xtable(tab1,caption = "",align = 'cccccc'),include.rownames = FALSE)


tab2 = erros_wider %>% select(c("modelo","7","5")) %>% mutate(dif27 = round(round(`5`,2)-round(`7`,2),2)) %>% mutate(pct = 100*round(1-(round(`5`,2)/round(`7`,2)),4))

print(xtable(tab2,caption = "",align = 'cccccc'),include.rownames = FALSE)


tab2_ = erros_wider %>% select(c("modelo","3","1")) %>% mutate(dif27 = round(round(`1`,2)-round(`3`,2),2)) %>% mutate(pct = 100*round(1-(round(`1`,2)/round(`3`,2)),4))
print(xtable(tab2_,caption = "",align = 'cccccc'),include.rownames = FALSE)

tab3 = erros_wider %>% select(c("modelo","5","6")) %>% mutate(dif27 = round(round(`6`,2)-round(`5`,2),2)) %>% mutate(pct = 100*round(1-(round(`6`,2)/round(`5`,2)),4))
print(xtable(tab3,caption = "",align = 'cccccc'),include.rownames = FALSE)

tab4 = erros_wider %>% select(c("modelo","5","13")) %>% mutate(dif27 = round(round(`13`,2)-round(`5`,2),2)) %>% mutate(pct = 100*round(1-(round(`13`,2)/round(`5`,2)),4))
print(xtable(tab4,caption = "",align = 'cccccc'),include.rownames = FALSE)


erros$MAPE = round(erros$MAPE,2)
tab5 = erros %>% group_by(modelo) %>% summarise(mean.mape = mean(MAPE),sd.mape = sd(MAPE)) %>% arrange(mean.mape)
tab5$mean.mape = round(tab5$mean.mape,2)
print(xtable(tab4,caption = "",align = 'cccccc'),include.rownames = FALSE)

tab5$tipo_mod = ifelse(tab5$modelo %in% c("SARIMA","ETS","TBATS"),"CLASS","ML")
tab5_ = tab5 %>% group_by(modelo) %>% summarise(MAPEm = round(mean(mean.mape),2)) %>% arrange(MAPEm)
print(xtable(tab5_,caption = "",align = 'ccc'),include.rownames = T)

tab5_2 = tab5 %>% group_by(tipo_mod) %>% summarise(MAPEm = round(mean(mean.mape),2)) %>% arrange(MAPEm)
print(xtable(tab5_2,caption = "",align = 'ccc'),include.rownames = T)






##############################################################################
# Previsao das variacoes anuais do PIB goiano para 2025,  2026, 2027 e 2028 #
##############################################################################

load(paste0(dir,'/Dados/bases.RData'))
load(paste0(dir,"/Dados/covs_prev_estudo3.RData"))

pib = ts(c(bases$y_0$treino,bases$y_0$teste),start = c(2012,03),end = c(2025,05),frequency = 12)
pib_dessaz = ts(c(bases$y_1$treino,bases$y_1$teste),start = c(2012,03),end = c(2025,05),frequency = 12)

covs = covs_prev_estudo3$series_inputs_forecast
colnames(covs) = str_replace_all(colnames(covs),"(\\.ETS|\\.SARIMA|\\.TBATS|\\.seasonaladj)","")

colnames(covs)

# Retirando as variáveis de lag do pib
covs =covs[,26:ncol(covs)]
colnames(covs)

modelos = c("SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS")

xreg_ = window(covs,start = c(2012,3),end = c(2025,5))[,2:ncol(covs)]
newxreg_ = window(covs,start = c(2025,6),end = c(2028,12))[,2:ncol(covs)]

previsoes = data.frame(matrix(NA,nrow=43,ncol=9))
names(previsoes) = modelos

for(modelo in modelos){
  modelo_i_prev = f_modelo(modelo, y = pib_dessaz, h = 43, xreg = xreg_, newxreg = newxreg_, dif_ml=TRUE)
  previsoes[,modelo] = modelo_i_prev$mean 
}


previsoes = ts(previsoes,start = c(2025,6),end = c(2028,12),frequency = 12)

pibs_dessaz = cbind(pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz,pib_dessaz)
colnames(pibs_dessaz) = colnames(previsoes)

p=data.frame(rbind(pibs_dessaz,previsoes))
library(zoo)
p$data = as.character(as.Date(time(ts(NA,start = c(2012,03),end = c(2028,12),frequency = 12))))

prev_pivot = p %>% pivot_longer(names(p)[1:9], names_to = "Modelo",values_to = "Valor")

prev_pivot$ano = str_sub(prev_pivot$data,1,4)

prev_pivot_ = prev_pivot %>% group_by(ano,Modelo) %>% summarise(Valor_ano_acum = sum(Valor))

prev_pivot_$ano = as.numeric(prev_pivot_$ano)
prev_pivot_$ano_anterior = prev_pivot_$ano - 1

prev_pivot_1 = data.frame(prev_pivot_) %>% select(c("ano", "Modelo",  "Valor_ano_acum"))
names(prev_pivot_1) = c("ano", "Modelo",  "Valor_ano_acum_ano_anterior")
prev_pivot_2 = data.frame(prev_pivot_) %>% select(c("ano_anterior", "Modelo",  "Valor_ano_acum")) %>% filter(ano_anterior >= 2012)
names(prev_pivot_2) = c("ano", "Modelo",  "Valor_ano_acum")

prev_pivot_3 = prev_pivot_2 %>% left_join(prev_pivot_1,by=c("ano", "Modelo"))
prev_pivot_3$ano = prev_pivot_3$ano +1


prev_pivot_3$variacao = 100*(prev_pivot_3$Valor_ano_acum - prev_pivot_3$Valor_ano_acum_ano_anterior)/prev_pivot_3$Valor_ano_acum_ano_anterior

variacoes_previstas = prev_pivot_3 %>% filter(ano>=2025)
variacoes_previstas$variacao = round(variacoes_previstas$variacao,2)





pib_var = window(pib,start= c(2017,01),end=c(2024,12))
variacoes_anteriores = data.frame(pib_var)
variacoes_anteriores$data = as.character(as.Date(time(ts(NA,start = c(2017,01),end = c(2024,12),frequency = 12))))

variacoes_anteriores$ano = str_sub(variacoes_anteriores$data,1,4)

variacoes_anteriores = variacoes_anteriores %>% group_by(ano) %>% summarise(Valor_ano_acum = sum(pib_var))

variacoes_anteriores$ano = as.numeric(variacoes_anteriores$ano)
variacoes_anteriores$ano_anterior = variacoes_anteriores$ano - 1

variacoes_anteriores_1 = data.frame(variacoes_anteriores) %>% select(c("ano",  "Valor_ano_acum"))
names(variacoes_anteriores_1) = c("ano",  "Valor_ano_acum_ano_anterior")
variacoes_anteriores_2 = data.frame(variacoes_anteriores) %>% select(c("ano_anterior",  "Valor_ano_acum")) %>% filter(ano_anterior >= 2012)
names(variacoes_anteriores_2) = c("ano",  "Valor_ano_acum")

variacoes_anteriores_3 = variacoes_anteriores_2 %>% left_join(variacoes_anteriores_1,by=c("ano"))
variacoes_anteriores_3$ano = variacoes_anteriores_3$ano + 1


variacoes_anteriores_3$variacao = 100*(variacoes_anteriores_3$Valor_ano_acum - variacoes_anteriores_3$Valor_ano_acum_ano_anterior)/variacoes_anteriores_3$Valor_ano_acum_ano_anterior

variacoes_anteriores_3$variacao = round(variacoes_anteriores_3$variacao,2)

variacoes_anteriores_3$Modelo = ifelse(variacoes_anteriores_3$ano <=2022,"Consolidado","Estimativa")
variacoes_anteriores_3= na.omit(variacoes_anteriores_3)

variacoes_previstas = variacoes_previstas %>% select("ano","Modelo","variacao")
variacoes_anteriores = variacoes_anteriores_3 %>% select("ano","Modelo","variacao")

variacoes_imb = data.frame(ano = c(2025,2026,2027,2028),
                           Modelo = rep("Projeção IMB",4),
                           variacao = c(3.5,2.7,3.0,3.0))

variacoes = rbind(variacoes_anteriores,variacoes_previstas,variacoes_imb)

variacoes$Tipo = ifelse(variacoes$Modelo %in% c("Consolidado","Estimativa"),"Observado","Projetado")

variacoes$Modelo = factor(variacoes$Modelo,levels = c("Projeção IMB", "SARIMA", "ETS", "TBATS", "RF", "XGBOOST", "ELM", "GBM", "PCR", "MARS"))

names(variacoes)
variacoes_wider = variacoes %>% filter(Tipo == "Projetado") %>% select("ano","Modelo","variacao") %>% arrange(Modelo)
variacoes_wider = variacoes_wider %>% pivot_wider(names_from = ano, values_from = variacao)

print(xtable(variacoes_wider,align = "cccccc"),include.rownames = F)
