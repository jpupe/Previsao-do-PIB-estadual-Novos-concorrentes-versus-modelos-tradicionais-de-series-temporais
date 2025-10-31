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

# No Rstudio, no menu de opções, ir em: 'Session' -> 'Set Working Directory' e clicar em 'The Source File Location' 
dir = getwd()
dir = paste0(dir,"/Dados/")
setwd(dir)


for(i in 1:sum(str_detect(list.files(),'.RData'))){
  tryCatch({
    load(list.files()[str_detect(list.files(),'.RData')][i])
  },error = function(cond) {
    print(str_detect(list.files(),'.RData')[i])
  })
}


names_inflation <- c(
  "anbima12_tjtln112", "anbima12_tjtln1212", "anbima12_tjtln312", "anbima12_tjtln612",
  "bm12_cca12", "bm12_ccapf12", "bm12_ccapj12", "bm12_cin12", "bm12_cinpf12", "bm12_cinpj12",
  "bm12_crdcapf12", "bm12_crdcapj12", "bm12_crlcapf12", "bm12_crlcapj12", "bm12_cs12",
  "bm12_ctj12", "bm12_ctjpf12", "bm12_dtspy12", "bm12_erc12", "bm12_ercf12",
  "bm12_erv12", "bm12_ervf12", "bm12_ipcaexp1212", "bm12_ipcaexp612", "bm12_m0mn12",
  "bm12_m0n12", "bm12_m0ny12", "bm12_nfspjnys12", "bm12_nfspnnas12", "bm12_nfspnys12",
  "bm12_nfsppnas12", "bm12_nfsppns12", "bm12_nfsppys12", "bm12_tjcdi12", "bm12_tjlp12",
  "bm12_tjover12", "bm12_tjtr12", "bpag12_bcm12", "bpag12_bcx12", "ce12_cutind12",
  "cni12_htrab12", "cni12_iceica12", "cni12_iceiexp12", "cni12_iceiger12", "cni12_inde12",
  "cni12_nucap12", "cni12_peemp12", "cni12_remn12", "cni12_remr12", "cni12_rendr12",
  "cni12_venrea12", "deral12_atarpo12", "deral12_atbcad12", "deral12_atbcat12", "deral12_atfec12",
  "deral12_atfep12", "deral12_atfrr12", "deral12_atfso12", "deral12_atftrc12", "deral12_atmc12",
  "deral12_atolr12", "deral12_atove12", "deral12_atovg12", "deral12_atovm12", "deral12_atscar12",
  "deral12_atsulo12", "deral12_atsupa12", "deral12_atsupe12", "deral12_prbgo12", "deral12_prcan12",
  "deral12_prcco12", "deral12_prfrv12", "deral12_prleco12", "deral12_prmi12", "deral12_prso12",
  "deral12_prsuc12", "deral12_prtrg12", "eletro12_ceeco12", "eletro12_ceecom12", "eletro12_ceeind12",
  "eletro12_ceene12", "eletro12_ceeno12", "eletro12_ceeout12", "eletro12_ceeres12", "eletro12_ceese12",
  "eletro12_ceesu12", "eletro12_ceet12", "eletro12_ceetcom12", "eletro12_ceetind12", "eletro12_ceetres12",
  "eletro12_ceett12", "fcesp12_iic12", "fcesp12_iica12", "fcesp12_iicf12", "fipe12_venbr12",
  "gac12_salminre12", "gac12_tcerxm12", "gac12_tcerxminpc12", "gac12_tcerxt12", "gac12_tcerxtinpc12",
  "ons12_conv12", "ons12_hidr12", "pan12_tjover12", "pmc12_ivvrn12", "pmc12_ivvrnamp12",
  "pmc12_vrconstn12", "pmc12_vreletrn12", "pmc12_vrfarmn12", "pmc12_vrsupn12", "pmc12_vrsuptn12",
  "pmc12_vrveicn12", "pmc12_vrvestn12", "pnadc12_espub12", "pnadc12_espubcc12", "pnadc12_espubmil12",
  "pnadc12_espubsc12", "pnadc12_mrrte12", "pnadc12_mrrth12", "pnadc12_mrtem12", "pnadc12_mrthm12",
  "pnadc12_nocup12", "pnadc12_nocupm12", "sgs12_ibcbr12"
)

covs_consideradas = colnames(covariates_ts)[!(colnames(covariates_ts) %in% names_inflation)]
covariates_ts = covariates_ts[,covs_consideradas]

ym_to_vec = function(date){return(as.numeric(stringr::str_split(as.character(date),'-',simplify = T)[,1:2]))}
vec_to_ym = function(ym){return(lubridate::ym(paste0(ym,collapse = "-")))}

covariates_ts = na.omit(covariates_ts) 


y = responses_remove_seas_forecast_ts[,'pib']
y = na.omit(y)

base_lag = y
for(i in 1:12){
  base_lag = cbind(base_lag,stats::lag(y,-i))
}
colnames(base_lag) <- paste0("lag_", 0:12)
base_lag=base_lag[,-1]

base_ma = y
for(i in 1:13){
  ma_i = zoo::rollmean(y,i)
  ma_i = c(rep(NA,i-1),ma_i)
  base_ma = cbind(base_ma,ma_i)
}
colnames(base_ma) <- paste0("ma_", -1:12)
base_ma=base_ma[,-1]
base_ma=base_ma[,-1]

ys = cbind(base_lag,base_ma)[1:length(y),]
ys = ts(ys,start = start(y),frequency = 12)
ys = na.omit(ys)

start(ys)
end(ys)

colnames(ys) = str_replace_all(colnames(ys),'(base_lag\\.|base_ma\\.)','')



pib = window(ys,start=start(covariates_ts),end=end(covariates_ts))
covariates_ts_observadas = cbind(pib,covariates_ts)
colnames(covariates_ts_observadas) = str_replace_all(colnames(covariates_ts_observadas),'covariates_ts\\.','')



########################################################################################################################
series1 = window(covariates_ts_observadas,start = start(covariates_ts_observadas),end = c(2025,05))
H_prev = c(2025,05)
h_cv=18
covs_2025observado_semDessaz = series1
colnames(covs_2025observado_semDessaz) = str_replace_all(colnames(covs_2025observado_semDessaz),"(\\.ETS|\\.SARIMA|\\.TBATS|\\.seasonaladj)","")
save(covs_2025observado_semDessaz, file = paste0(dir,"covs_2025observado_semDessaz.RData"))

########################################################################################################################
series1 = window(covariates_ts_observadas,start = start(covariates_ts_observadas),end = c(2025,05))
covs_2025observado_Dessaz = series1

for(k in 1:ncol(series1)){
  
  cov_i = na.omit(series1[,colnames(series1)[k]])
  cov_i_name = colnames(series1)[k]
  tryCatch({
    cov_i = seas(cov_i)$data[,c('seasonaladj')]
    cov_i_name = paste0(cov_i_name,'.seasonaladj')
  },
  error = function(cond) {
    cov_i = cov_i
    cov_i_name = cov_i_name
  })
  covs_2025observado_Dessaz= cbind(covs_2025observado_Dessaz, cov_i)
  colnames(covs_2025observado_Dessaz) = str_replace_all(colnames(covs_2025observado_Dessaz),"covs_2025observado_Dessaz\\.","")
  colnames(covs_2025observado_Dessaz)[ncol(covs_2025observado_Dessaz)] = cov_i_name
  print(paste(k,'/',ncol(series1)))
}
covs_2025observado_Dessaz = covs_2025observado_Dessaz[,(ncol(series1)+1):ncol(covs_2025observado_Dessaz)]
colnames(covs_2025observado_Dessaz) = str_replace_all(colnames(covs_2025observado_Dessaz),"(\\.ETS|\\.SARIMA|\\.TBATS|\\.seasonaladj)","")
save(covs_2025observado_Dessaz, file = paste0(dir,"covs_2025observado_Dessaz.RData"))





########################################################################################################################
########################################## Previsões das covariáveis ###################################################
########################################################################################################################

# Função para a previsao das covariaveis
forecast_cv_arima_ets_tbats =  function(series,H_prev,h_cv,x13.seas = FALSE,wind_len=100){
  
  ym_to_vec = function(date){return(as.numeric(stringr::str_split(as.character(date),'-',simplify = T)[,1:2]))}
  vec_to_ym = function(ym){return(lubridate::ym(paste0(ym,collapse = "-")))}
  
  f_arima = function(y,h,xreg=NULL,newxreg=NULL){
    return(forecast(auto.arima(y),h=h))
  }
  
  f_ets <- function(y, h){
    return(forecast(ets(y), h))
  }
  
  f_tbats = function(y,h){
    return(forecast(tbats(y),h=h))
  }
  
  f_modelo = function(modelo, y, h, xreg = NULL, newxreg = NULL,dif_ml=FALSE){
    modelo = tolower(modelo)
    if(modelo == "sarima"){
      return(f_arima(y, h))
    } else if(modelo == "ets"){
      return(f_ets(y, h))
    } else if(modelo == "tbats"){
      return(f_tbats(y, h))
    } else {
      stop("Modelo não reconhecido.")
    }
  }
  swcv_pinned = function(modelo,y,h,window_,initial,xreg = NULL,dif_ml=FALSE){
    n <- length(y)
    Hor.MAEs <- c()
    Hors = c()
    for (i in 1:(h)) {
      mes_zero= vec_to_ym(start(y)) %m-% months(1)
      start_treino = mes_zero %m+% months(initial+i)
      end_treino = start_treino %m+% months(window_)
      dados_treinoi <- window(y,start = ym_to_vec(start_treino),end = ym_to_vec(end_treino)) 
      dados_testei <- window(y,start = ym_to_vec(end_treino %m+% months(1)),end = ym_to_vec(end_treino %m+% months(h)) )
      ]predicoes_modelo <- f_modelo(modelo, dados_treinoi, h, xreg = covs_treinoi, newxreg = covs_testei,dif_ml)$mean
      
      Hor.MAEs[i] <- mean(abs(dados_testei - predicoes_modelo))
      print(paste('H',i,'previsto com treino de',start_treino,'a',end_treino,'e teste de',end_treino %m+% months(1),'a',end_treino %m+% months(h)))
    }
    Hors = c(1:h)
    return(data.frame(H = Hors,MAEs = Hor.MAEs))
  }
  
  base_final_covs = ts(NA,start = start(series),end = H_prev,frequency = 12)
  base_inicial_covs =  ts(NA,start = start(series),end = end(series),frequency = 12)
  modelo_vencedor = character()
  
  for(k in 1:ncol(series)){
    tempo_inicial = Sys.time()
    cov_i = na.omit(series[,colnames(series)[k]])
    cov_i_name = colnames(series)[k]
    if(x13.seas == TRUE){
      tryCatch({
        cov_i = seas(cov_i)$data[,c('seasonaladj')]
        cov_i_name = paste0(cov_i_name,'.seasonaladj')
      },
      error = function(cond) {
        cov_i = cov_i
        cov_i_name = cov_i_name
      })
    }
    
    iniciar_em = length(cov_i)-2*h_cv-wind_len
    if(iniciar_em<=0)(iniciar_em = 1)
    cv_sarima = swcv_pinned(y = cov_i,modelo = 'sarima',h=h_cv,window_ = wind_len,initial = iniciar_em,xreg = NULL,dif_ml=FALSE)
    cv_ets = swcv_pinned(y = cov_i,modelo = 'ets',h=h_cv,window_ = wind_len,initial = iniciar_em,xreg = NULL,dif_ml=FALSE)
    cv_tbats = swcv_pinned(y = cov_i,modelo = 'tbats',h=h_cv,window_ = wind_len,initial = iniciar_em,xreg = NULL,dif_ml=FALSE)
    cv = cbind(cv_sarima,cv_ets,cv_tbats)
    nomes = c(names(cv)=="MAEs")
    nomes[1]=TRUE
    cv = cv[,nomes]
    names(cv)=c("Horizonte","ARIMA","ETS","BATS")
    
    winner = c()
    for(i in 1:nrow(cv)){
      if(min(cv$ARIMA[i],cv$ETS[i],cv$BATS[i]) == cv$ARIMA[i]){winner[i]='SARIMA'}
      if(min(cv$ARIMA[i],cv$ETS[i],cv$BATS[i]) == cv$ETS[i]){winner[i]='ETS'}
      if(min(cv$ARIMA[i],cv$ETS[i],cv$BATS[i]) == cv$BATS[i]){winner[i]='TBATS'}
    }
    
    winner = data.frame(table(winner)) %>% arrange(desc(Freq))
    placar=winner
    winner = winner$winner[1]
    
    H.necessarios = round(as.numeric(vec_to_ym(H_prev) - vec_to_ym(end(cov_i)))/30,0)
    
    if(winner == 'SARIMA'){previsoes = forecast(auto.arima(cov_i),h=H.necessarios)$mean}
    if(winner == 'ETS'){previsoes = forecast(ets(cov_i),h=H.necessarios)$mean}
    if(winner == 'TBATS'){previsoes = forecast(tbats(cov_i),h=H.necessarios)$mean}
    
    cov_i_final = ts(append(cov_i,previsoes),start = start(cov_i),end = end(previsoes),frequency = 12)
    
    
    base_final_covs = cbind(base_final_covs,cov_i_final)
    colnames(base_final_covs)[k+1] = paste0(cov_i_name,'.',winner)
    
    base_inicial_covs = cbind(base_inicial_covs,cov_i)
    colnames(base_inicial_covs)[k+1] = cov_i_name
    
    modelo_vencedor[k] = as.character(placar[1,1])
    print(paste("A covariavel",cov_i_name,"Teve",H.necessarios,"Horizontes previstos pela funcao",winner,'que venceu pelo placar de: '))
    print(placar)
    tempo_final=Sys.time()
    print(tempo_final)
    tempo = tempo_final - tempo_inicial
    print(tempo)
    save(base_final_covs,file = 'base_final_covs.RData')
    
  }
  
  
  colnames(base_final_covs) = str_replace_all(colnames(base_final_covs),'base_final_covs.','')
  
  
  return(
    list(
      series_inputs = series,
      model_winners = modelo_vencedor,
      series_inputs_forecast = base_final_covs
    )
  )
  
  
}


########################################################################################################################
series1 = window(covariates_ts_observadas,start = start(covariates_ts_observadas),end = c(2024,12))
H_prev = c(2025,05)
h_cv=18
covs_2025previsto_semDessaz = forecast_cv_arima_ets_tbats(series = series1,
                                                          H_prev = H_prev,
                                                          h_cv = h_cv,
                                                          x13.seas = FALSE,
                                                          wind_len = 100)
covs_2025previsto_semDessaz = covs_2025previsto_semDessaz$series_inputs_forecast
colnames(covs_2025previsto_semDessaz) = str_replace_all(colnames(covs_2025previsto_semDessaz),"(\\.ETS|\\.SARIMA|\\.TBATS|\\.seasonaladj)","")
save(covs_2025previsto_semDessaz, file = paste0(dir,"\covs_2025previsto_semDessaz.RData"))

########################################################################################################################
series1 = window(covariates_ts_observadas,start = start(covariates_ts_observadas),end = c(2024,12))
H_prev = c(2025,05)
h_cv=18
covs_2025previsto_Dessaz = forecast_cv_arima_ets_tbats(series = series1,
                                                       H_prev = H_prev,
                                                       h_cv = h_cv,
                                                       x13.seas = TRUE,
                                                       wind_len = 100)
covs_2025previsto_Dessaz = covs_2025previsto_Dessaz$series_inputs_forecast
colnames(covs_2025previsto_Dessaz) = str_replace_all(colnames(covs_2025previsto_Dessaz),"(\\.ETS|\\.SARIMA|\\.TBATS|\\.seasonaladj)","")
save(covs_2025previsto_Dessaz, file = paste0(dir,"covs_2025previsto_Dessaz.RData"))

########################################################################################################################

pib = na.omit(responses_remove_seas_forecast_ts[,c('pib')])
pib_dessaz = seas(pib)$data[,'seasonaladj']
ys = cbind(pib,pib_dessaz)
save(ys, file = paste0(dir,"ys.RData"))





################ Organizar todas as situacoes em uma só lista de bases


rm(list=ls())



# No Rstudio, no menu de opções, ir em: 'Session' -> 'Set Working Directory' e clicar em 'The Source File Location' 
dir = getwd()
dir = paste0(dir,'/Dados/')
setwd(dir)


for(i in 1:sum(str_detect(list.files(),'.RData'))){
  tryCatch({
    load(list.files()[str_detect(list.files(),'.RData')][i])
  },error = function(cond) {
    print(str_detect(list.files(),'.RData')[i])
  })
}

bases = list()

### 1 codigo separado por underline:
## 0 para pib sazonal 1 para dessazonalizado por x13arimaseats

y_0 = list()
y = ys[,'pib']
y_0$treino = window(y,c(2012,3),c(2024,12))
y_0$teste = window(y,c(2025,1),c(2025,5))
bases$y_0 = y_0

y_1 = list()
y = ys[,'pib_dessaz']
y_1$treino = window(y,c(2012,3),c(2024,12))
y_1$teste = window(y,c(2025,1),c(2025,5))
bases$y_1 = y_1

### 3 codigos separados por underline:
## Primeiro código: 0 para observadas 1 para previstas
## Segundo código: 0 para sazonais 1 para dessazonalizadas
## Terceiro código: 0 para sem lags 1 para com lags


covs_0_0_0 = list()
series = covs_2025observado_semDessaz[,colnames(covs_2025observado_semDessaz)[!str_detect(colnames(covs_2025observado_semDessaz),'(base_final_covs|pib\\.lag_|pib\\.ma_)')]]
covs_0_0_0$treino = window(series,c(2012,3),c(2024,12))
covs_0_0_0$teste = window(series,c(2025,1),c(2025,5))
bases$covs_0_0_0 = covs_0_0_0

covs_1_0_0 = list()
series = covs_2025previsto_semDessaz[,colnames(covs_2025previsto_semDessaz)[!str_detect(colnames(covs_2025previsto_semDessaz),'(base_final_covs|pib\\.lag_|pib\\.ma_)')]]
covs_1_0_0$treino = window(series,c(2012,3),c(2024,12))
covs_1_0_0$teste = window(series,c(2025,1),c(2025,5))
bases$covs_1_0_0 = covs_1_0_0

covs_1_1_0 = list()
series = covs_2025previsto_Dessaz[,colnames(covs_2025previsto_Dessaz)[!str_detect(colnames(covs_2025previsto_Dessaz),'(base_final_covs|pib\\.lag_|pib\\.ma_)')]]
covs_1_1_0$treino = window(series,c(2012,3),c(2024,12))
covs_1_1_0$teste = window(series,c(2025,1),c(2025,5))
bases$covs_1_1_0 = covs_1_1_0

covs_0_1_0 = list()
series = covs_2025observado_Dessaz[,colnames(covs_2025observado_Dessaz)[!str_detect(colnames(covs_2025observado_Dessaz),'(base_final_covs|pib\\.lag_|pib\\.ma_)')]]
covs_0_1_0$treino = window(series,c(2012,3),c(2024,12))
covs_0_1_0$teste = window(series,c(2025,1),c(2025,5))
bases$covs_0_1_0 = covs_0_1_0

covs_1_1_1 = list()
series = covs_2025previsto_Dessaz[,colnames(covs_2025previsto_Dessaz)[!str_detect(colnames(covs_2025previsto_Dessaz),'base_final_covs')]]
covs_1_1_1$treino = window(series,c(2012,3),c(2024,12))
covs_1_1_1$teste = window(series,c(2025,1),c(2025,5))
bases$covs_1_1_1 = covs_1_1_1

covs_0_0_1 = list()
series = covs_2025observado_semDessaz[,colnames(covs_2025observado_semDessaz)[!str_detect(colnames(covs_2025observado_semDessaz),'base_final_covs')]]
covs_0_0_1$treino = window(series,c(2012,3),c(2024,12))
covs_0_0_1$teste = window(series,c(2025,1),c(2025,5))
bases$covs_0_0_1 = covs_0_0_1

covs_0_1_1 = list()
series = covs_2025observado_Dessaz[,colnames(covs_2025observado_Dessaz)[!str_detect(colnames(covs_2025observado_Dessaz),'base_final_covs')]]
covs_0_1_1$treino = window(series,c(2012,3),c(2024,12))
covs_0_1_1$teste = window(series,c(2025,1),c(2025,5))
bases$covs_0_1_1 = covs_0_1_1

covs_1_0_1 = list()
series = covs_2025previsto_semDessaz[,colnames(covs_2025previsto_semDessaz)[!str_detect(colnames(covs_2025previsto_semDessaz),'base_final_covs')]]
covs_1_0_1$treino = window(series,c(2012,3),c(2024,12))
covs_1_0_1$teste = window(series,c(2025,1),c(2025,5))
bases$covs_1_0_1 = covs_1_0_1


save(bases, file = paste0(dir,"bases.RData"))


load(paste0(dir,"bases.RData"))

covs = ts(rbind(bases$covs_0_0_1$treino,bases$covs_0_0_1$teste),start = c(2012,03),end = c(2025,05),frequency = 12)

covs_prev = forecast_cv_arima_ets_tbats(series = covs,
                            H_prev = c(2028,12),
                            h_cv = 18,
                            x13.seas = TRUE,
                            wind_len = 100)

covs_prev_estudo3 = covs_prev

save(covs_prev_estudo3,file = paste0(dir,"covs_prev_estudo3.RData"))





