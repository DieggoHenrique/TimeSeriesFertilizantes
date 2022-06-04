#-------------------------------------------------------------------------------
# MBA EM BIG DATA E ANALYTICS
# PROFESSOR: ALVARO VILLARINHO
# ALUNO: DIEGGO HENRIQUE
# TURMA: 10
#-------------------------------------------------------------------------------
# OVERVIEW----------------------------------------------------------------------
# Dados extraídos da Associação Nacional de Defensivos Agrícolas - ANDA referentes
# ao volume de fertilizantes entregues ao mercado. Série em mil toneladas.
# Período de jan-1998 até out-2021
# http://anda.org.br/wp-content/uploads/2022/01/Principais_Indicadores_2021.pdf
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Limpar o console
rm(list=ls())

#-------------------------------------------------------------------------------
# BIBLIOTECAS UTILIZADAS
#install.packages(c("tidyverse", "readxl", "fpp2", "TSstudio", "tseries",
#                   "BETS"))

library(tidyverse)
library(readxl)
library(fpp2)
library(TSstudio)
library(tseries)
library(BETS)
library(gridExtra)
library(forecast)

#-------------------------------------------------------------------------------
## ROTEIRO

# Modelagem autorregressiva
# Decompor a série
# Efetuar as autocorrelações 
# Modelar 
# Efetuar os testes
# Projetar

# Holt-Winter aditivo e multiplicativo
# Modela pelo auto.arima

# Efetuar as projeções: 
# Pode fazer treino e teste

# Escolha pelo critério de AKAIKe

#-------------------------------------------------------------------------------
# Leitura da Base de dados
demanda <- read_excel("Fertilizantes.xlsx")

# Vizualizando o formato das variaveis
glimpse(demanda)

head(demanda)
tail(demanda)

#Transformando em TS
ts.demanda <- ts(demanda$fertilizantes, 
                 frequency = 12, start = c(1998,1), 
                 end= c(2021,10))

#-------------------------------------------------------------------------------
# ANÁLISE EXPLORATÓRIA DA TIME-SERIE

ggplot2::autoplot(ts.demanda) +
  ggplot2::theme_classic() +
  labs(title = "Demanda por Fertilizantes (Jan-1998 a Out-2021)",
       x = "Período",
       y = "Demanda (ton)",
       caption = "Fonte: ANDA período de jan-1998 até out-2021.") +
  ggplot2::theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# Para utilização da série utilizamos um periodo mais comportado a partir de 2009.


# Boxplot Demanda por fertilizante
boxplot(ts.demanda~cycle(ts.demanda),
        col = rep(c("gold","darkgreen","blue", "red"), each=3),
        xlab="Período", ylab= "Demanda (ton)",
        main ="Demanda por Fertilizantes (Jan-1998 a Out-2021)",
        par(bg="white"))

# Observa-se no boxplot que há uma tendencia de sazonalidade a partir de maio.

# Sazonalidade por fertilizante
ggseasonplot(ts.demanda, year.labels = TRUE, year.label.left = TRUE) +
  ylab("Demanda (ton)") + xlab("Mês") +
  ggtitle("Demanda por Fertilizantes (Jan-1998 a Out-2021)") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))

# Comparando de 1998 a 2021, observa-se uma grande demanda, consultando o 
# calendário agricola, observa-se que entre Novembro e Abril do ano seguinte seja o periodo 
# de colheita, e maio a novembro o periodo de plantio da lavoura (a depender 
# do tipo do Produto Agrícola).

#-------------------------------------------------------------------------------
# Efetuar um corte de janeiro 2009 em diante  para modelar
# window para fazer o corte na series, deixando ela mais comportada.

DemandaCorte <- ts.demanda %>% 
  stats::window(frequency = 12,      # Frequência da `ts`
                start = c(2009,01),  # Início da Time serie
                end = c(2021,10)     # Fim da Time Serie 
                )


#-------------------------------------------------------------------------------
# Rodar Holt-Winter Aditivo e Multiplicativo



# DECOMPOSIÇÃO SERIE TEMPORAL
DecomposeAdditive <- DemandaCorte %>% 
  stats::decompose(type = "additive")

DecomposeMulti <- DemandaCorte %>%
  stats::decompose(type = "multiplicative")

# Plotando os gráficos da decomposição
PlotDecompAdd <- autoplot(DecomposeAdditive, colour = "red")
PlotDecompMult <- autoplot(DecomposeMulti, colour = "blue")

# Vizualizar os dados um ao lado do outro
grid.arrange(PlotDecompAdd, PlotDecompMult, nrow = 1, ncol = 2)


# Tanto na decomposição ativiva quanto na multiplicativa a tendencia da série é 
# Crescente. Pelo comportamento das series, ela possui uma caracteristica mais 
# multiplicativa.

#-------------------------------------------------------------------------------
# MODELO HW ADITIVO: AJUSTE SAZONAL
HoltWinterDemandaAddit <- hw(DemandaCorte, 
                             seasonal = "additive", 
                             h = 24, level = 60)

# Gerando o gráfico
autoplot(DemandaCorte, series = "Original") +
  autolayer(HoltWinterDemandaAddit$fitted, 
            series = "HW Aditivo",  showgap = FALSE) +
  autolayer(HoltWinterDemandaAddit, 
            serie = "Previsão HW Aditivo h = 24",  showgap = FALSE) + 
  ggtitle("Demanda por Fertilizante 2009 a 2021 - Holt-Winters Aditivo") + 
  ylab("Demanda (ton)") + xlab("Período") + 
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "italic"),
        legend.position = "top", legend.title=element_blank() )


#-----------------------------------------
# MODELO HW MULTIPLICATIVO: AJUSTE SAZONAL
HoltWinterDemandaMult <- hw(DemandaCorte, 
                            seasonal = "multiplicative", 
                            h = 24, level = 60)


autoplot(DemandaCorte, series = "Original") +
  autolayer(HoltWinterDemandaMult$fitted, 
            series = "HW Multiplicativo", 
            showgap = FALSE) +
  autolayer(HoltWinterDemandaMult, 
            serie = "Previsão HW Multiplicativo h = 24",
            showgap = FALSE) + 
  ggtitle("Demanda por Fertilizante 2009 a 2021 - Holt-Winter Multiplicativo") + 
  ylab("Demanda (ton)") + xlab("Período") + 
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "italic"),
        legend.position = "top", legend.title=element_blank() )

#----------------------------------------
# Acuracia dos Modelos, quanto menor o erro melhor
# PARAMETROS DE ERRO: HW-ADITIVO
AccuracyAddRMSE <- round(accuracy(HoltWinterDemandaAddit)[,2],2)
AccuracyAddMAPE <- round(accuracy(HoltWinterDemandaAddit)[,5],2)

# PARAMETROS DE ERRO: HW-MULTIPLICATIVO
AccuracyMultRMSE <- round(accuracy(HoltWinterDemandaMult)[,2],2)
AccuracyMultMAPE <- round(accuracy(HoltWinterDemandaMult)[,5],2)


# Gerando os dois modelos em um unico gráfico
autoplot(DemandaCorte, series = "Original") +
  autolayer(HoltWinterDemandaAddit, 
            series = "Previsão HW Aditivo h = 24",  
            showgap = FALSE) +
  autolayer(HoltWinterDemandaMult, 
            serie = "Previsão HW Multiplicativo h = 24",
            showgap = FALSE) + 
  # Titulo dos eixos
  ggtitle("Demanda por Fertilizante 2009 a 2021 (Holt-Winters)") + 
  ylab("Demanda (ton)") + xlab("Período") + 
  # Tema do Gráfico
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "italic"),
        legend.position = "top", legend.title=element_blank() ) + 
  annotate("text", x = 2012, y = 7000, size = 4.1, col = "darkgreen",
           label = paste0("RMSE Additivo = ", AccuracyAddRMSE) ) +
  annotate("text", x = 2012, y = 6800, size = 4.1, col = "blue",
           label = paste0("RMSE Multiplicativo = ", AccuracyMultRMSE) ) +
  annotate("text", x = 2012, y = 6400, size = 4.1,col = "darkgreen",
           label = paste0("MAPE Additivo = ", AccuracyAddMAPE) ) +
  annotate("text", x = 2012, y = 6200, size = 4.1, col = "blue",
           label = paste0("MAPE Multiplicativo = ", AccuracyMultMAPE) )

# O modelo Multiplicativo se mostrou ser o melhor, pelo fato de possuir
# o menor erro!


#-------------------------------------------------------------------------------
# GERANDO AS BASES TREINO E TESTE PARA O MODELO HW
#-------------------------------------------------------------------------------
DemandaTreino <- window(DemandaCorte, end = c(2017,12))
DemandaTeste <- window(DemandaCorte, start = c(2018,1))

# Treinando o modelo Multiplicativo
DemandaTreinoWHM <- hw(DemandaTreino, 
                       seasonal = "multiplicative", 
                       h = 46, 
                       level = 0);DemandaTreinoWHM 

# Para brincar um pouco!
DemandaTreinoWHAdd <- hw(DemandaTreino, 
                         seasonal = "additive", 
                         h = 46, 
                         level = 0);DemandaTreinoWHAdd 

# A acurácia do modelo.
accuracy(DemandaTreinoWHM)
accuracy(DemandaTreinoWHAdd)

autoplot(DemandaTreino)+
  autolayer(DemandaTreinoWHAdd, 
            series = "Previsão HW Aditivo", 
            lwd = 0.5,  showgap = FALSE)+
  autolayer(DemandaTreinoWHM, 
            series = "Previsão HW Multiplicativa", 
            lwd = 0.5,  showgap = FALSE)+
  autolayer(DemandaTeste, 
            serie = "Realizado", lwd = 0.5) + 
  ggtitle("Previsão de Demanda de Fertilizante Holt-Winters (Add vs Mult)")+
  ylab("Treino da Demanda") + xlab("Período") + 
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))








#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# USANDO OS MODELOS ARIMA E AUTO-ARIMA

# Plotando a estacionariedade das séries ACF e PCF
ACFDemanda <- ggAcf(DemandaCorte, lag.max = 100)
PACFDemanda <- ggPacf(DemandaCorte, lag.max = 100)

# Para saber quantos termos utilizar no treino do seu modelo  
gridExtra::grid.arrange(ACFDemanda, PACFDemanda, ncol = 1)

# Plotando a sazonalidade
TSstudio::ts_cor(DemandaCorte) 

#-------------------------------------------------------------------------------
# Rodando o modelo Auto-Arima
AutoArima <- auto.arima(DemandaCorte, 
                        # Possui sazonalidade
                        seasonal = T,
                        # Se TRUE, fará a seleção passo a passo (mais rápido).
                        stepwise = F,
                        # Aprox pela doma dos quadrados 
                        approximation = F);AutoArima

# O Modelo trouxe dois, qual eu uso? 
# ARIMA(1,0,0)(0,1,1)[12]

#-------------------------------------------------------------------------------
# REALIZANDO OS TESTES DE HIPOTESES DO MODELO (ESTACIONARIEDADE)
# p-valor baixo < 0.05 rejeita Ho == Regra de Ouro

#---------------------------------
# TESTE ADF
# Ho. A série não é estacionária
# H1: A serie é estacionaria

adf.test(DemandaCorte)
# Rejeita H0:: p-value = 0.01

#---------------------------------
# Pelo KPSS
# Ho. A série é estacionaria
# H1: A série não é estacionaria

kpss.test(DemandaCorte)
# Rejeita H0:: p-value = 0.01

#---------------------------------
# PHILLP-PERRON
# Ho. A série não é estacionaria
# H1: A serie é estacionaria

pp.test(DemandaCorte)
# Rejeita H0:: p-value = 0.01


#-------------------------------------------------------------------------------
# Realizando a Primeira diferença com a funcão `diff` no lag 2 (base PACF)
g1 <- autoplot(diff(DemandaCorte, 
                    # lag de corte
                    lag = 4, 
                    # numero da diferença
                    differences = 1)) +
  ylab("diff(DemandaCorte)") +
  ggtitle("Demanda por Fertilizante com uma diferença")

g2 <- ggAcf(diff(DemandaCorte, 
                 lag = 4, 
                 differences = 1),
            lag.max = 36) +
  ggtitle("Demanda por Fertilizante com uma diferença")

# Plotando o gráfico
grid.arrange(g1, g2, nrow =2)

# Gerando a correlação da primeira diferença
ts_cor(diff(DemandaCorte, lag = 4, differences = 1))

# Reduzindo a heterocedasticidade
ts.plot(diff(log(DemandaCorte),
             lag = 4,
             differences = 1))



#-------------------------------------------------------------------------------
# Rodando o modelo ARIMA
ArimaModel.1 <- Arima(DemandaCorte, 
                      order = c(1,0,0), 
                      seasonal = c(0,1,1), 
                      method = "ML",
                      # lambda = zero efetua a transforma??o de Box-Cox
                      lambda = 0);ArimaModel.1

# Teste de significância para o modelo ARIMA(1,0,0)(0,1,1)12
t_test(ArimaModel.1)
# Rejeita H0


#-------------------------------------------------------------------------------
# Realizando a Previsão ARIMA.1
PrevisaoArima.1 <- forecast(ArimaModel.1, h = 24, level = 60)

autoplot(DemandaCorte, series = "Original") +
  autolayer(ArimaModel.1$fitted, 
            series = "Arima Modelo 1", showgap = FALSE) +
  autolayer(PrevisaoArima.1, 
            series = "Previsão Arima Modelo 1",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante ARIMA(1,0,0)") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())


#------------------------
# Rodando o modelo ARIMA
ArimaModel.2 <- Arima(DemandaCorte, 
                      order = c(0,1,1), 
                      seasonal = c(0,1,1), 
                      method = "ML",
                      # lambda = zero efetua a transforma??o de Box-Cox
                      lambda = 0);ArimaModel.2

# Teste de significância para o modelo ARIMA(1,0,0)(0,1,1)12
t_test(ArimaModel.2)
# Rejeita H0


# Realizando a Previsão ARIMA.1
PrevisaoArima.2 <- forecast(ArimaModel.2, h = 24, level = 60)

autoplot(DemandaCorte, series = "Original") +
  autolayer(ArimaModel.2$fitted, 
            series = "Arima Modelo 2", showgap = FALSE) +
  autolayer(PrevisaoArima.2, 
            series = "Previsão Arima Modelo 2",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante ARIMA(0,1,1)") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())



# Plotando o Modelo 1 e Modelo 2
autoplot(DemandaCorte, series = "Original") +
  autolayer(PrevisaoArima.1, 
            series = "Previsão Arima Modelo 1", showgap = FALSE) +
  autolayer(PrevisaoArima.2, 
            series = "Previsão Arima Modelo 2",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante ARIMA(1,0,0)(0,1,1)") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())

# O modelo 2 é o melhor
AIC(ArimaModel.1)
AIC(ArimaModel.2)

# Modelo 1 é o melhor



#--------------------------------
# Realizando a Previsão AUTO ARIMA
PrevisaoAutoArima <- forecast(AutoArima, h = 24, level = 60)

autoplot(DemandaCorte, series = "Original") +
  autolayer(PrevisaoAutoArima$fitted, 
            series = "Modelo Auto-Arima", showgap = FALSE) +
  autolayer(PrevisaoAutoArima, 
            series = "Previsão Auto-Arima",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante AUTO-ARIMA (2021 a 2023)") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())



# Vizualização do modelo 1 com o auto arima 


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Comaprando os modelos pelo método de AKAIKE
AICAutoArima <- round(AIC(AutoArima),2)
AICArima <- round(AIC(ArimaModel.2),2)


# Gerando a comparação dos Models
# Modelo Original
autoplot(DemandaCorte, series = "Original") +
  # Previsão do ARIMA
  autolayer(PrevisaoArima.2, serie = "Arima Model.2", showgap = FALSE) +
  # Previsão do AUTO-ARIMA
  autolayer(PrevisaoAutoArima, serie = "Auto-Arima Model", showgap = FALSE) +
  # Resultado de Akaike
  annotate("text", x = 2012, y = 6000, size = 4.1,
           label = paste0("AIC Arima Model = ", AICArima) ) +
  annotate("text", x = 2012, y = 5600, size = 4.1,
           label = paste0("AIC Auto-Arima Model = ", AICAutoArima) ) +
  # Tema do gráfico
  theme_classic() +
  # Plotagem das labels
  ggtitle("Previsão da Demanda por Fertilizante (2021 a 2023)") +
  xlab("Período") + ylab("Demanda (ton)") +
  # Formato das labels (titulo, eixos e legenda) no gráfico
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "italic"),
        # Posicionaodo a legenda e removendo o titulo da legenda
        legend.position = "top", legend.title=element_blank() )
  

#-------------------------------------------------------------------------------
# Avaliando os resíduos do modelo
autoplot(ArimaModel.2$residuals) + theme_minimal() +
  ylab("Resíduo") + xlab("Período") +
  ggtitle("Avaliação dos Resíduos do Arima Modelo 1") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


# Base para Plotar o Histograma no ggplot2
HistPlotArima <- data.frame(ArimaModel.1$residuals)

# calculando a média ddos dados
media <- mean(HistPlotArima$ArimaModel.1.residuals, na.rm = TRUE)
# calculando o desvio padrão
devpad <- sd(HistPlotArima$ArimaModel.1.residuals, na.rm = TRUE) 
# Teste de shapiro-wilk
shapiro <- shapiro.test(HistPlotArima$ArimaModel.1.residuals)
# p-valor do teste de shapiro
p_valor.shapiro <- round(shapiro$p.value, 2)
# Estatistica do teste
estatis.shapiro <- round(shapiro$statistic, 2)


#-------------------------------------------
# definindo o conjunto de dados e a variável
ggplot(HistPlotArima, aes(x=ArimaModel.1.residuals)) + 
  # E plotando o histograma com densidade
  geom_histogram(aes(y = ..density..),
                 # Definindo a cor de preenchimento
                 bins = 30, fill = "cyan4",
                 # Definindo a cor do contorno
                 col = alpha("red",.3)) +
  # gerando a linha da distribuição
  geom_function(fun = dnorm, args = list(mean = media, sd = devpad),
                col = "tomato", lwd = 1.5, lty = 4) +
  # labels do gráfico
  labs(title = "Teste de Normalidade dos Resíduos",
       x = "Resíduo da Demanda de Fertilizante",
       y = "Densidade de probabilidade",
       caption = "Fonte: ANDA período de jan-1998 até out-2021.") +
  # Inclusão do teste Shapiro-Wilk e o seu p-valor
  annotate("text", x = 0.5, y = 6, size = 4.1,
           label = paste0("Shapiro-Wilk = ", estatis.shapiro)) +
  annotate("text", x = 0.5, y = 5.8, size = 4.1,
           label = paste0("p-value = ", p_valor.shapiro)) + 
  # Definindo o tema do gráfico
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))








#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPARAÇAO ENTRE HW MULTIPLICATIVE E ARIMA

ArimaTreino <- window(DemandaCorte, end = c(2017,12))


# Rodando o modelo ARIMA
ArimaModelTreino <- Arima(ArimaTreino, 
                          order = c(0,1,1), 
                          seasonal = c(0,1,1), 
                          method = "ML",
                          # lambda = zero efetua a transforma??o de Box-Cox
                          lambda = 0);ArimaModelTreino

# Previsão do modelo ate 2021.10
PrevisaoArimaTreino <- forecast(ArimaModelTreino, 
                                    h = 46, level = 0)
PrevisaoArimaTreino

AIC(ArimaModelTreino)

# ----------------------------
# Grafico comparativo
autoplot(DemandaTreino, col = "Black")+
  autolayer(PrevisaoArimaTreino, 
            series = "Previsão ARIMA", showgap = FALSE)+
  autolayer(DemandaTreinoWHM, 
            series = "Previsão Holt-Winters Multi",  showgap = FALSE)+
  autolayer(DemandaTeste, 
            serie = "Realizado") + 
  ggtitle("Previsão da Demanda de Fertilizante \n ARIMA vs Holt-Winters Multiplicative (2018 a 2021)")+
  ylab("Treino da Demanda (ton)") + xlab("Período") + 
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# Pelo gráfico o modelo que mais se aproximou da realidade (linha azul) foi o 
# modelo de Holt-Winter Multiplicative (linha verde).

# END


