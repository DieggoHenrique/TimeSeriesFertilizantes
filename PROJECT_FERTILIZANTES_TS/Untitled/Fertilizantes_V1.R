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

autoplot(ts.demanda) +
  theme_classic() +
  labs(title = "Demanda por Fertilizantes (Jan-1998 a Out-2021)",
       x = "Período",
       y = "Demanda (ton)",
       caption = "Fonte: ANDA período de jan-1998 até out-2021.") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# Para utilização da serie utilzamos um periodo mais comportado a partir de 2009.


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
DemandaCorte <- window(ts.demanda,      # Série no formato `ts`
                       # Frequancia mensal = 12
                       frequency = 12,
                       # Inicio do corte jan 2009
                       start = c(2009, 01),
                       # Final da Série nov 2021
                       end = c(2021,10)
                       )


#-------------------------------------------------------------------------------
# Rodar Holt-Winter Aditivo e Multiplicativo

# DECOMPOSIÇÃO SERIE TEMPORAL
DecomposeAdditive <- decompose(DemandaCorte, type = "additive")
DecomposeMulti <- decompose(DemandaCorte, type = "multiplicative")

# Plotando os gráficos da decomposição
PlotDecompAdd <- autoplot(DecomposeAdditive, colour = "red")
PlotDecompMult <- autoplot(DecomposeMulti, colour = "blue")
grid.arrange(PlotDecompAdd, PlotDecompMult, nrow = 1, ncol = 2)


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

# Acuracia dos Modelos, quanto menor o erro melhor
AccuracyAddRMSE <- round(accuracy(HoltWinterDemandaAddit)[,2],2)
AccuracyAddMAPE <- round(accuracy(HoltWinterDemandaAddit)[,5],2)

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
  ggtitle("Demanda por Fertilizante 2009 a 2021 (Holt-Winters)") +
  ylab("Demanda (ton)") + xlab("Período") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "italic"),
        legend.position = "top", legend.title=element_blank() ) +
  annotate("text", x = 2012, y = 7000, size = 4.1,
           label = paste0("RMSE Additivo = ", AccuracyAddRMSE) ) +
  annotate("text", x = 2012, y = 6800, size = 4.1, col = "blue",
           label = paste0("RMSE Multiplicativo = ", AccuracyMultRMSE) ) +
  annotate("text", x = 2012, y = 6400, size = 4.1,
           label = paste0("MAPE Additivo = ", AccuracyAddMAPE) ) +
  annotate("text", x = 2012, y = 6200, size = 4.1, col = "blue",
           label = paste0("MAPE Multiplicativo = ", AccuracyMultMAPE) )

# O modelo Multiplicativo é o melhor, pois possui o menor erro!


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
            lwd = 1.5,  showgap = FALSE)+
  autolayer(DemandaTreinoWHM,
            series = "Previsão HW Multiplicativa",
            lwd = 1.5,  showgap = FALSE)+
  autolayer(DemandaTeste,
            serie = "Realizado", lwd = 1.5) +
  ggtitle("Previsão de Demanda de Fertilizante Holt-Winters (Add vs Mult)")+
  ylab("Treino da Demanda") + xlab("Período") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


# trasformar a serie em df
# Transformar a previsão do modelo em df
# concatenar
df <- data.frame(cbind(DemandaTeste, DemandaTreinoWHM))
df

df$erro <- df$DemandaTeste - df$DemandaTreinoWHM.Point.Forecast


df$data <- seq.Date(as.Date("2018-01-31"),
                    length.out = 46, by = "month")

glimpse(df)

ggplot(df, aes(data, erro) ) +
  geom_line()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# USANDO OS MODELOS ARIMA E SARIMA

# Plotando a estacionariedade das séries ACF e PCF
ACFDemanda <- ggAcf(DemandaCorte, lag.max = 60)
PACFDemanda <- ggPacf(DemandaCorte, lag.max = 60)
gridExtra::grid.arrange(ACFDemanda, PACFDemanda, nrow = 1)

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
# ARIMA(1,0,1)(0,1,1)[12]

Previsao <- forecast(AutoArima, h = 46, level = c(60,80))

autoplot(DemandaCorte, series = "Original") +
  autolayer(Previsao, series = "Previsão Auto Arima") +
  autolayer(AutoArima$model, series = "Model")


#---------------------------xxxx--------------------------------









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
  ylab("diff(DemandaCorte)")+
  ggtitle("Demanda por Fertilizante com uma diferença")

g2 <- ggAcf(diff(DemandaCorte, lag = 4, differences = 1),
            lag.max = 36)+
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
ArimaModel <- Arima(DemandaCorte,
                    order = c(1,0,0),
                    seasonal = c(0,1,1),
                    method = "ML",
                    # lambda = zero efetua a transforma??o de Box-Cox
                    lambda = 0);ArimaModel

# Teste de significância para o modelo ARIMA(1,0,0)(0,1,1)12
t_test(ArimaModel)
# Rejeita H0

#-------------------------------------------------------------------------------
# Realizando a Previsão SARIMA
PrevisaoAutoArima <- forecast(AutoArima, h = 24, level = 60)

autoplot(DemandaCorte, series = "Original") +
  autolayer(PrevisaoAutoArima$fitted,
            series = "Modelo Auto-Arima", showgap = FALSE) +
  autolayer(PrevisaoAutoArima,
            series = "Previsão Auto-Arima",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante AUTO-ARIMA") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())

#-------------------------------------------------------------------------------
# Realizando a previsão ARIMA
PrevisaoArima <- forecast(ArimaModel, h = 24, level = 60)

autoplot(DemandaCorte, series = "Original") +
  autolayer(PrevisaoArima$fitted, series = "Modelo Arima",  showgap = FALSE) +
  autolayer(PrevisaoArima, series = "Previsão Arima",  showgap = FALSE) +
  ggtitle("Previsão da Demanda de Fertilizante ARIMA (2021 a 2023)") +
  xlab("Período") + ylab("Demanda (ton)") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "top", legend.title = element_blank())

#-------------------------------------------------------------------------------
# Comaprando os modelos pelo método de AKAIKE
AICAutoArima <- round(AIC(AutoArima),2)
AICArima <- round(AIC(ArimaModel),2)


# Gerando a comparação dos Models
# Modelo Original
autoplot(DemandaCorte, series = "Original") +
  # Previsão do ARIMA
  autolayer(PrevisaoArima, serie = "Arima Model", showgap = FALSE) +
  # Previsão do AUTO-ARIMA
  autolayer(PrevisaoAutoArima, serie = "Auto-Arima Model", showgap = FALSE) +
  # Resultado de Akaike
  annotate("text", x = 2012, y = 5000, size = 4.1,
           label = paste0("AIC Arima Model = ", AICArima) ) +
  annotate("text", x = 2012, y = 4800, size = 4.1,
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
autoplot(ArimaModel$residuals) + theme_classic() +
  ylab("Resíduo") + xlab("Período") +
  ggtitle("Avaliação dos Resíduos do Modelo Arima") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


# Base para Plotar o Histograma no ggplot2
HistPlotArima <- data.frame(ArimaModel$residuals)

# calculando a média ddos dados
media <- mean(HistPlotArima$ArimaModel.residuals, na.rm = TRUE)
# calculando o desvio padrão
devpad <- sd(HistPlotArima$ArimaModel.residuals, na.rm = TRUE)
# Teste de shapiro-wilk
shapiro <- shapiro.test(HistPlotArima$ArimaModel.residuals)
# p-valor do teste de shapiro
p_valor.shapiro <- round(shapiro$p.value, 2)
# Estatistica do teste
estatis.shapiro <- round(shapiro$statistic, 2)


#-------------------------------------------
# definindo o conjunto de dados e a variável
ggplot(HistPlotArima, aes(x=ArimaModel.residuals)) +
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
                          order = c(1,0,0),
                          seasonal = c(0,1,1),
                          method = "ML",
                          # lambda = zero efetua a transforma??o de Box-Cox
                          lambda = 0);ArimaModelTreino

# Previsão do modelo ate 2021.10
PrevisaoAutoArimaTreino <- forecast(ArimaModelTreino,
                                    h = 46, level = 0);PrevisaoAutoArimaTreino


# ----------------------------
# Grafico comparativo
autoplot(DemandaTreino)+
  autolayer(PrevisaoAutoArimaTreino,
            series = "Previsão ARIMA", lwd = 1.5,  showgap = FALSE)+
  autolayer(DemandaTreinoWHM,
            series = "Previsão Holt-Winters Multi", lwd = 1.0,  showgap = FALSE)+
  autolayer(DemandaTeste,
            serie = "Realizado", lwd = 1.5) +
  ggtitle("Previsão da Demanda de Fertilizante \n ARIMA vs Holt-Winters Multiplicative (2018 a 2021)")+
  ylab("Treino da Demanda (ton)") + xlab("Período") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# Pelo gráfico o modelo que mais se aproximou da realidade (linha azul) foi o
# modelo de Holt-Winter Multiplicative (linha verde).

# END


