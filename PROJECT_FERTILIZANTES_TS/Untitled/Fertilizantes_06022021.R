#-------------------------------------------------------------------------------
# MBA EM BIG DATA E ANALYTICS
# PROFESSOR: ALVARO VILLARINHO
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

AutoArimaModel <- as.ts(AutoArima)

autoplot(DemandaCorte, series = "Original") +
  autolayer(Previsao, series = "Previsão Auto Arima") +
# ajustar esse parametro
  autolayer(AutoArimaModel$model, series = "Model")

# Comparação entre os dois modelos HW Multiplicativo e
# e o Aruto Arima.
AIC(DemandaTreinoWHM$model)
AIC(AutoArima)


#---------------------------xxxx--------------------------------
