par(mfrow = c(1,2))
seasonplot(DemandaTreino, ylim=c(0,5000))
abline(h = 2000, lwd = 3, col = "red")
seasonplot(DemandaTeste, ylim=c(0,5000))
abline(h = 2000, lwd = 3, col = "red")
abline(v = 4, lwd = 2, col = "blue", lty = 2)


autoplot(DemandaCorte) +
  autolayer(PrevisaoAutoArima, lwd = 1, showgap = FALSE) +
  ggtitle("Previsão de Demanda de Fertilizante  (Auto-Arima) \n (Out-2021 a Out-2025)")+
  ylab("Demanda (ton)") + xlab("Período") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank(),
        axis.title = element_text(size = 9, face = "italic"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
