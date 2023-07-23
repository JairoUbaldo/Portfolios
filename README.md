# Portfolios
Code for Portfolio optimization

library(readxl)
library(PerformanceAnalytics)
library(TTR)
library(xts)
library(gridExtra)
library(ggplot)

ruta <- ("D:/Escritorio/JAIRO/SCOTIABANK/NUEVO/Baseprueba.xlsx")
basedf <- read_excel(ruta, sheet = "Final1");head(basedf)
basedf$Date <- as.Date(basedf$Date)

basets <- xts(basedf[,-1], basedf$Date)
rets<- ROC(basets);head(rets)
rets <- rets[-1,];head(rets)

g0 <- plot.xts(basets);g0
g1 <- plot.xts(rets);g1  

aa <- Return.annualized(rets, scale =365);aa
bb <- sd.annualized(rets, scale = 365);bb
cc <- SharpeRatio.annualized(rets, scale = 365);cc
dd<- Return.cumulative(rets);dd
ee <- maxDrawdown(rets);ee

#convirtiendo a Dataframe
 
a1 <- c(aa)
b1 <- c(bb)
c1 <- c(cc)
d1 <- c(dd)
e1 <- c(ee*-1)

print(colnames(aa))
metricas <- data.frame(Fondos = colnames(aa), Retorno = a1, Volatilidad = b1, Sharpe = c1, Ret_acum = d1, Max_caida = e1)
metricas


g4 <- ggplot(metricas, aes(x=Fondos, y =Ret_acum))+
  geom_bar(stat="identity", fill="black", width = 0.6)+
  labs(title = "Retorno Acumulado de los Fondos", x = "Fondos", y = "Retornos")+
  theme_minimal(); g4


g5 <- ggplot(metricas, aes(x=Fondos, y =Sharpe))+
  geom_bar(stat="identity", fill="black", width = 0.6)+
  labs(title = "Sharpe de los Fondos", x = "Fondos", y = "Retornos")+
  theme_minimal(); g5

g6 <- ggplot(metricas, aes(x=Fondos, y =Max_caida))+
  geom_bar(stat="identity", fill="black", width = 0.6)+
  labs(title = "Máxima Caída de los Fondos", x = "Fondos", y = "Retornos")+
  theme_minimal(); g6


g8 <- ggplot(metricas, aes(x=Volatilidad, y=Retorno))+
  geom_point(aes(color = Fondos), size = 3)+
      geom_text(aes(label = Fondos), vjust=-1,hjust=1)+
      labs(title="Retorno-Volatilidad",
           x="Volatilidad",
           y="Retorno")+
      theme_minimal();g8
    

grid.arrange(g8,g4,g6,g5,ncol=2)
  
