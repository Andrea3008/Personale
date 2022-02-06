library(xts)
library(zoo)
library(tseries)
library(quantmod)
library(dygraphs)
library(PerformanceAnalytics)
library(forecast)
library(caret)
library(kernlab)
library(nnet)
library(data.table)



#scarico i dati dei titoli selezionati TSLA, F, PFE, JNJ, JPM, WFC
start <- "2018-10-01"
end <- "2020-10-31"
TSLA <- getSymbols("TSLA", src= "yahoo", from = start, to = end, auto.assign = FALSE) #Tesla
FORD <- getSymbols("F", src= "yahoo", from = start, to = end, auto.assign = FALSE) #Ford
PFE <- getSymbols("PFE", src= "yahoo", from = start, to = end, auto.assign = FALSE) #Pfizer
JNJ <- getSymbols("JNJ", src= "yahoo", from = start, to = end, auto.assign = FALSE) #Johnson & Johnson
JPM <- getSymbols("JPM", src= "yahoo", from = start, to = end, auto.assign = FALSE) #JPMorgan Chase
WFC <- getSymbols("WFC", src= "yahoo", from = start, to = end, auto.assign = FALSE) #Wells Fargo
Tickers <- c("TSLA", "F", "PFE", "JNJ", "JPM", "WFC")


# differenzio i dati giornalieri
TSLA_daily <- TSLA$TSLA.Adjusted
FORD_daily <- FORD$F.Adjusted
PFE_daily <- PFE$PFE.Adjusted
JNJ_daily <- JNJ$JNJ.Adjusted
JPM_daily <- JPM$JPM.Adjusted
WFC_daily <- WFC$WFC.Adjusted
Stocks_daily <- merge(TSLA_daily, FORD_daily, PFE_daily, JNJ_daily, JPM_daily, WFC_daily)
colnames(Stocks_daily) <- Tickers


# estraggo i dati mensili e considero gli Adjusted close
TSLA <- to.monthly(TSLA)$TSLA.Adjusted
FORD <- to.monthly(FORD)$FORD.Adjusted
PFE <- to.monthly(PFE)$PFE.Adjusted
JNJ <- to.monthly(JNJ)$JNJ.Adjusted
JPM <- to.monthly(JPM)$JPM.Adjusted
WFC <- to.monthly(WFC)$WFC.Adjusted

head(TSLA)
head(FORD)
head(PFE)
head(JNJ)
head(JPM)
head(WFC)

Stocks <- merge(TSLA, FORD, PFE, JNJ, JPM, WFC)
colnames(Stocks) <- Tickers
head(Stocks)


#grafico dei dati mensili
par(mfcol=c(2,3))
plot(Stocks$TSLA, main="AdjClose Tesla", col="orange")
plot(Stocks$F, main="AdjClose Ford", col="black")
plot(Stocks$PFE, main="AdjClose Pfizer", col="blue")
plot(Stocks$JNJ, main="AdjClose J & J", col="red")
plot(Stocks$JPM, main="AdjClose JPM", col="green")
plot(Stocks$WFC, main="AdjClose Wells Fargo", col="violet")


# grafico dati giornalieri
plot(Stocks_daily$TSLA, main="AdjClose Tesla", col="orange")
plot(Stocks_daily$F, main="AdjClose Ford", col="black")
plot(Stocks_daily$PFE, main="AdjClose Pfizer", col="blue")
plot(Stocks_daily$JNJ, main="AdjClose J & J", col="red")
plot(Stocks_daily$JPM, main="AdjClose JPM", col="green")
plot(Stocks_daily$WFC, main="AdjClose Wells Fargo", col="violet")


colori <- c("orange", "black", "blue", "red", "green", "violet") #utilizzerò sempre gli stessi colori per rappresentare i titoli, per questo motivo creo un vettore 

#grafico AdjClose mensili completo 
#(poco significativo per l'analisi ma utile per capire l'ordine di grandezza del valore delle azioni)
par(mfrow=c(1,1))
plot(Stocks, main="Adjusted Close TSLA -F- PFE- JNJ- JPM- WFC", xlab="Tempo", ylab="Prezzo",
     col = colori)
posizione_leg <- locator() #individuo le coordinate del grafico in cui posizionare la legenda
#il comando locator() prende le coordinate del punto in cui clicchiamo sul grafico appena stampato
#cliccando su un punto e terminando l'esecuzione con ESC si ottengono le coordinate del punto in cui vogliamo posizionale la legenda
#utilizzo questo comando per posizionare la legenda in un punto vuoto del grafico in modo da non sovrapporla ad elementi importanti
legend(posizione_leg, legend = colnames(Stocks), fill = colori)


#grafico interattivo AdjClose mensili completo 
#(poco significativo per l'analisi ma utile per capire l'ordine di grandezza del valore delle azioni)
dygraph(Stocks_daily, main="Adjusted Close di TSLA-F-PFE-JNJ-JPM-WFC") %>% 
  dySeries("TSLA", label="Tesla", color = "orange") %>% 
  dySeries("F", label="Ford", color= "black") %>% 
  dySeries("PFE", label="Pfizer", color = "blue") %>% 
  dySeries("JNJ", label="Johnson & Johnson", color = "red") %>% 
  dySeries("JPM", label="JPMorgan Chase", color = "green") %>% 
  dySeries("WFC", label="Wells Fargo", color = "violet") %>% 
  dyAxis("y", label="Prezzo") %>% 
  dyAxis("x", label="Tempo") %>% 
  dyLegend(show = "onmouseover", hideOnMouseOut = T) %>%
  dyOptions(fillGraph = T) %>% 
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = T) %>%
  dyRangeSelector(height = 30)


#Descriptive Analytics

#CC Monthly Returns
TSLA_CCrtn <- na.omit(CalculateReturns(TSLA, method = "compound"))
FORD_CCrtn <- na.omit(CalculateReturns(FORD, method = "compound"))
PFE_CCrtn <- na.omit(CalculateReturns(PFE, method = "compound"))
JNJ_CCrtn <- na.omit(CalculateReturns(JNJ, method = "compound"))
JPM_CCrtn <- na.omit(CalculateReturns(JPM, method = "compound"))
WFC_CCrtn <- na.omit(CalculateReturns(WFC, method = "compound"))

#per una questione di visualizzazione dei dati nei grafici, cambio il nome delle colonne
colnames(TSLA_CCrtn) <- "TSLA"
colnames(FORD_CCrtn) <- "F"
colnames(PFE_CCrtn) <- "PFE"
colnames(JNJ_CCrtn) <- "JNJ"
colnames(JPM_CCrtn) <- "JPM"
colnames(WFC_CCrtn) <- "WFC"

#SIMPLE Monthly Returns
TSLA_SIMPLErtn <- na.omit(CalculateReturns(TSLA, method = "simple"))
FORD_SIMPLErtn <- na.omit(CalculateReturns(FORD, method = "simple"))
PFE_SIMPLErtn <- na.omit(CalculateReturns(PFE, method = "simple"))
JNJ_SIMPLErtn <- na.omit(CalculateReturns(JNJ, method = "simple"))
JPM_SIMPLErtn <- na.omit(CalculateReturns(JPM, method = "simple"))
WFC_SIMPLErtn <- na.omit(CalculateReturns(WFC, method = "simple"))




#SIMPLE Returns plots
par(mfcol=c(2,3))
plot(TSLA_SIMPLErtn, main="Simple returns di Tesla", xlab="Tempo", ylab="Ritorno", col="orange") 
plot(FORD_SIMPLErtn, main="Simple returns di Ford", xlab="Tempo", ylab="Ritorno", col="black")
plot(PFE_SIMPLErtn, main="Simple returns di Pfizer", xlab="Tempo", ylab="Ritorno", col="blue")
plot(JNJ_SIMPLErtn, main="Simple returns di J & J", xlab="Tempo", ylab="Ritorno", col="red")
plot(JPM_SIMPLErtn, main="Simple returns di JPMorgan Chase", xlab="Tempo", ylab="Ritorno", col="green")
plot(WFC_SIMPLErtn, main="Simple returns di Wells fargo", xlab="Tempo", ylab="Ritorno", col="violet")
par(mfcol=c(1,1))
#SIMPLE Returns time plot singolo
Stocks_SIMPLE <- merge(TSLA_SIMPLErtn, FORD_SIMPLErtn, PFE_SIMPLErtn, JNJ_SIMPLErtn, JPM_SIMPLErtn, WFC_SIMPLErtn)
colnames(Stocks_SIMPLE) <- Tickers
plot(Stocks_SIMPLE, main="SIMPLE returns TSLA-F-PFE-JNJ-JPM-WFC", xlab="Tempo", ylab="Ritorno", col = colori)
posizione_leg <- locator()
legend(posizione_leg, legend=colnames(Stocks), fill=colori)



#CC Returns plots
#stampo un grafico per ogni singolo titolo, confronto i 6 titoli, dopodiché confronto i titoli appartenenti allo stesso settore
par(mfcol=c(2,3))
plot(TSLA_CCrtn, main="CC returns di Tesla", xlab="Tempo", ylab="Ritorno", col="orange") 
plot(FORD_CCrtn, main="CC returns di Ford", xlab="Tempo", ylab="Ritorno", col="black")
plot(PFE_CCrtn, main="CC returns di Pfizer", xlab="Tempo", ylab="Ritorno", col="blue")
plot(JNJ_CCrtn, main="CC returns di J & J", xlab="Tempo", ylab="Ritorno", col="red")
plot(JPM_CCrtn, main="CC returns di JPMorgan Chase", xlab="Tempo", ylab="Ritorno", col="green")
plot(WFC_CCrtn, main="CC returns di Wells fargo", xlab="Tempo", ylab="Ritorno", col="violet")

#time plot singolo CC returns
par(mfrow=c(1,1))
Stocks_CC <- merge(TSLA_CCrtn, FORD_CCrtn, PFE_CCrtn, JNJ_CCrtn, JPM_CCrtn, WFC_CCrtn)
colnames(Stocks_CC) <- Tickers
plot(Stocks_CC, main="CC returns TSLA-F-PFE-JNJ-JPM-WFC", xlab="Tempo", ylab="Ritorno", col = colori)
posizione_leg <- locator()
legend(posizione_leg, legend=colnames(Stocks), fill=colori)





#confronto titoli dello stesso settore di mercato

AUTO_CC <- merge(TSLA_CCrtn, FORD_CCrtn) #creo un unico oggetto contenente i CC returns dei titoli del settore Automobile Manufacturers
plot(AUTO_CC, main ="CC Returns TSLA vs F", xlab="Tempo", ylab="Ritorno", col=c("orange", "black"))
posizione_leg <- locator()
legend(posizione_leg, legend=c("TSLA", "F"), fill = c("orange", "black"))


HEALTH_CC <- merge(PFE_CCrtn, JNJ_CCrtn) #creo un unico oggetto contenente i CC returns dei titoli del settore Health Care
plot(HEALTH_CC, main ="CC Returns PFE vs JNJ", xlab="Tempo", ylab="Ritorno", col=c("blue", "red"))
posizione_leg <- locator()
legend(posizione_leg, legend=c("PFE", "JNJ"), fill = c("blue", "red"))


BANKS_CC <- merge(JPM_CCrtn, WFC_CCrtn) #creo un unico oggetto contenente i CC returns dei titoli del settore Financial Services
plot(BANKS_CC, main ="CC Returns JPM vs WFC", xlab="Tempo", ylab="Ritorno", col=c("green", "violet"))
posizione_leg <- locator()
legend(posizione_leg, legend=c("JPM", "WFC"), fill = c("green", "violet"))








#4-panel diagnostic plots

TSLA.mat <- coredata(TSLA_CCrtn)
FORD.mat <- coredata(FORD_CCrtn)
PFE.mat <- coredata(PFE_CCrtn)
JNJ.mat <- coredata(JNJ_CCrtn)
JPM.mat <- coredata(JPM_CCrtn)
WFC.mat <- coredata(WFC_CCrtn)


par(mfrow=c(2,2))

#TSLA
hist(TSLA_CCrtn, breaks = 15, freq=F, main="Istogramma CC returns di TSLA", xlab="Ritorno", col = "orange")
plot(density(TSLA_CCrtn), main="Densità dei CC returns di TSLA", xlab="Ritorno", col="orange")
qqnorm(TSLA_CCrtn, main="QQ-plot CC returns di TSLA", col="orange")
qqline(TSLA_CCrtn)
chart.Boxplot(TSLA.mat, outlier.symbol = "O", main="Boxplot CC returns TSLA", col = "orange", xlab="Ritorno")

#F
hist(FORD_CCrtn, breaks = 15, freq=F, main="Istogramma CC returns di F", xlab="Ritorno", col = "black")
plot(density(FORD_CCrtn), main="Densità dei CC returns di F", xlab="Ritorno", col="black")
qqnorm(FORD_CCrtn, main="QQ-plot CC returns di F", col="black")
qqline(FORD_CCrtn)
chart.Boxplot(FORD.mat, outlier.symbol = "O", main="Boxplot CC returns di F", col = "black", xlab="Ritorno")

#PFE
hist(PFE_CCrtn, breaks = 15, freq=F, main="Istogramma CC returns di PFE", xlab="Ritorno", col = "blue")
plot(density(PFE_CCrtn), main="Densità dei CC returns di PFE", xlab="Ritorno", col="blue")
qqnorm(PFE_CCrtn, main="QQ-plot CC returns di PFE", col="blue")
qqline(PFE_CCrtn)
chart.Boxplot(PFE.mat, outlier.symbol = "O", main="Boxplot CC returns di PFE", col = "blue", xlab="Ritorno")

#JNJ
hist(JNJ_CCrtn, breaks = 15, freq=F, main="Istogramma CC returns di JNJ", xlab="Ritorno", col = "red")
plot(density(JNJ_CCrtn), main="Densità dei CC returns di JNJ", xlab="Ritorno", col="red")
qqnorm(JNJ_CCrtn, main="QQ-plot CC returns di JNJ", col="red")
qqline(JNJ_CCrtn)
chart.Boxplot(JNJ.mat, outlier.symbol = "O", main="Boxplot CC returns di JNJ", col = "red", xlab="Ritorno")

#JPM
hist(JPM_CCrtn, breaks=15, freq=F, main="Istogramma CC returns di JPM", xlab="Ritorno", col = "green")
plot(density(JPM_CCrtn), main="Densità dei CC returns di JPM", xlab="Ritorno", col="green")
qqnorm(JPM_CCrtn, main="QQ-plot CC returns di JPM", col="green")
qqline(JPM_CCrtn)
chart.Boxplot(JPM.mat, outlier.symbol = "O", main="Boxplot CC returns di JPM", col = "green", xlab="Ritorno")

#WFC
hist(WFC_CCrtn, breaks=15, freq=F, main="Istogramma CC returns di WFC", xlab="Ritorno", col = "violet")
plot(density(WFC_CCrtn), main="Densità dei CC returns di WFC", xlab="Ritorno", col="violet")
qqnorm(WFC_CCrtn, main="QQ-plot CC returns di WFC", col="violet")
qqline(WFC_CCrtn)
chart.Boxplot(WFC.mat, outlier.symbol = "O", main="Boxplot CC returns di WFC", col = "violet", xlab="Ritorno")


#Boxplot dei dati uniti
par(mfrow=c(1,1))
Stocks.mat <- coredata(Stocks_CC)
colnames(Stocks.mat) <- Tickers
chart.Boxplot(Stocks.mat, outlier.symbol = "O", main="Boxplot CC returns TSLA-F-PFE-JNJ-JPM-WFC", 
              xlab="Ritorno", colorset = colori)
#per individuare precisamente quali sono gli outliers, utilizzo la funzione boxplot.stats()
#non posso utilizzarla sulla serie di dati unica perché altrimenti non riesco a riconoscere a quale titolo appartengono
boxplot.stats(TSLA.mat)$out #non ci sono outliers in TSLA
boxplot.stats(FORD.mat)$out
boxplot.stats(PFE.mat)$out
boxplot.stats(JNJ.mat)$out #non ci sono outliers in JNJ
boxplot.stats(JPM.mat)$out
boxplot.stats(WFC.mat)$out





#Statistiche univariate

apply(Stocks_CC, 2, mean) #il titolo con media più alta è TSLA, quello con media più bassa è WFC
apply(Stocks_CC, 2, var) #il titolo con varianza più alta è TSLA, quello con varianza più bassa è JNJ
apply(Stocks_CC, 2, sd) 

#skewness = misura la simmetria di una distribuzione rispetto alla sua media (in base al suo valore capiamo se una distribuzione è più o meno simile ad una normale)
#         = 0 simmetrica (è una normale)
#         > 0 la coda di destra è più lunga rispetto ad una normale 
#         < 0 la coda di sinistra è più lunga rispetto ad una normale
apply(Stocks_CC, 2, skewness)

#kurtosis = misura lo spessore delle code di una distribuzione
#         = 0 le code sono come quelle di una normale
#         > 0 le code sono più spesse di quelle di una normale
#         < 0 le code sono più fine di quelle di una normale
apply(Stocks_CC, 2, kurtosis)

Quantili <- data.frame(apply(Stocks_CC, 2, quantile))


#creo una funzione per stampare automaticamente l'istogramma con i quantili appena calcolati del titolo passato come argomento
#l'altro argomento che viene passato è numStock, un semplice intero che mi permette di indicizzare il titolo nel dataframe Quantili
#in tutto il codice, i 6 titoli sono stati trattati sempre nello stesso ordine:
#TSLA=1, F=2, PFE=3, JNJ=4, JPM=5, WFC=6
plot_quantili_function <- function(CC_serie, numStock){
  hist(CC_serie, breaks=15, freq=F, main=paste("Istogramma CC returns con quantili di", colnames(CC_serie)), xlab="Ritorno", col = colori[numStock])
  for(i in 1:4){
    abline(v=Quantili[i, numStock], col="grey", lwd=5)
  }
}
plot_quantili_function(TSLA_CCrtn, 1)
plot_quantili_function(FORD_CCrtn, 2)
plot_quantili_function(PFE_CCrtn, 3)
plot_quantili_function(JNJ_CCrtn, 4)
plot_quantili_function(JPM_CCrtn, 5)
plot_quantili_function(WFC_CCrtn, 6)


# Matrici di covarianza, correlazione, scatterplots

matrice_covarianza <- cov(Stocks_CC)
matrice_correlazione <- cor(Stocks_CC) #Più correlati F - JPM, meno correlati F - PFE

library(car)
pairs(cbind(TSLA.mat, FORD.mat, PFE.mat, JNJ.mat, JPM.mat, WFC.mat), pch=18, col="blue", main = "Scatterplots matrix dei CC returns")
scatterplotMatrix(formula = ~ TSLA + F + PFE + JNJ + JPM + WFC, data=Stocks_CC, 
                  main="Matrice scatterplots per i CC returns di TSLA - F - PFE - JNJ - JPM - WFC") #visualizzazione nettamente più accattivante



lista_correlazione <- vector('list', length = 6)
lista_volatilità <- vector('list', length = 6)
lista_ritorni <- vector('list', length = 6)
delta_t <- 4
for(i in 1:length(lista_correlazione)){
  lista_correlazione[i] <- list(cor(Stocks_CC[(delta_t-3):delta_t,]))
  lista_volatilità[i] <- list(apply(Stocks_CC[(delta_t-3):delta_t,], 2, var))
  lista_ritorni[i] <- list(apply(Stocks_CC[(delta_t-3):delta_t,], 2, mean))
  delta_t <- delta_t + 4
}
names(lista_correlazione) <- c("11/2018 - 02/2019", "03/2019 - 06/2019", "07/2019 - 10/2019", "11/2019 - 02/2020", "03/2020 - 06/2020", "07/2020 - 10/2020")
names(lista_volatilità) <- c("11/2018 - 02/2019", "03/2019 - 06/2019", "07/2019 - 10/2019", "11/2019 - 02/2020", "03/2020 - 06/2020", "07/2020 - 10/2020")
names(lista_ritorni) <- c("11/2018 - 02/2019", "03/2019 - 06/2019", "07/2019 - 10/2019", "11/2019 - 02/2020", "03/2020 - 06/2020", "07/2020 - 10/2020")



for (i in 1:length(lista_correlazione)){
cat("Periodo :", names(lista_correlazione)[i],"\n")
cat("Matrice di correlazione :")
print(lista_correlazione[i])
cat("Volatilità dei titoli :")
print(lista_volatilità[i])
cat("Media dei CC returns mensili :")
print(lista_ritorni[i])
cat("\n\n\n\n\n\n\n\n\n")
}




# BETA COMPUTATION

SP500 <- getSymbols("^GSPC", src="yahoo", from=start, to=end, auto.assign = F)
SP500 <- to.monthly(SP500)
SP500_CC <- na.omit(CalculateReturns(SP500$SP500.Adjusted, method = "compound"))
colnames(SP500_CC) <- c("SP500")
head(SP500_CC)


beta_function <- function(stock, indice){
  beta <- cov(stock, indice)/var(indice)
  return(beta)
}

Stocks_Beta <- vector('numeric', length = 6)
for (i in 1:length(Stocks_Beta)){
  Stocks_Beta[i] <- beta_function(Stocks_CC[,i], SP500_CC)
}
names(Stocks_Beta) <- Tickers
Stocks_Beta

#calcolo ora i valori del beta per diverse finestre temporali
TSLA_betas <- NULL
FORD_betas <- NULL
PFE_betas <- NULL
JNJ_betas <- NULL
JPM_betas <- NULL
WFC_betas <- NULL

delta_t <- 4 #siccome i valori delle statistiche descrittive sono stati analizzati su quadrimestri diversi, 
# per un'analisi efficiente è utile calcolare anche il beta su finestre temporali di 4 mesi
length_period <- dim(SP500_CC)[1]
start <- delta_t+1
for (i in start:length_period){
  # Beta value <- beta_function(CC_SERIE[finestra temporale di 4 mesi], SP500[finestra temporale di 4 mesi])
  beta_val_TSLA <- beta_function(TSLA_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  beta_val_FORD <- beta_function(FORD_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  beta_val_PFE <- beta_function(PFE_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  beta_val_JNJ <- beta_function(JNJ_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  beta_val_JPM <- beta_function(JPM_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  beta_val_WFC <- beta_function(WFC_CCrtn[(i-delta_t):(i-1)], SP500_CC[(i-delta_t):(i-1)])
  # creo una ts per ogni valore di beta calcolato 
  beta_TSLA <- as.xts(beta_val_TSLA, order.by = index(TSLA_CCrtn[(i-1)]))
  beta_FORD <- as.xts(beta_val_FORD, order.by = index(FORD_CCrtn[(i-1)]))
  beta_PFE <- as.xts(beta_val_PFE, order.by = index(PFE_CCrtn[(i-1)]))
  beta_JNJ <- as.xts(beta_val_JNJ, order.by = index(JNJ_CCrtn[(i-1)]))
  beta_JPM <- as.xts(beta_val_JPM, order.by = index(JPM_CCrtn[(i-1)]))
  beta_WFC <- as.xts(beta_val_WFC, order.by = index(WFC_CCrtn[(i-1)]))
  
  
 
  if(is.null(TSLA_betas)){
    TSLA_betas <- beta_TSLA
    FORD_betas <- beta_FORD
    PFE_betas <- beta_PFE
    JNJ_betas <- beta_JNJ
    JPM_betas <- beta_JPM
    WFC_betas <- beta_WFC
    
  }else{
    TSLA_betas <- rbind(TSLA_betas, beta_TSLA)
    FORD_betas <- rbind(FORD_betas, beta_FORD)
    PFE_betas <- rbind(PFE_betas, beta_PFE)
    JNJ_betas <- rbind(JNJ_betas, beta_JNJ)
    JPM_betas <- rbind(JPM_betas, beta_JPM)
    WFC_betas <- rbind(WFC_betas, beta_WFC)
    
  }
  
  print('------FINESTRA TEMPORALE-------')
  print(paste("INIZIO FINESTRA TEMPORALE:", index(TSLA_CCrtn)[i-delta_t]))
  print(paste("FINE FINESTRA TEMPORALE:  ", index(TSLA_CCrtn)[i-1]))
  print('------BETA RELATIVO ALLA FINESTRA TEMPORALE------')
  print(paste("TIME INDEX BETA: ", index(TSLA_CCrtn)[i]))
  print(paste("TSLA beta:", beta_val_TSLA))
  print(paste("FORD beta:", beta_val_FORD))
  print(paste("PFE beta:", beta_val_PFE))
  print(paste("JNJ beta:", beta_val_JNJ))
  print(paste("JPM beta:", beta_val_JPM))
  print(paste("WFC beta:", beta_val_WFC))
  
}






#FORECAST

#devo scaricare i dati degli ultimi 10 anni
inizio_10 <- "2010-10-01"
fine_10 <- "2020-10-31"

TSLA_10y <- get.hist.quote("TSLA", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(TSLA_10y) <- as.yearmon(index(TSLA_10y))
TSLA_10y_rtn <- na.omit(CalculateReturns(TSLA_10y, method = "compound"))


FORD_10y <- get.hist.quote("F", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(FORD_10y) <- as.yearmon(index(FORD_10y))
FORD_10y_rtn <- na.omit(CalculateReturns(FORD_10y, method="compound"))


PFE_10y <- get.hist.quote("PFE", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(PFE_10y) <- as.yearmon(index(PFE_10y))
PFE_10y_rtn <- na.omit(CalculateReturns(PFE_10y, method="compound"))


JNJ_10y <- get.hist.quote("JNJ", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(JNJ_10y) <- as.yearmon(index(JNJ_10y))
JNJ_10y_rtn <- na.omit(CalculateReturns(JNJ_10y, method = "compound"))


JPM_10y <- get.hist.quote("JPM", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(JPM_10y) <- as.yearmon(index(JPM_10y))
JPM_10y_rtn <- na.omit(CalculateReturns(JPM_10y, method = "compound"))


WFC_10y <- get.hist.quote("WFC", provider = "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(WFC_10y) <- as.yearmon(index(WFC_10y))
WFC_10y_rtn <- na.omit(CalculateReturns(WFC_10y, method = "compound"))

# faccio il forecast anche dell'indice per la simulazione dell'investimento forecast-based
SP500_10y <- get.hist.quote("^GSPC", provider= "yahoo", start = inizio_10, end = fine_10, quote = "AdjClose", compression = "monthly")
index(SP500_10y) <- as.yearmon(index(SP500_10y))
SP500_10y_rtn <- na.omit(CalculateReturns(SP500_10y, method = "compound"))

Tickers_new <- c(Tickers, "SP500 index")

arma_function <- function(serie, i){
  
  TrainingSet <- serie[1:80] #n=80
  TestSet <- serie[81:110] #m=30
  
  best_RMSE <- 10 #inizializzo un valore di RMSE molto grande
  serie_ARIMA <- list()
  
  for(ar in 1:10){
    for(ma in 1:10){
      
      try(fit <- arima(TrainingSet, order = c(ar, 0, ma)))
      arma.forecast <- forecast(fit, h = length(TestSet),level = c(95,80))
     
      if ( accuracy(arma.forecast, TestSet)[2,2] < best_RMSE){ #se RMSE del modello attuale è minore del RMSE migliore trovato fino ad ora
        best_RMSE <- accuracy(arma.forecast, TestSet)[2,2] # aggiorno il valore di RMSE migliore
        best_fit <- fit
        best_forecast <- arma.forecast
      }
    }
  }
  plot(best_forecast, main=paste("ARIMA forecast for", Tickers_new[i], "CC returns"), xlab="Tempo", ylab="Ritorno")
  lines(TestSet)
  serie_ARIMA <- list(best_forecast, best_fit, TestSet)
  names(serie_ARIMA) <- c("Forecast", "BestFit", "TestSet")
  return(serie_ARIMA)
}


best_TSLA <- arma_function(TSLA_10y_rtn, 1)
accuracy(best_TSLA$Forecast, best_TSLA$TestSet)[2,2] #0.13

best_FORD <- arma_function(FORD_10y_rtn, 2)
accuracy(best_FORD$Forecast, best_FORD$TestSet)[2,2] #0.07

best_PFE <- arma_function(PFE_10y_rtn, 3)
accuracy(best_PFE$Forecast, best_PFE$TestSet)[2,2] #0.04

best_JNJ <- arma_function(JNJ_10y_rtn, 4)
accuracy(best_JNJ$Forecast, best_JNJ$TestSet)[2,2] #0.04

best_JPM <- arma_function(JPM_10y_rtn, 5) 
accuracy(best_JPM$Forecast, best_JPM$TestSet)[2,2] #0.05

best_WFC <- arma_function(WFC_10y_rtn, 6) 
accuracy(best_WFC$Forecast, best_WFC$TestSet)[2,2] #0.06

best_SP500 <- arma_function(SP500_10y_rtn, 7)
accuracy(best_SP500$Forecast, best_SP500$TestSet)[2,2] #0.03

# Simulazione investimento forecast-based strategy

# L'idea è quella di fare un forecast per ogni titolo di 40 mesi, 30 di test set + 10 mesi per l'investimento
TSLA_forecast_punc <- forecast(best_TSLA$BestFit, h=40, level=c(95,80))$mean
FORD_forecast_punc <- forecast(best_FORD$BestFit, h=40, level=c(95,80))$mean
PFE_forecast_punc <- forecast(best_PFE$BestFit, h=40, level=c(95,80))$mean
JNJ_forecast_punc <- forecast(best_JNJ$BestFit, h=40, level=c(95,80))$mean
JPM_forecast_punc <- forecast(best_JPM$BestFit, h=40, level=c(95,80))$mean
WFC_forecast_punc <- forecast(best_WFC$BestFit, h=40, level=c(95,80))$mean
SP500_forecast_punc <- forecast(best_SP500$BestFit, h=40, level=c(95,80))$mean

# le prime 30 predictions sono identiche a quelle trovate prima, per simulare il nostro investimento
# ci interessano solamente le ultime 10 predictions

TSLA_forecast_punc <- window(TSLA_forecast_punc, start=c(2020,01), end=c(2020,10))
FORD_forecast_punc <- window(FORD_forecast_punc, start=c(2020,01), end=c(2020,10))
PFE_forecast_punc <- window(PFE_forecast_punc, start=c(2020,01), end=c(2020,10))
JNJ_forecast_punc <- window(JNJ_forecast_punc, start=c(2020,01), end=c(2020,10))
JPM_forecast_punc <- window(JPM_forecast_punc, start=c(2020,01), end=c(2020,10))
WFC_forecast_punc <- window(WFC_forecast_punc, start=c(2020,01), end=c(2020,10))
SP500_forecast_punc <- window(SP500_forecast_punc, start=c(2020,01), end=c(2020,10))

# su queste predictions per i 10 mesi dell'investimento devo basare la mia strategia di investimento

accuracy(TSLA_forecast_punc, TSLA_CCrtn[15:24])[2] #0.28
accuracy(FORD_forecast_punc, FORD_CCrtn[15:24])[2] #0.15
accuracy(PFE_forecast_punc, PFE_CCrtn[15:24])[2] #0.09
accuracy(JNJ_forecast_punc, JNJ_CCrtn[15:24])[2] #0.07
accuracy(JPM_forecast_punc, JPM_CCrtn[15:24])[2] #0.10
accuracy(WFC_forecast_punc, WFC_CCrtn[15:24])[2] #0.14
accuracy(SP500_forecast_punc, SP500_CC[15:24])[2] #0.07

# uniamo le serie in un unico oggetto

TSLA_forecast_punc <- as.xts(window(TSLA_forecast_punc, start=c(2020,01), end=c(2020,10)))
FORD_forecast_punc <- as.xts(window(FORD_forecast_punc, start=c(2020,01), end=c(2020,10)))
PFE_forecast_punc <- as.xts(window(PFE_forecast_punc, start=c(2020,01), end=c(2020,10)))
JNJ_forecast_punc <- as.xts(window(JNJ_forecast_punc, start=c(2020,01), end=c(2020,10)))
JPM_forecast_punc <- as.xts(window(JPM_forecast_punc, start=c(2020,01), end=c(2020,10)))
WFC_forecast_punc <- as.xts(window(WFC_forecast_punc, start=c(2020,01), end=c(2020,10)))
SP500_forecast_punc <- as.xts(window(SP500_forecast_punc, start=c(2020,01), end=c(2020,10)))

Asset_forecast <- merge(TSLA_forecast_punc, FORD_forecast_punc, PFE_forecast_punc, JNJ_forecast_punc,
                        JPM_forecast_punc, WFC_forecast_punc)
colnames(Asset_forecast) <- Tickers






# siccome l'investimento parte il 2 gennaio 2020 non possiamo considerare i valori di beta 
# già calcolati.
# ci sono due strade: siccome abbiamo la previsione dei ritorni possiamo predire anche il valore
# di beta dei prossimi 10 mesi; in alternativa possiamo basarci sul valore di beta degli asset 
# calcolato nella finestra temporale novembre 2018 - dicembre 2019
# La strada che seguo è la prima


Investimento_beta <- vector('numeric', length = 6)
for (i in 1:length(Investimento_beta)){
  Investimento_beta[i] <- beta_function(Asset_forecast[,i], SP500_forecast_punc[,1])
}
Investimento_beta


# calcoliamo ora i rendimenti attesi per ogni asset

rendimento_portfolio_mercato <- 0.083 # Il rendimento medio annuo di S&P 500 è del 10% circa fin dalla sua nascita, dunque siccome il nostro investimento dura 10 mesi possiamo assumere che il rendimento atteso di SP500 in 10 mesi sia dell'8.3%

Rendimenti_attesi <- vector('numeric', length = 6)
for(i in 1:length(Rendimenti_attesi)){
  Rendimenti_attesi[i] <- Investimento_beta[i] * rendimento_portfolio_mercato
}
Rendimenti_attesi


# ora dobbiamo decidere i pesi per la costituzione del nostro portfolio
# dalle slides viste a lezione (Portfolio Optimization), dobbiamo sostanzialmente risolvere un
# problema di minimizzazione in cui la funzione obiettivo è
# min t(vettore_pesi) %*% matrice_di_costo %*% vettore_pesi
# siccome vogliamo diversificare il portfolio, come matrice di costo consideriamo la matrice di
# covarianza degli asset costruita basandosi sulle previsioni a nostra disposizione

matrice_covarianza <- cov(Asset_forecast)
matrice_covarianza
# costruiamo 5000 portafogli con pesi casuali

matrice_pesi <- matrix(nrow=10000, ncol=6)
vettore_ritorni <- vector('numeric', length = 10000)
vettore_rischi <- vector('numeric', length = 10000)
vettore_sharpe_ratio <- vector('numeric', length = 10000)

for (i in 1:10000){
  pesi <- runif(n=6)
  pesi <- pesi/sum(pesi) #serve per ottenere una somma dei pesi pari a 1
  matrice_pesi[i,] <- pesi
  
  ritorno_atteso_portfolio <- log(1+sum(pesi*Rendimenti_attesi))
  vettore_ritorni[i] <- ritorno_atteso_portfolio
  
  rischio_portfolio <- sqrt(t(pesi) %*% (matrice_covarianza %*% pesi))
  vettore_rischi[i] <- rischio_portfolio
  
  sharpe_ratio <- ritorno_atteso_portfolio/rischio_portfolio
  vettore_sharpe_ratio[i] <- sharpe_ratio
}

matrice_portafogli <- as.data.frame(cbind(matrice_pesi, vettore_ritorni, vettore_rischi, vettore_sharpe_ratio))
colnames(matrice_portafogli) <- c(Tickers, "Ritorno", "Rischio", "SharpeRatio")


# ora dobbiamo selezionare il nostro portfolio tra i 150 generati.
# assumiamo di essere investitori propensi al rischio, per cui non ci interessa il livello di 
# rischio dell'investimento ma ci focalizziamo sul portfolio con rendimento massimo

my_portfolio <- matrice_portafogli[which.max(matrice_portafogli$Ritorno),]
my_portfolio

cat("Peso di TSLA:",my_portfolio$TSLA,"\n")
cat("Peso di F:",my_portfolio$F,"\n")
cat("Peso di PFE:",my_portfolio$PFE,"\n")
cat("Peso di JNJ:",my_portfolio$JNJ,"\n")
cat("Peso di JPM:",my_portfolio$JPM,"\n")
cat("Peso di WFC:",my_portfolio$WFC,"\n")
cat("Somma dei pesi:",sum(my_portfolio[1,1:6]),"\n")

# simuliamo l'investimento

budget <- 2000
costo_transazione <- 0.01
inizio_investimento <- "2020-01-02"
fine_investimento <- "2020-10-01"


num_azioni <- vector('list', length = 6)
for(i in 1:length(num_azioni)){
  num_azioni[i] <- floor(budget * my_portfolio[,i]/(as.numeric(Stocks_daily[,i][inizio_investimento]) * (1+costo_transazione)))
}
names(num_azioni) <- Tickers
num_azioni

costo_azioni <- vector('list', length = 6)
costo_totale <- 0
for(i in 1:length(costo_azioni)){
  costo_azioni[i] <- as.numeric(num_azioni[i]) * as.numeric(Stocks_daily[,i][inizio_investimento]) * (1+costo_transazione)
  costo_totale <- costo_totale + as.numeric(costo_azioni[i])
}
names(costo_azioni) <- Tickers
costo_azioni
costo_totale
budget_avanzato <- budget - costo_totale
budget_avanzato

valore_iniziale_portfolio <- 0
ritorno_effettivo <- 0
for (i in 1:6){
  valore_iniziale_portfolio <- valore_iniziale_portfolio + as.numeric(num_azioni[i]) *as.numeric(Stocks_daily[,i][inizio_investimento])
  ritorno_effettivo <- ritorno_effettivo + 
                       (my_portfolio[,i] * #peso * 
                       (as.numeric(Stocks_daily[,i][inizio_investimento])/ #valore inizio / valore fine
                        as.numeric(Stocks_daily[,i][fine_investimento]) - 1)) # -1 
}

valore_finale_portfolio <- valore_iniziale_portfolio * (1+ritorno_effettivo)



cat("Valore portfolio alla costituzione:",valore_iniziale_portfolio)
cat("Previsioni di valore:", valore_iniziale_portfolio* (1+my_portfolio$Ritorno))
cat("Valore portfolio alla fine dell'investimento:",valore_finale_portfolio)
cat("Rendimento atteso portfolio:", round(100*my_portfolio$Ritorno, 2),"%")
cat("Rendimento effettivo del portfolio:", round(100*ritorno_effettivo, 2),"%")









# Portfolio management

#ricordiamo che la finestra di investimento è di 10 mesi, siccome le nostre serie di ritorni sono di 24 mesi dobbiamo considerare solamente i primi 14 mesi




TSLA_optim <- TSLA_SIMPLErtn[1:14,]
FORD_optim <- FORD_SIMPLErtn[1:14,]
PFE_optim <- PFE_SIMPLErtn[1:14,]
JNJ_optim <- JNJ_SIMPLErtn[1:14,]
JPM_optim <- JPM_SIMPLErtn[1:14,]
WFC_optim <- WFC_SIMPLErtn[1:14,]

Stocks_optim <- merge(TSLA_optim, FORD_optim, PFE_optim, JNJ_optim, JPM_optim, WFC_optim)

MOP <- portfolio.optim(Stocks_optim, shorts = F)
print(MOP)

for(i in 1:length(MOP$pw)){
  if(MOP$pw[i] <= 0)
    MOP$pw[i] = 0
}

cat("Peso di TSLA:",MOP$pw[1],"\n")
cat("Peso di F:",MOP$pw[2],"\n")
cat("Peso di PFE:",MOP$pw[3],"\n")
cat("Peso di JNJ:",MOP$pw[4],"\n")
cat("Peso di JPM:",MOP$pw[5],"\n")
cat("Peso di WFC:",MOP$pw[6],"\n")
cat("Somma dei pesi:",sum(MOP$pw),"\n")

mean_portfolio_returns <- seq(0.0,0.02,length.out=150)
risk_portfolio <- numeric(length(mean_portfolio_returns))+NA
for( i in 1:length(mean_portfolio_returns) ) {
  portfolio <- NULL
  try( portfolio <- portfolio.optim(x = Stocks_optim, pm=mean_portfolio_returns[i], shorts = F) )
  if( !is.null(portfolio) ) 
    risk_portfolio[i] <- portfolio$ps
}

plot( risk_portfolio, mean_portfolio_returns, pch=20, col="blue", xlab="Rischio", ylab="Ritorno",
      main="Frontiera efficiente con media in [0 , 0.02]")
points( MOP$ps, MOP$pm, pch=17, col="red" )






# Simulazione investimento
# Si assumono costi di transazione dell' 1%
budget <- 2000
costo_transazione <- 0.01
inizio_investimento <- "2020-01-02"

#num azioni da comprare, bisogna considerare il costo di transazione anche qui per evitare di investire una somma superiore al budget
TSLA_azioni <- floor(budget * MOP$pw[1]/(as.numeric(TSLA_daily$TSLA.Adjusted[inizio_investimento]) * (1+costo_transazione)))
TSLA_azioni

FORD_azioni <- floor(budget * MOP$pw[2]/(as.numeric(FORD_daily$F.Adjusted[inizio_investimento]) * (1+costo_transazione)))
FORD_azioni

PFE_azioni <- floor(budget * MOP$pw[3]/(as.numeric(PFE_daily$PFE.Adjusted[inizio_investimento]) * (1+costo_transazione)))
PFE_azioni

JNJ_azioni <- floor(budget * MOP$pw[4]/(as.numeric(JNJ_daily$JNJ.Adjusted[inizio_investimento]) * (1+costo_transazione)))
JNJ_azioni

JPM_azioni <- floor(budget * MOP$pw[5]/(as.numeric(JPM_daily$JPM.Adjusted[inizio_investimento]) * (1+costo_transazione)))
JPM_azioni

WFC_azioni <- floor(budget * MOP$pw[6]/(as.numeric(WFC_daily$WFC.Adjusted[inizio_investimento]) * (1+costo_transazione)))
WFC_azioni

#costo
TSLA_costo <- (TSLA_azioni * as.numeric(TSLA_daily$TSLA.Adjusted[inizio_investimento])) * (1+costo_transazione)
TSLA_costo

FORD_costo <- (FORD_azioni * as.numeric(FORD_daily$F.Adjusted[inizio_investimento])) * (1+costo_transazione)
FORD_costo

PFE_costo <- (PFE_azioni * as.numeric(PFE_daily$PFE.Adjusted[inizio_investimento])) * (1+costo_transazione)
PFE_costo

JNJ_costo <- (JNJ_azioni * as.numeric(JNJ_daily$JNJ.Adjusted[inizio_investimento])) * (1+costo_transazione)
JNJ_costo

JPM_costo <- (JPM_azioni * as.numeric(JPM_daily$JPM.Adjusted[inizio_investimento])) * (1+costo_transazione)
JPM_costo

WFC_costo <- (WFC_azioni * as.numeric(WFC_daily$WFC.Adjusted[inizio_investimento])) * (1+costo_transazione)
WFC_costo

costo_totale <- TSLA_costo + FORD_costo + PFE_costo + JNJ_costo + JPM_costo + WFC_costo
costo_totale
budget_avanzato <- round(budget - costo_totale, 2)
budget_avanzato


fine_investimento <- "2020-10-01" #10 mesi dopo l'inizio

TSLA_valore_inizio <- as.numeric(TSLA_daily$TSLA.Adjusted[inizio_investimento])
FORD_valore_inizio <- as.numeric(FORD_daily$F.Adjusted[inizio_investimento])
PFE_valore_inizio <- as.numeric(PFE_daily$PFE.Adjusted[inizio_investimento])
JNJ_valore_inizio <- as.numeric(JNJ_daily$JNJ.Adjusted[inizio_investimento])
JPM_valore_inizio <- as.numeric(JPM_daily$JPM.Adjusted[inizio_investimento])
WFC_valore_inizio <- as.numeric(WFC_daily$WFC.Adjusted[inizio_investimento])

TSLA_valore_fine <- as.numeric(TSLA_daily$TSLA.Adjusted[fine_investimento])
FORD_valore_fine <- as.numeric(FORD_daily$F.Adjusted[fine_investimento])
PFE_valore_fine <- as.numeric(PFE_daily$PFE.Adjusted[fine_investimento])
JNJ_valore_fine <- as.numeric(JNJ_daily$JNJ.Adjusted[fine_investimento])
JPM_valore_fine <- as.numeric(JPM_daily$JPM.Adjusted[fine_investimento])
WFC_valore_fine <- as.numeric(WFC_daily$WFC.Adjusted[fine_investimento])

#valore MOP a inizio investimento
MOP_valore_inizio <- TSLA_valore_inizio*TSLA_azioni + FORD_valore_inizio*FORD_azioni + 
                     PFE_valore_inizio*PFE_azioni + JNJ_valore_inizio*JNJ_azioni +
                     JPM_valore_inizio*JPM_azioni + WFC_valore_inizio*WFC_azioni
MOP_valore_inizio



#valore MOP a fine investimento
MOP_valore_fine <- TSLA_valore_fine*TSLA_azioni + FORD_valore_fine*FORD_azioni + 
                   PFE_valore_fine*PFE_azioni + JNJ_valore_fine*JNJ_azioni +
                   JPM_valore_fine*JPM_azioni + WFC_valore_fine*WFC_azioni
MOP_valore_fine


MOP_rtn <- MOP$pw[1]*(TSLA_valore_fine/TSLA_valore_inizio-1) + MOP$pw[2]*(FORD_valore_fine/FORD_valore_inizio-1) + 
           MOP$pw[3]*(PFE_valore_fine/PFE_valore_inizio-1) + MOP$pw[4]*(JNJ_valore_fine/JNJ_valore_inizio-1) + 
           MOP$pw[5]*(JPM_valore_fine/JPM_valore_inizio-1) + MOP$pw[6]*(WFC_valore_fine/WFC_valore_inizio-1) 
  
cat("Ritorno atteso del MOP:",MOP$pm,"[",round(100*MOP$pm,2),"% ]\n")
cat("Ritorno effettivo del MOP:",MOP_rtn,"[",round(100*MOP_rtn,2),"% ]")

  
  






# WEB APP
library(shiny)
library(magick)
library(rsvg)

source("./ui.R")
source("./server.R")
shinyApp(ui = ui, server = server)












# fine codice progetto
# più giù nel codice c'è una prova di forecasting con SVM sul titolo TSLA, il modello risulta essere
# meno performante rispetto ad ARIMA e dunque è stato scartato








  


# SVM TSLA, meno performante rispetto ad ARIMA in questo caso

input_TSLA <- merge(shift(TSLA_10y_rtn, 1), shift(TSLA_10y_rtn, 2), shift(TSLA_10y_rtn , 3), all=F)
data_TSLA <- merge(input_TSLA, TSLA_10y_rtn, all=F)
colnames(data_TSLA) <- c("lag.1", "lag.2", "lag.3", "TARGET")
data_TSLA <- na.omit(data_TSLA)
head(data_TSLA)
nrow(data_TSLA)
train_SVM_TSLA_index <- createDataPartition(1:nrow(data_TSLA), p=(80/120), list=F)[,1] #insieme di indici
train_SVM_TSLA_index
train_SVM_TSLA <- as.data.frame(data_TSLA[train_SVM_TSLA_index])
head(train_SVM_TSLA)

test_SVM_TSLA <- as.data.frame(data_TSLA[-train_SVM_TSLA_index])
nrow(test_SVM_TSLA) #devo ulteriormente togliere gli ultimi 7 mesi affinché m=30
test_SVM_TSLA <- test_SVM_TSLA[ 1 : (nrow(test_SVM_TSLA) - 7),]
nrow(test_SVM_TSLA) # m=30
rownames(test_SVM_TSLA) <- NULL

bootControl <- trainControl(number=200) 
preProc <- c("center", "scale")
indexTrn <- ncol(train_SVM_TSLA)


svmFit_TSLA <- train(train_SVM_TSLA[, -indexTrn], train_SVM_TSLA[,indexTrn], method="svmRadial", tuneLength = 5,
                     trControl = bootControl, preProc = preProc)
svmFit_TSLA
svmBest_TSLA <- svmFit_TSLA$finalModel


pred_SVM_TSLA <- predict(svmBest_TSLA, test_SVM_TSLA[, -ncol(test_SVM_TSLA)])
length(pred_SVM_TSLA)
length(test_SVM_TSLA[,1])
pred_SVM_TSLA

#library(Metrics) #OVERRIDE DELLA FUNZIONE ACCURACY(), UNA VOLTA CARICATO QUESTO PACCHETTO LA FUNNZIONE ACCURACY() UTILIZZATA PER IL FORECAST NON FUNZIONA PIÙ
actual_TARGET_TSLA <- test_SVM_TSLA[,ncol(test_SVM_TSLA)] #colonna TARGET
predicted_TARGET_TSLA <- pred_SVM_TSLA #predictions fatte da SVM

mae_svm <- mae(actual = actual_TARGET_TSLA, predicted = predicted_TARGET_TSLA)
mse_svm <- mse(actual = actual_TARGET_TSLA, predicted = predicted_TARGET_TSLA)
  
  
