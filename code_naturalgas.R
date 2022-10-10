str(data_naturalgas)
names(data_naturalgas)[names(data_naturalgas) == "PNGASEUUSDM"] <- "Price"
class(data_naturalgas)
summary(data_naturalgas)
dim(data_naturalgas)
print(data_naturalgas)
library(ggplot2)
ggplot(data_naturalgas, aes(DATE,Price,group=1))+geom_line()+labs(title='Global price of Natural Gas', x="Years", y="US Dollars per Metric Ton")
sum(is.na(data_naturalgas))
summary(is.na(data_naturalgas))
boxplot(data_naturalgas[,c('Price')])

# Converting to ts

data_naturalgas.ts=ts(data_naturalgas[,-1],frequency = 12)
plot(data_naturalgas.ts)

# ACF

acf(data_naturalgas.ts)

# PACF

pacf(data_naturalgas.ts)

# Dicky-Fuller test

library(tseries)
adf.test(data_naturalgas.ts)

data_naturalgas.ts.df = diff(data_naturalgas.ts)
plot(data_naturalgas.ts.df)
acf(data_naturalgas.ts.df,ylab="ACF")
pacf(data_naturalgas.ts.df,ylab="PACF")
adf.test(data_naturalgas.ts.df)

install.packages("FitAR")
install.packages("rcompanion")
library(FitAR)
library(rcompanion)
parameterEstimationARIMA <- function(p,d,q) {
  #Parameter estimation using CSS and MLE
  
  Estimate1=arima(data_naturalgas.ts.df,order=c(p,d,q), method = 'CSS')
  print(Estimate1$coef[1:2])
  Estimate1=arima(data_naturalgas.ts.df,order=c(p,d,q), method = 'ML')
  print(Estimate1$coef[1:2])
  
  #Compare models using AIC and BIC
  
  AR=arima(data_naturalgas.ts.df,order = c(p,d,q))
  print(AIC(AR))
  print(BIC(AR))
  acf(AR$residuals)
  
  # Ljung-Box Test
  
  boxresult=LjungBoxTest(AR$residuals,k=2,StartLag=1)
  plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
  
  # QQ Plot
  
  qqnorm(AR$residuals)
  qqline(AR$residuals)
  
  # Shapiro-Wilk Test
  
  print(shapiro.test(AR$residuals))
  plotNormalHistogram(AR$residuals)
  ts.plot(data_naturalgas.ts.df)
  AR_fit = data_naturalgas.ts.df - residuals(AR)
  points(AR_fit,type='o',col='red',lty=2)
}

#q=2,4,5 p=3, d=1

parameterEstimationARIMA(3,1,2)
parameterEstimationARIMA(3,1,4)
parameterEstimationARIMA(3,1,5)

library(forecast)
final_AR=arima(data_naturalgas.ts.df,order = c(3,1,2))
pred=predict(final_AR,n.ahead=10)
ts.plot(data_naturalgas.ts.df, pred$pred,col=c(4,2))
