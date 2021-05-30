souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

###Data

library(TSA)
souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirts
souvenirlog=log(souvenirts) #take log

#plot time series 
plot(souvenirts,main="time series plots of monthly sales of souenir",ylab="sales")
plot(souvenirlog)
#plot acf
acf(as.vector(souvenirts),lag.max = 36,main="sample ACF of monthly sales of souvenir") 
acf(as.vector(souvenirlog),lag.max = 36) 
###


### Model specification

#plot the time series after first difference
plot(diff(souvenirts),main='time series plots of first difference of monthly sales of souvenir',ylab="first difference of sales")
points(y=diff(souvenirts),x=time(diff(souvenirts)),pch=as.vector(season(diff(souvenirts))))
plot(diff(souvenirlog))
points(y=diff(souvenirlog),x=time(diff(souvenirlog)),pch=as.vector(season(diff(souvenirlog))))

#plot acf after first difference
acf(as.vector(diff(souvenirts)),lag.max = 36,main="sample ACF of first difference of monthly sales of souvenir ")
acf(as.vector(diff(souvenirlog)),lag.max = 36)

eacf(as.vector(diff(souvenirlog)))#cannot tell the model

#seasonal diff
plot(diff(souvenirlog,lag=12))
points(y=diff(souvenirlog,lag=12),x=time(diff(souvenirlog,lag=12)),pch=as.vector(season(diff(souvenirlog,lag=12))))

#acf/pacf after seasonal diff
acf(as.vector(diff(souvenirlog,lag=12)),lag.max = 36,ci.type="ma")
pacf(as.vector(diff(souvenirlog,lag=12)),lag.max = 36)

eacf(as.vector(diff(souvenirlog,lag=12))) #ARMA(0,2)*(0,1,0)?

#plot the time series after first and seasonal differences
plot(diff(diff(souvenirts),lag=12),main="time series plots of first and sesasonal differences of monthly sales of souvenir",
     ylab="first and seasonal differences of sales")
points(diff(diff(souvenirts),lag=12),x=time(diff(diff(souvenirts),lag=12)),
       pch=as.vector(season(diff(diff(souvenirts),lag=12))))
plot(diff(diff(souvenirlog),lag=12))
points(diff(diff(souvenirlog),lag=12),x=time(diff(diff(souvenirlog),lag=12)),
       pch=as.vector(season(diff(diff(souvenirlog),lag=12))))
#plot acf after first and seasonal differences 
acf(as.vector(diff(diff(souvenirts),lag=12)),lag.max = 36,ci.type="ma",main="sample ACF of first and seasonal differences of monthly sales of souvenir")
acf(as.vector(diff(diff(souvenirlog),lag=12)),lag.max = 36)

#plot pacf after first and seasonal differences
pacf(as.vector(diff(diff(souvenirts),lag=12)),lag.max = 36,ci.type="ma",main="sample PACF of first and seasonal differences of monthly sales of souvenir")
pacf(as.vector(diff(diff(souvenirlog),lag=12)),lag.max = 36,ci.type="ar")

eacf(as.vector(diff(diff(souvenirlog),lag=12)))#ARIMA(0,1,1)*(0,1,0)

#consider specifying the multiplicative, seasonal ARIMA(0,1,1)*(0,1,1) for both souvenir and log(souvenir)
###


###Model fitting
model=arima(souvenirts,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
model2=arima(souvenirlog,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
model$aic
model2$aic
#Therefore, model2 is better, consider seasonal ARIMA(0,1,1)*(0,1,1) for log(soouvenir)
###


###Diagnostic checking

#plot of residuals                                                                               
plot(rstandard(model2),ylab="standardized residuals")
#acf plot of residuals
acf(as.vector(rstandard(model2)),lag.max = 36,main="acf of residuals")
#Ljung-Box test
tsdiag(model2) 

##Check the normality of the residuals
#plot the histagram
hist(rstandard(model2),main="residuals")
#qqplot
qqnorm(rstandard(model2))
qqline(rstandard(model2))
#Shapiro-Wilk normality test
shapiro.test(rstandard(model2))
#Residuals have normality
###


###Forecasting

#Forecast next 2 years
plot(model2,n1=c(1991,1),n.ahead = 24,ylab="log(souvenir)")
#Forecast next 4 years
plot(model2,n1=c(1989,1),n.ahead = 48,ylab="log(souvenir)")

