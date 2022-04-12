data=data=read.csv("arima.csv",header=T,sep=",")
head(data)
library(forecast)
install.packages("lmtest")
library(lmtest)
library(tseries)

saham_tseries=ts(data,frequency=12,start=c(2014,1),end=c(2020,12))
head(saham_tseries)
sts1=saham_tseries
plot.ts(sts1, main="data arima")

#untuk mengetahui pola dalam data
dec=decompose(sts1);dec
plot(dec)

#Cek stasioner dalam varians
BoxCox.lambda(sts1)
#jika >1 maka stasioner dalam varians

#Cek stationer dalam rata-rata
adf.test(saham_tseries)
#H0 diterima, data tidak stationer bila p-val>0,05(alfa)
t1=diff(saham_tseries,1)
adf.test(t1)
#H0 ditolak, data stationer bila p-val<0,05(alfa)

##penentuan orde ARIMA
#Plot ACF (untuk menentukan MA(q))
acf(t1) 
#Plot PACF (untuk menentukan AR(p))
pacf(t1) 

#ARIMA (p,d,q) cari dari plot ACF dan PACF
fit1=arima(sts1, order=c(1,1,0));fit1
fit2=arima(sts1, order=c(0,1,1));fit2
fit3=arima(sts1, order=c(1,1,1));fit3
fit4=arima(sts1, order=c(3,1,0));fit4 #tidak signifikan
fit5=arima(sts1, order=c(3,1,1));fit5 #tidak signifikan
fit6=arima(sts1, order=c(3,1,3));fit6 #tidak signifikan
fit7=arima(sts1, order=c(0,1,3));fit7 #tidak signifikan
fit8=arima(sts1, order=c(1,1,3));fit8 #tidak signifikan
#melihat analisis error
summary(fit1)

#Uji Signifikansi Parameter Manual
#contoh model fit 1
n=length(t1)
p=1
#thitung=koefisien taksiran/standard error
thit=0.3327/0.1037 ;thit
ttabel=qt(c(0.05/2),df=n-1,lower.tail=FALSE);ttabel
#thit<ttabel maka parameter signifikan

#Uji Signifikansi Parameter Syntacs
printstatarima <- function (x, digits = 4,se=T,...){
  if (length(x$coef) > 0) {
    cat("\nCoefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <- coef[1,]/ses
      pval  <- 2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = F)
      coef <- rbind(coef, t=round(statt,digits=digits),sign.=round(pval,digits=digits))
      coef <- t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
#contoh model fit 1
printstatarima(fit1)
#pval<alfa maka parameter signifikan

#Diagnostic Check
##Residual
res1=residuals(fit1)
res2=residuals(fit2)
res3=residuals(fit3)
##Uji Residual Non-Autokorelasi
tsdiag(fit1,gof.lag=84)
tsdiag(fit2,gof.lag=84)
tsdiag(fit3,gof.lag=84)

##Uji Normalitas
#Menggunakan kolmogorov smirnov
#Hipotesis, Tolak H0 jika p-value<=0.05
#H0:Data residual mengikuti distribusi normal
#H1: Data residual tidak mengikuti distribusi normal
n1=length(res1)
mean1=mean(res1)
sd1=sd(res1)
resn1=rnorm(n1,mean1,sd1)
ks.test(res1,resn1)
# model fit1, pval>alfa, maka menerima H0
n2=length(res2)
mean2=mean(res2)
sd2=sd(res2)
resn2=rnorm(n2,mean2,sd2)
ks.test(res2,resn2)
# model fit2, pval>alfa, maka menerima H0
n3=length(res3)
mean3=mean(res3)
sd3=sd(res3)
resn3=rnorm(n3,mean3,sd3)
ks.test(res3,resn3)
# model fit2, pval>alfa, maka menerima H0

##Uji White Noise
#H0: residual memenuhi syarat white noise
#tolak H0 bila pval<alfa
Box.test(res1^2,lag=1,type="Ljung-Box")
Box.test(res2^2,lag=1,type="Ljung-Box")
Box.test(res3^2,lag=1,type="Ljung-Box")
#setiap model mnerima H0

#Pemilihan model terbaik
##error
e1=res1
e2=res2
e3=res3
##MSE
mse1=sum(e1^2)/84;mse1
mse2=sum(e2^2)/84;mse2
mse3=sum(e3^2)/84;mse3
##MAPE
mape1=(sum(abs(e1/t1))/84)*100;mape1
mape2=(sum(abs(e2/t1))/84)*100;mape2
mape3=(sum(abs(e3/t1))/84)*100;mape3
##AIC
AIC1=fit1$aic;AIC1
AIC2=fit2$aic;AIC2
AIC3=fit3$aic;AIC3

#Peramalan
##misalnya yg paling kecil MAPE,MSE,AIC
fit3=arima(sts1,order=c(1,1,1))
#untuk meramal 12 periode ke depan
peramalan=forecast(fit3,12);peramalan
plot(peramalan)

#Menentukan ARIMA Manual
auto.arima(sts1)
