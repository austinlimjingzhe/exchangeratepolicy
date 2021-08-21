#load libraries
library(easypackages)
libraries("urca","vars","tseries","tidyverse","forecast","dplyr","gridExtra","RColorBrewer","DT")

#import data
vietnam_quarterly<-read.csv("C:/Users/User/OneDrive/Desktop/vietnam quarterly data.csv")

gdp<-ts(log(vietnam_quarterly$GDP),start=c(2002,2),frequency = 4)
cpi<-ts(log(vietnam_quarterly$CPI),start=c(2002,2),frequency = 4)
exr<-ts(log(vietnam_quarterly$EXR),start=c(2002,2),frequency = 4)
m2<-ts(log(vietnam_quarterly$M2),start=c(2002,2),frequency = 4)
#ir<-ts(vietnam_quarterly$IR,start=c(2002,2),frequency = 4)
#liquid<-ts(vietnam_quarterly$LIQUID,start=c(2012,2),frequency = 4)

pp.test(gdp) #all are non-stationary.
pp.test(cpi)
pp.test(exr)
pp.test(m2)

pp.test(diff(cpi,differences = 1))
pp.test(diff(exr,differences = 1))
pp.test(diff(m2,differences = 1))

# cpi<-diff(cpi,differences = 1)
# exr<-diff(exr,differences = 1)
# m2<-diff(m2,differences = 1)

plot(cbind(gdp,cpi,exr,m2),main = "time series plots")

newdf<-na.omit(cbind(gdp,cpi,exr,m2))
colnames(newdf)<-c("gdp","cpi","exr","m2")

bestlag<-VARselect(newdf,type = "both",lag.max = 10)
bestlag$selection


var1<-VAR(newdf,p=4,type="both",season = 4, exogen =NULL)
plot(stability(var1, type="OLS-CUSUM"),nc=2)

#granger causality
gcexr<-causality(var1,cause = "exr")
gcexr
gcm2<-causality(var1,cause = "m2")
gcm2

granger<-cbind(gcexr$Granger$p.value,gcm2$Granger$p.value)
colnames(granger)<-c("Granger-cause EXR","Granger-cause M2")
rownames(granger)<-"p-value"
granger
datatable(granger)

#irf
gdpirf1<-irf(var1,impulse = "exr",response = "gdp",boot = T,n.ahead = 40)
gdpirf2<-irf(var1,impulse = "m2",response = "gdp",boot = T,n.ahead = 40)

cpiirf1<-irf(var1,impulse = "exr",response = "cpi",boot = T,n.ahead = 40)
cpiirf2<-irf(var1,impulse = "m2",response = "cpi",boot = T,n.ahead = 40)

plot(gdpirf1,ylab="GDP",main="Shock to Exchange Rate")
plot(gdpirf2,ylab="GDP",main="Shock to M2")

plot(cpiirf1,ylab="CPI",main="Shock to Exchange Rate")
plot(cpiirf2,ylab="CPI",main="Shock to M2")

#variance decomposition
fevd1<-fevd(var1,n.ahead = 40)
timeframe<-factor(c("3 months","6 months", "1 year","5 years","10 years"))
variables<-factor(c("m2","exr","cpi","gdp"))

res1<-fevd1$gdp[c(1,2,4,20,40),]
res2<-fevd1$cpi[c(1,2,4,20,40),]
res3<-fevd1$exr[c(1,2,4,20,40),]
res4<-fevd1$m2[c(1,2,4,20,40),]

res1<-cbind(data.frame(res1),timeframe)
res2<-cbind(data.frame(res2),timeframe)
res3<-cbind(data.frame(res3),timeframe)
res4<-cbind(data.frame(res4),timeframe)

res1<-res1%>%gather(variable,percent,-timeframe)
res2<-res2%>%gather(variable,percent,-timeframe)
res3<-res3%>%gather(variable,percent,-timeframe)
res4<-res4%>%gather(variable,percent,-timeframe)

res1$timeframe<-factor(res1$timeframe,levels = timeframe)
res2$timeframe<-factor(res2$timeframe,levels = timeframe)
res3$timeframe<-factor(res3$timeframe,levels = timeframe)
res4$timeframe<-factor(res4$timeframe,levels = timeframe)

res1$variable<-factor(res1$variable,levels = variables)
res2$variable<-factor(res2$variable,levels = variables)
res3$variable<-factor(res3$variable,levels = variables)
res4$variable<-factor(res4$variable,levels = variables)

g1<-ggplot(res1, aes(fill=variable, y=percent, x=timeframe)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Variance Decomposition of GDP") + 
  scale_fill_brewer(palette="Dark2")
g2<-ggplot(res2, aes(fill=variable, y=percent, x=timeframe)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Variance Decomposition of CPI") + 
  scale_fill_brewer(palette="Dark2")
g3<-ggplot(res3, aes(fill=variable, y=percent, x=timeframe)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Variance Decomposition of EXR") + 
  scale_fill_brewer(palette="Dark2")
g4<-ggplot(res4, aes(fill=variable, y=percent, x=timeframe)) + 
  geom_bar(position="stack", stat="identity") +
  labs(title="Variance Decomposition of M2") + 
  scale_fill_brewer(palette="Dark2")
grid.arrange(g1, g2, g3, g4, nrow = 2)

s<-summary(var1)
s$roots

#diagnostics
serial.test(var1,lags.pt = 12,type = "PT.asymptotic")
arch.test(var1,lags.multi = 12,multivariate.only = T)
normality.test(var1,multivariate.only = T)

cointegration <- ca.jo(newdf, type="trace",ecdet="trend",spec="transitory")
summary(cointegration)

gdppred<-predict(var1,n.ahead = 40)$fcst$gdp[c(1,2,6,20,40),-4]
cpipred<-predict(var1,n.ahead = 40)$fcst$cpi[c(1,2,6,20,40),-4]
t(gdppred)
t(cpipred)
