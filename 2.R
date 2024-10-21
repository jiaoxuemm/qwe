##第二章  

rm(list=ls())   ##删除当前环境中所有对象
setwd("E:/R/data")   ##设定工作目录


##模拟生成白噪声序列 

set.seed(10) #固定随机种子，方便之后生成相同数据
wn<-rnorm(200)
wn<-ts(wn,start = c(2000,1), frequency = 4)
##如果要模拟生成从2000年第3月开始的月度数据，则用start = c(2000,3), frequency = 12
start(wn)
end(wn)
length(wn)
plot(wn)
plot(wn,main="白噪声序列",col = "red")
wn.acf<-acf(wn,plot=T)
wn.acf

#acf函数的其他用法
acf(wn,lag=40,plot=T)
acf(wn,plot=F)
acf(wn)$acf


##时间序列平稳性的时序图检验和自相关图检验
##中国纱产量时间序列
sha<-read.table("D:/R/data/file4.csv",sep=",",header = T)
output<-ts(sha$output,start=1964)
plot(output,main="中国纱年产量序列时序图1964-1999",xlab="年份",ylab="纱产量")
acf(output）
pacf(output)
acf(output,lag=20,plot=FALSE)    #显示滞后各期自相关系数   
pacf(output,lag=20,plot=FALSE)    #显示滞后各期自相关系数 
##单位根检验
install.packages("tseries")
library(tseries)
adf.test(output)                 ##对output序列进行单位根检验
adf.test(diff(output))           ##对output序列的一阶差分进行单位检验


##平均每头奶牛月产量序列
a<-read.table("file5.csv",sep=",",header = T)
milk<-ts(a$milk,start = c(1962,1),frequency = 12)
plot(milk,main="平均每头奶牛月产量序列1962.1-1975.12")
acf(milk)
pacf(milk)

##北京市每年最高气温序列1949-1998
b<-read.table("file6.csv",sep=",",header = T)
temp<-ts(b$temp,start = 1949)
plot(temp,main="北京市每年最高气温序列1949-1998")
acf(temp)
pacf(temp)


###白噪声检验（纯随机性检验）
##模拟生成正态白噪声序列
white_noise<-rnorm(1000)
white_noise<-ts(white_noise)
plot(white_noise)
acf(white_noise)
#白噪声检验
Box.test(white_noise,lag = 6)                     #默认为Box-Pierce检验，白噪声检验，滞后期长度为6，自由度为6
Box.test(white_noise,type="Ljung-Box",lag = 6)    #设定为Ljung-Box检验，白噪声检验，滞后期长度为6，自由度为6
Box.test(white_noise,type="Ljung-Box",lag=12)     #白噪声检验，滞后期长度为12，自由度为12
for(i in 1:2)print(Box.test(white_noise,type="Ljung-Box",lag=6*i))  #白噪声检验，滞后期长度分别为6和12


##北京市城乡居民定期储蓄所占比例序列的纯随机性检验
c<-read.table("file7.csv",sep=",",header = T)
prop<-ts(c$prop,start = 1950)
plot(prop,main="北京市城乡居民定期储蓄所占比例")
acf(prop)
for(i in 1:2)print(Box.test(prop,type="Ljung-Box",lag=6*i))  #白噪声检验，滞后期长度分别为6和12


##差分运算
##中国纱产量时间序列的一阶差分
sha<-read.table("file4.csv",sep=",",header = T)
output<-ts(sha$output,start=1964)
plot.ts(output,main="中国纱年产量序列时序图1964-1999",xlab="年份",ylab="纱产量")
doutput<-diff(output)
plot(doutput,main="中国纱年产量序列的一阶差分")
acf(doutput)

####平均每头奶牛月产量序列的差分运算
a<-read.table("file5.csv",sep=",",header = T)
milk<-ts(a$milk,start = c(1962,1),frequency = 12)
plot(milk,main="平均每头奶牛月产量序列1962.1-1975.12")

dmilk<-diff(milk)    ##奶牛产量月度序列的一阶差分
plot(dmilk)
acf(dmilk,lag=36)

d12milk<-diff(milk,lag=12)    ##奶牛产量月度序列的一次季节差分（12步差分）
plot(d12milk)
acf(d12milk,lag=36)

dd12milk<-diff(diff(milk,lag=12)) ##取奶牛产量月度序列的一次季节差分后再进行一阶差分
plot(dd12milk)
acf(dd12milk,lag=36)


##退出
q()


