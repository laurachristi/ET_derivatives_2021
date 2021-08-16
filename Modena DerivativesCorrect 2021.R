library(readxl)
library(writexl)
library(tidyr)
library(tidyverse)

setwd("~/Modena data") # sets the working directory in the folder containing the data file
data=read_excel("Allen/Soil Moisture/2021/Corn/Modena 4 Corn West July 6.xlsx", sheet="SM Data")        # load the data from excel

w=3 # window used to calculate the derivatives in days

### sensor 1/101 ################################################################

data.101=data.frame(Date=data$`Date`, sm1=data$`1 Acclima`,sm2=data$`2 Acclima`, sm3=data$`3 Acclima`, sm4=data$`4 Acclima`, sm5=data$`5 Acclima`, sm6=data$`6 Acclima`, sm7=data$`7 Acclima`, sm8=data$`8 Acclima`)
#data.101=data.frame(Date=data$`Date`, sm1=data$`1 Acclima`,sm2=data$`2 Acclima`, sm3=data$`3 Acclima`, sm4=data$`4 Acclima`, sm5=data$`5 Acclima`, sm6=data$`6 Acclima`, sm7=data$`7 Acclima`)
data.101=na.omit(data.101)
n=length(data.101$Date)

# sensor depths
##Depths for Corn 215, 245, 252 
sd1=3*25.4 # sensor 1: from surface to 3 in
sd2=3*25.4 # sensor 2: from surface to 3 in
sd3=6*25.4 # sensor 3: from 3 in to 6 in
sd4=6*25.4 # sensor 4: from 3 in to 6 in
sd5=12*25.4 # sensor 5: from 6 in to 1ft
sd6=24*25.4 # sensor 6: from 1ft to 2ft
sd7=36*25.4 # sensor 7: from 2ft to 3ft
sd8=48*25.4 # sensor 8: from 3ft to 4ft

##Depths for Corn 215, 263
#sd1=3*25.4 # sensor 1: from surface to 3 in
#sd2=6*25.4 # sensor 2: from 6 in to 12 in
#sd3=12*25.4 # sensor 3: from 12 in to 1ft
#sd4=24*25.4 # sensor 4: from 1ft to 2ft
#sd5=36*25.4 # sensor 5: from 2ft to 3ft
#sd6=48*25.4 # sensor 6: from 3ft to 4ft
#sd7=60*25.4 # sensor 7: from 4ft to 5ft

##Depths for Corn 215
#sd1=3*25.4 # sensor 1: from surface to 3 in
#sd2=6*25.4 # sensor 2: from 6 in to 12 in
#sd3=12*25.4 # sensor 3: from 12 in to 1ft
#sd4=24*25.4 # sensor 4: from 1ft to 2ft
#sd5=36*25.4 # sensor 5: from 2ft to 3ft
#sd6=48*25.4 # sensor 6: from 3ft to 4ft
#sd7=60*25.4 # sensor 7: from 4ft to 5ft


# CORN soil moisture (mm)
sm1=data.101$sm1*sd1/100
sm2=data.101$sm2*sd2/100
sm3=data.101$sm3*(sd3-sd2)/100
sm4=data.101$sm4*(sd4-sd2)/100
sm5=data.101$sm5*(sd5-sd4)/100
sm6=data.101$sm6*(sd6-sd5)/100
sm7=data.101$sm7*(sd7-sd6)/100
sm8=data.101$sm8*(sd8-sd7)/100

# ALFALFA soil moisture (mm)
#sm1=data.101$sm1*sd1/100
#sm2=data.101$sm2*(sd2-sd1)/100
#sm3=data.101$sm3*(sd3-sd2)/100
#sm4=data.101$sm4*(sd4-sd3)/100
#sm5=data.101$sm5*(sd5-sd4)/100
#sm6=data.101$sm6*(sd6-sd5)/100
#sm7=data.101$sm7*(sd7-sd6)/100

# sensor depth 1 ####

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm1[i+1]-sm1[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 1st derivative - soil depth 1', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 1', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor1=data.frame(Date=data.101$Date, SM=sm1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor1$f1[i]<0 & sensor1$f2[i]>0) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}

plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 1', xlab='', ylab='ET (mm)',
     ylim=c(0,4))

# sensor depth 2 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm2[i+1]-sm2[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 1st derivative - soil depth 2', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 2', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor2=data.frame(Date=data.101$Date, SM=sm2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor2$f1[i]<0 & sensor2$f2[i]>0) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

plot(sensor2$Date, sensor2$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET from - depth 2', xlab='', ylab='ET (mm)',
     ylim=c(0,4))

# sensor depth 3 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm3[i+1]-sm3[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 1st derivative - soil depth 3', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 3', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor3=data.frame(Date=data.101$Date, SM=sm3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor3$f1[i]<0 & sensor3$f2[i]>0) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

plot(sensor3$Date, sensor3$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 3', xlab='', ylab='ET (mm)',
     ylim=c(0, 4))

# sensor depth 4 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm4[i+1]-sm4[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 sm4 slope - soil depth 4', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 4', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor4=data.frame(Date=data.101$Date, SM=sm4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor4$f1[i]<0 & sensor4$f2[i]>0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

plot(sensor4$Date, sensor4$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 4', xlab='', ylab='ET (mm)',
     ylim=c(0, 6))

# sensor depth 5 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm5[i+1]-sm5[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 sm5 slope - soil depth 5', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 5', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor5=data.frame(Date=data.101$Date, SM=sm5, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor5$f1[i]<0 & sensor5$f2[i]>0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}

plot(sensor5$Date, sensor5$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 5', xlab='', ylab='ET (mm)',
     ylim=c(0, 4))

# sensor depth 6 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm6[i+1]-sm6[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 sm6 slope - soil depth 6', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 6', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor6=data.frame(Date=data.101$Date, SM=sm6, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor6$f1[i]<0 & sensor6$f2[i]>0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

plot(sensor6$Date, sensor6$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 6', xlab='', ylab='ET (mm)',
     ylim=c(0, 4))

# sensor depth 7 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm7[i+1]-sm7[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 sm7 slope - soil depth 7', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 7', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor7=data.frame(Date=data.101$Date, SM=sm7, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor7$f1[i]<0 & sensor7$f2[i]>0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

plot(sensor7$Date, sensor7$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 7', xlab='', ylab='ET (mm)',
     ylim=c(0, 4))

# sensor depth 8 ####

# 1st derivative - slope of the SM curve
w=3    # window width in days
f1=c() # 1st derivative
f1[1]=NA
for (i in 2:n) {
  f1[i]=(sm8[i+1]-sm8[i-1])/w
}
plot(data.101$Date, f1, 
     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
     main='Corn 215 sm8 slope - soil depth 8', xlab='', ylab='f1')
abline(h=0, lwd=2)

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in 2:n) {
  f2[i]=(f1[i+1]-f1[i-1])/w
}
plot(data.101$Date, f2, 
     type='o', pch=19, cex=0.8, lwd=2, col='brown2', 
     main='Corn 215 2nd derivative - soil depth 8', xlab='', ylab='f2')
abline(h=0, lwd=2)

sensor8=data.frame(Date=data.101$Date, SM=sm8, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor8$ET=c(rep(NA, n)) # create ET column
for (i in 3:(n-3)) {
  if (sensor8$f1[i]<0 & sensor8$f2[i]>0) {sensor8$ET[i]=abs(sensor8$f1[i])} 
}

plot(sensor8$Date, sensor8$ET,
     type='h', lwd=3, col='deepskyblue',
     main='Corn 215 ET - soil depth 8', xlab='', ylab='ET (mm)',
     ylim=c(0, 3))

# Total ET from the entire soil profile ####
sensor1$ET[is.na(sensor1$ET)]=0 # replaces NA values with zero
sensor2$ET[is.na(sensor2$ET)]=0 # replaces NA values with zero
sensor3$ET[is.na(sensor3$ET)]=0 # replaces NA values with zero
sensor4$ET[is.na(sensor4$ET)]=0 # replaces NA values with zero
sensor5$ET[is.na(sensor5$ET)]=0 # replaces NA values with zero
sensor6$ET[is.na(sensor6$ET)]=0 # replaces NA values with zero
sensor7$ET[is.na(sensor7$ET)]=0 # replaces NA values with zero
sensor8$ET[is.na(sensor8$ET)]=0 # replaces NA values with zero

ET.tot=sensor1$ET+
  sensor2$ET+
  sensor3$ET+
  sensor4$ET+
  sensor5$ET+
  sensor6$ET+
  sensor7$ET+
  sensor8$ET

plot(data.101$Date, ET.tot, 
     type='h', lwd=2,
     main='Corn 215 ET from entire soil profile', xlab='', ylab='ET (mm)',
     ylim=c(0,20))


# Heatmap ####
library(ggplot2)
library(hrbrthemes)
library(viridis)

x=data.101$Date
y=paste('sensor', 8:1)
ET=c(sensor8$ET, sensor7$ET, sensor6$ET, sensor5$ET, sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)  #Corn 8 sensors
#ET=c(sensor7$ET, sensor6$ET, sensor5$ET, sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)             #Alfalfa 7 sensors
ET[ET==0]=NA # turns zeros into NAs
heatmap.data=expand.grid(x=x, y=y)
ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
  geom_tile() +
  labs(x='', y='', title='Corn 215 Daily ET (mm)') +
  scale_fill_viridis(direction=-1, na.value='white') +
  theme_ipsum()

