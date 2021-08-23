# ET estimation for sensor 101 in Wellsville 2021

# Packages ####
library(readxl)
library(writexl)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(tidyverse)

# Load the data and define the variables for sensor 101####
setwd("~/Modena Data") # set the working directory in the folder containing the data file
data.101=read_excel("2021 Data/Modena Alfalfa North.xlsx", sheet="SM Data") # load the SM data from excel
data.101=na.omit(data.101) # eliminate the days with no data

n=length(data.101$Date) # Number of days with data
ETo=data.101$ETo_mm         # Reference ET (mm)
# Sensor depth (mm)
##Depths for Corn 215, 245, 252 
#sd1=3*25.4 # sensor 1: from surface to 3 in
#sd2=6*25.4 # sensor 4: from 3 in to 6 in
#sd3=12*25.4 # sensor 5: from 6 in to 1ft
#sd4=24*25.4 # sensor 6: from 1ft to 2ft
#sd5=36*25.4 # sensor 7: from 2ft to 3ft
#sd6=48*25.4 # sensor 8: from 3ft to 4ft

sd1=4.5*25.4 # sensor 1: from surface to 3 in
sd2=4.5*25.4 # sensor 4: from 3 in to 6 in
sd3=9*25.4 # sensor 5: from 6 in to 1ft
sd4=12*25.4 # sensor 6: from 1ft to 2ft
sd5=12*25.4 # sensor 7: from 2ft to 3ft
sd6=12*25.4 # sensor 8: from 3ft to 4ft
sd7=12*25.4 #alfalfa sensor 7


# Soil moisture - volumetric water content %
sm1=data.101$`1 Acclima`
sm2=data.101$`2 Acclima`
sm3=data.101$`3 Acclima`
sm4=data.101$`4 Acclima`
sm5=data.101$`5 Acclima`
sm6=data.101$`6 Acclima`
sm7=data.101$`7 Acclima`

# Root depth (mm)
rd=data.101$Zr
# plot root depth with sensor depth
plot(data.101$Date, -rd, 
     type='h', col='goldenrod', lwd='3', ylim=c(-1800, 0),
     main='Corn 30 East Root Depth', ylab='Soil depth (mm)', xlab='2021')
abline(h=0, lwd=2)
abline(h=-sd1, col='coral4', lwd=2)
abline(h=-sd2, col='steelblue4', lwd=2)
abline(h=-sd3, col='gold4', lwd=2)
abline(h=-sd4, col='olivedrab4', lwd=2)
abline(h=-sd5, col='coral2', lwd=2)
abline(h=-sd6, col='steelblue1', lwd=2)

legend('bottom', lty=1, lwd=3, legend=c('s1', 's2','s3','s4','3 in','6 in','1 ft','2 ft','s5','s6',NA, NA,'3 ft','4 ft',NA,NA), col = c(NA,NA,NA,NA,'coral4','steelblue4','gold4','olivedrab4',NA,NA,NA,NA,'coral2','steelblue1',NA,NA), ncol=4)

# Plot daily soil moisture data
# Alfalfa North
plot (data.101$Date, sm1, ylim=c(0, 50), type='l', lwd=2, col='coral4', 
      main='Daily SM for Alfalfa North', xlab='2021', ylab='Soil water content (%)') 
lines(data.101$Date, sm2, type='l', lwd=2, col='steelblue4')
lines(data.101$Date, sm3, type='l', lwd=2, col='gold4')
lines(data.101$Date, sm4, type='l', lwd=2, col='olivedrab4')
lines(data.101$Date, sm5, type='l', lwd=2, col='coral2')
lines(data.101$Date, sm6, type='l', lwd=2, col='steelblue1')
lines(data.101$Date, sm7, type='l', lwd=1, col='gold3')
#legend('bottom', lty=1, lwd=3, legend=c('s1 - 3in','s2 - 6in','s3 - 1ft','s4 - 2ft','s5 - 3ft','s6 - 4ft'), col = c('coral4','steelblue4','gold4','olivedrab4','coral2','steelblue1'), ncol=2)
legend('topleft', lty=1, lwd=3, legend=c('s1 - 3in','s2 - 6in','s3 - 1ft','s4 - 2ft','s5 - 3ft','s6 - 4ft', 's7 - 5ft'), col = c('coral4','steelblue4','gold4','olivedrab4','coral2','steelblue1', 'gold3'), ncol=2)


# Method A: Water balance on the soil profile as a whole ####
# This method estimates ET (mm/day) by applying the water balance method to the 
# entire soil profile at once.

# Alfalfa North
# Total water content (mm) of the soil profile
#wc.1=(sm1*sd1+sm2*sd2+sm3*sd3+sm4*sd4+sm5*sd5+sm6*sd6)/100
wc.1=(sm1*sd1+sm2*sd2+sm3*sd3+sm4*sd4+sm5*sd5+sm6*sd6+sm7*sd7)/100

# ET from Alfalfa North
ET.A.1=c()
for(i in 2:n) { 
  ET.A.1[i]=wc.1[i-1]-wc.1[i]}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET.A.1[i]<0) {ET.A.1[i]=NA}}         
plot(data.101$Date, ET.A.1, 
     ylim=c(0,15), type='h', lwd=2, col='darkslategray3',
     main='Method A Daily ET for Alfalfa North', xlab='2021', ylab='Depletion/ET (mm/d)')
lines(data.101$Date, ETo, type='l', xlab='2021', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# ET/ETo for Alfalfa North
#Kc.A.1=ET.A.1/ETo
#plot(data.101$Date, Kc.A.1,
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')

# Method B: Water balance on each sensors soil depth NOT accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method B does not account for root growth throughout the season.

# Alfalfa North

# ET from soil depth 1
ET1.101=c() 
for(i in 2:n) { 
  ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}
for(i in 2:n) { # eliminate negative ET values
  if (ET1.101[i]<0) {ET1.101[i]=NA}}         
#plot(data.101$Date, ET1.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 1', xlab='2021', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c() 
for(i in 2:n) { 
  ET2.101[i]=(sm2[i-1]-sm2[i])/100*sd2}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET2.101[i]<0) {ET2.101[i]=NA}}         
#plot(data.101$Date, ET2.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 2', xlab='2021', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) { 
  ET3.101[i]=(sm3[i-1]-sm3[i])/100*sd3}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET3.101[i]<0) {ET3.101[i]=NA}}         
#plot(data.101$Date, ET3.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 3', xlab='2021', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) { 
  ET4.101[i]=(sm4[i-1]-sm4[i])/100*sd4}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET4.101[i]<0) {ET4.101[i]=NA}}         
#plot(data.101$Date, ET4.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 4', xlab='2021', ylab='ET (mm)')

# ET from soil depth 5
ET5.101=c() 
for(i in 2:n) { 
  ET5.101[i]=(sm5[i-1]-sm5[i])/100*sd5}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET5.101[i]<0) {ET5.101[i]=NA}}         
#plot(data.101$Date, ET5.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 5', xlab='2021', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  ET6.101[i]=(sm6[i-1]-sm6[i])/100*sd6}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET6.101[i]<0) {ET6.101[i]=NA}}         
#plot(data.101$Date, ET6.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 6', xlab='2021', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  ET7.101[i]=(sm7[i-1]-sm7[i])/100*sd7}
for(i in 2:n) { # eliminate negative ET contributions
  if (ET7.101[i]<0) {ET7.101[i]=NA}}         
#plot(data.101$Date, ET7.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 7', xlab='2021', ylab='ET (mm)')

# Total ET for Alfalfa North
ET1.101[is.na(ET1.101)]=0 # Turn NAs into zeroes for the sum
ET2.101[is.na(ET2.101)]=0  
ET3.101[is.na(ET3.101)]=0  
ET4.101[is.na(ET4.101)]=0 
ET5.101[is.na(ET5.101)]=0 
ET6.101[is.na(ET6.101)]=0  
ET7.101[is.na(ET7.101)]=0

ET.B.1=ET1.101+ET2.101+ET3.101+ET4.101+ET5.101+ET6.101+ET7.101
ET.B.1[ET.B.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date, ET.B.1,
     ylim=c(0,15),type='h', col='darkslategray3', lwd=2,
     main='Method B Daily ET for Alfalfa North', xlab='2021', ylab='Depletion/ET (mm/d)')
lines(data.101$Date, ETo, type='l', xlab='2021', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
#ET1.101[ET1.101==0]=NA # Turn zeroes into NAs
#ET2.101[ET2.101==0]=NA
#ET3.101[ET3.101==0]=NA
#ET4.101[ET4.101==0]=NA
#ET5.101[ET1.101==0]=NA 
#ET6.101[ET2.101==0]=NA

#x=data.101$Date
#y=paste('sensor', 8:1)
#ET=c(ET8.101, ET7.101, ET6.101, ET5.101, ET4.101, ET3.101, ET2.101, ET1.101)
#heatmap.data=expand.grid(x=x, y=y)
#ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
#  geom_tile() +
#  labs(x='', y='', title='Daily ET (mm) for Alfalfa North') +
#  scale_fill_viridis(direction=-1, na.value='white') +
#  theme_ipsum()

# ET/ETo for Alfalfa North
#Kc.B.1=ET.B.1/ETo
#plot(data.101$Date, Kc.B.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')


# Method C: Water balance on each sensors soil depth accounting for root growth ####
# This method estimates ET (mm/day) by applying the water balance to each soil
# layer and then adds them all up after having eliminated all negative 
# contributions. Method C takes root growth throughout the season in account.

# Adjusted root depth: the average with previous day is used for the calculations.
RD=c()      
for(i in 2:n) {
  RD[i]=(rd[i-1]+rd[i])/2} # average root depth of yesterday and today

# ET from soil depth 1
ET1.101=c()
for(i in 2:n) {
  if (RD[i] < sd1) {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*RD[i] }
  else {
    ET1.101[i]=(sm1[i-1]-sm1[i])/100*sd1}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET1.101[i]<0) {ET1.101[i]=NA}}         

#plot(data.101$Date, ET1.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 1', xlab='2021', ylab='ET (mm)')

# ET from soil depth 2
ET2.101=c()
for(i in 2:n) {
  if (RD[i] <= sd1) {
    ET2.101[i]=0}
  else if (sd1 < RD[i] & RD[i] <= sd2+sd1) {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(RD[i]-sd1) }
  else {
    ET2.101[i]=(sm2[i-1]-sm2[i])/100*(sd2) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET2.101[i]<0) {ET2.101[i]=NA}}    
ET2.101[ET2.101==0]=NA # Turns zeros into NAs

#plot(data.101$Date, ET2.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 2', xlab='2021', ylab='ET (mm)')

# ET from soil depth 3
ET3.101=c()
for(i in 2:n) {
  if (RD[i] <= sd2+sd1) {
    ET3.101[i]=0}
  else if (sd2+sd1 < RD[i] & RD[i] <= sd3+sd2+sd1) {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(RD[i]-sd2-sd1) }
  else {
    ET3.101[i]=(sm3[i-1]-sm3[i])/100*(sd3) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET3.101[i]<0) {ET3.101[i]=NA} }   
ET3.101[ET3.101==0]=NA # Turns zeros into NAs

#plot(data.101$Date, ET3.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 3', xlab='2021', ylab='ET (mm)')

# ET from soil depth 4
ET4.101=c()
for(i in 2:n) {
  if (RD[i] <= sd3+sd2+sd1) {
    ET4.101[i]=0}
  else if (sd1+sd2+sd3 < RD[i] & RD[i] <= sd4+sd3+sd2+sd1) {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(RD[i]-sd3-sd2-sd1) }
  else {
    ET4.101[i]=(sm4[i-1]-sm4[i])/100*(sd4) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET4.101[i]<0) {ET4.101[i]=NA} }         
ET4.101[ET4.101==0]=NA # Turns zeros into NAs

#plot(data.101$Date, ET4.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 4', xlab='2021', ylab='ET (mm)')

# ET from soil depth 5
ET5.101=c()
for(i in 2:n) { 
  if (RD[i] < sd4+sd3+sd2+sd1) {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*RD[i] }
  else if (sd1+sd2+sd3+sd4 < RD[i] & RD[i] <= sd5+sd4+sd3+sd2+sd1) {
    ET4.101[i]=(sm5[i-1]-sm5[i])/100*(RD[i]-sd4-sd3-sd2-sd1) }
  else {
    ET5.101[i]=(sm5[i-1]-sm5[i])/100*sd5}}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET5.101[i]<0) {ET5.101[i]=NA}} 
ET5.101[ET5.101==0]=NA # Turns zeros into NAs

#plot(data.101$Date, ET5.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 5', xlab='2021', ylab='ET (mm)')

# ET from soil depth 6
ET6.101=c()
for(i in 2:n) {
  if (RD[i] <= sd5+sd4+sd3+sd2+sd1) {
    ET6.101[i]=0}
  else if (sd1+sd2+sd3+sd4+sd5 < RD[i] & RD[i] <= sd6+sd5+sd4+sd3+sd2+sd1) {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(RD[i]-sd5-sd4-sd3-sd2-sd1) }
  else {
    ET6.101[i]=(sm6[i-1]-sm6[i])/100*(sd6) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET6.101[i]<0) {ET6.101[i]=NA}}  
ET6.101[ET6.101==0]=NA

#plot(data.101$Date, ET6.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 6', xlab='2021', ylab='ET (mm)')

# ET from soil depth 7
ET7.101=c()
for(i in 2:n) {
  if (RD[i] <= sd6+sd5+sd4+sd3+sd2+sd1) {
    ET7.101[i]=0}
  else if (sd1+sd2+sd3+sd4+sd5+sd6 < RD[i] & RD[i] <= sd7+sd6+sd5+sd4+sd3+sd2+sd1) {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(RD[i]-sd6-sd5-sd4-sd3-sd2-sd1) }
  else {
    ET7.101[i]=(sm7[i-1]-sm7[i])/100*(sd7) }}
# eliminate negative ET contributions
for(i in 2:n) { 
  if (ET7.101[i]<0) {ET7.101[i]=NA}}  
ET7.101[ET7.101==0]=NA

#plot(data.101$Date, ET7.101, 
#     type='h', lwd=2, ylim=c(0, 10),
#     main='ET from soil depth 7', xlab='2021', ylab='ET (mm)')

# Total ET for Alfalfa North
ET1.101[is.na(ET1.101)]=0 # Turn NAs into zeroes for the sum
ET2.101[is.na(ET2.101)]=0  
ET3.101[is.na(ET3.101)]=0  
ET4.101[is.na(ET4.101)]=0 
ET5.101[is.na(ET5.101)]=0 # Turn NAs into zeroes for the sum
ET6.101[is.na(ET6.101)]=0  
ET7.101[is.na(ET7.101)]=0

ET.C.1=ET1.101+ET2.101+ET3.101+ET4.101+ET5.101+ET6.101+ET7.101
ET.C.1[ET.C.1==0]=NA # Turn zeroes into NAs

plot(data.101$Date, ET.C.1,
     ylim=c(0, 15),
     type='h', col='darkslategray3', lwd=2,
     main='Method C Daily ET for Alfalfa North', xlab='2021', ylab='Depletion/ET (mm/d)')
lines(data.101$Date, ETo, type='l', xlab='2021', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
#x=data.101$Date
#y=paste('sensor', 8:1)
#ET=c(ET8.101, ET7.101, ET6.101, ET5.101, ET4.101, ET3.101, ET2.101, ET1.101)
#ET[ET==0]=NA # turns zeros into NAs
#heatmap.data=expand.grid(x=x, y=y)
#ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
#  geom_tile() +
#  labs(x='', y='', title='Daily ET (mm)') +
#  scale_fill_viridis(direction=-1, na.value='white') +
#  theme_ipsum()

# ET/ETo for Alfalfa North
#Kc.C.1=ET.C.1/ETo
#plot(data.101$Date, Kc.C.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')

# Method D: Derivatives on the soil profile as a whole ####
# This method estimates ET (mm/day) by analyzing the first and second derivatives
# of the soil moisture plots. When the first derivative is negative and the second
# derivative is positive we are assuming that water is leaving the system only via 
# evapotranspiration at a rate equal to the slope of the soil moisture graph.

w=3 # window used to calculate the derivatives in days

# Alfalfa North
# Total water content (mm) of the soil profile
wc.1=(sm1*sd1+sm2*sd2+sm3*sd3+sm4*sd4+sm5*sd5+sm6*sd6+sd7*sm7)/100

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=wc.1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}

# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for Alfalfa North', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date, SM=wc.1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
plot(sensor1$Date, sensor1$ET,
     type='h', lwd=3, col='darkslategray3',
     main='Method D Daily ET for Alfalfa North', xlab='', ylab='ET (mm)',
     ylim=c(0,15))
lines(data.101$Date, ETo, type='l', xlab='2019', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

ET.D.1=sensor1$ET

# ET/ETo for Alfalfa North
#Kc.D.1=ET.D.1/ETo
#plot(data.101$Date, Kc.D.1,
#     ylim=c(0, 5),
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')


# Method E: Derivatives NOT accounting for root growth####
# Same as method D but on each soil layer individually and then summed up and not accounting for
# root growth over the season.

w=3 # window used to calculate the derivatives in days

# Sub station 1

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  #  f1[i]=(SM1[i+1]-SM1[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}
#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 1', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
#plot(sensor1$Date, sensor1$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 1', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  #  f1[i]=(SM2[i+1]-SM2[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
  
}
#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 2', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor2$f1[i]<0 ) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

#plot(sensor2$Date, sensor2$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 2', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  #f1[i]=(SM3[i+1]-SM3[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  f2[i]=reg$coefficients[2]
}
#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 3', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor3$f1[i]<0 ) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

# plot(sensor3$Date, sensor3$ET,
#      type='h', lwd=2,
#      main='ET - soil depth 3', xlab='', ylab='ET (mm)',
#      ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  # f1[i]=(SM4[i+1]-SM4[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

# plot(data.101$Date.101, f1, 
#      type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#      ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#      main='SM derivatives for sensor 4', xlab='', ylab='')
# lines(data.101$Date.101, f2, 
#       type='o', pch=19, cex=0.8, lwd=2, col='brown2')
# abline(h=0, lwd=2)
# legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor4$f1[i]<0) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

#plot(sensor4$Date, sensor4$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 4', xlab='2021', ylab='ET (mm)',
#     ylim=c(0, 5))


# soil depth 5
SM5=sm5*sd5/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  # f1[i]=(SM4[i+1]-SM4[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 5', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor5=data.frame(Date=data.101$Date, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor5$f1[i]<0) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}
#plot(sensor5$Date, sensor5$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 5', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6-sd5)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  # f1[i]=(SM4[i+1]-SM4[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 6', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor6=data.frame(Date=data.101$Date, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor6$f1[i]<0) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

#plot(sensor6$Date, sensor6$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 2', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7-sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  # f1[i]=(SM4[i+1]-SM4[i-1])/w
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
  
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 7', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor7=data.frame(Date=data.101$Date, SM=SM7, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor7$f1[i]<0) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

#plot(sensor7$Date, sensor7$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 2', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# Alfalfa North

# Total ET from the entire soil profile
sensor1$ET[is.na(sensor1$ET)]=0 # turn NA values with zero
sensor2$ET[is.na(sensor2$ET)]=0 # turn NA values with zero
sensor3$ET[is.na(sensor3$ET)]=0 # turn NA values with zero
sensor4$ET[is.na(sensor4$ET)]=0 # turn NA values with zero
sensor5$ET[is.na(sensor5$ET)]=0 # turn NA values with zero
sensor6$ET[is.na(sensor6$ET)]=0 # turn NA values with zero
sensor7$ET[is.na(sensor7$ET)]=0 

ET.E.1=sensor1$ET+
  sensor2$ET+
  sensor3$ET+
  sensor4$ET+
  sensor5$ET+
  sensor6$ET+
  sensor7$ET

ET.E.1[ET.E.1==0]=NA # turn zeros into NA
plot(data.101$Date, ET.E.1, 
     type='h', lwd=2, col='darkslategray3',
     main='Method E Daily ET for Alfalfa North', xlab='2021', ylab='ET (mm)',
     ylim=c(0,15))
lines(data.101$Date, ETo, type='l', xlab='2021', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
#x=data.101$Date
#y=paste('sensor', 8:1)
#ET=c(sensor8$ET, sensor7$ET, sensor6$ET, sensor5$ET, sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)
#ET[ET==0]=NA # turns zeros into NAs
#heatmap.data=expand.grid(x=x, y=y)
#ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
#  geom_tile() +
#  labs(x='', y='', title='Daily ET (mm)') +
#  scale_fill_viridis(direction=-1, na.value='white') +
#  theme_ipsum()

# ET/ETo for Alfalfa North
#Kc.E.1=ET.E.1/ETo
#plot(data.101$Date, Kc.E.1,
#     ylim=c(0, 2),
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')


# Method F: Derivatives accounting for root growth ####
# Same as method F but accounting for root growth over the season.

w=3 # window used to calculate the derivatives in days

# soil depth 1
SM1=sm1*sd1/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 1', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor1=data.frame(Date=data.101$Date, SM=SM1, f1, f2) # data frame with the results for sensor1

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor1$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor1$f1[i]<0 ) {sensor1$ET[i]=abs(sensor1$f1[i])} 
}
#plot(sensor1$Date, sensor1$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 1', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 2
SM2=sm2*(sd2)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM2[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 2', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor2=data.frame(Date=data.101$Date, SM=SM2, f1, f2) # data frame with the results for sensor2

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor2$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor2$f1[i]<0 ) {sensor2$ET[i]=abs(sensor2$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd1) 
  {sensor2$ET[i]=NA}
}

#plot(sensor2$Date, sensor2$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 2', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 3
SM3=sm3*(sd3)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM3[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 3', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor3=data.frame(Date=data.101$Date, SM=SM3, f1, f2) # data frame with the results for sensor3

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor3$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor3$f1[i]<0 ) {sensor3$ET[i]=abs(sensor3$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd2+sd1) 
  {sensor3$ET[i]=NA}
}

#plot(sensor3$Date, sensor3$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 3', xlab='2021', ylab='ET (mm)',
#     ylim=c(0, 5))

# soil depth 4
SM4=sm4*(sd4)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM4[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 4', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor4=data.frame(Date=data.101$Date, SM=SM4, f1, f2) # data frame with the results for sensor4

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor4$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor4$f1[i]<0 ) {sensor4$ET[i]=abs(sensor4$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd3+sd2+sd1) 
  {sensor4$ET[i]=NA}
}

#plot(sensor4$Date, sensor4$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 4', xlab='2021', ylab='ET (mm)',
#     ylim=c(0, 5))

# soil depth 5
SM5=sm5*sd5/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM5[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 5', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor5=data.frame(Date=data.101$Date, SM=SM5, f1, f2) # data frame with the results for sensor5

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor5$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor5$f1[i]<0 ) {sensor5$ET[i]=abs(sensor5$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd4+sd3+sd2+sd1) 
  {sensor5$ET[i]=NA}
}

#plot(sensor5$Date, sensor5$ET,
#     type='h', lwd=2,
#     main='ET - soil depth 5', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 6
SM6=sm6*(sd6)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM6[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 6', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor6=data.frame(Date=data.101$Date, SM=SM6, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor6$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor6$f1[i]<0 ) {sensor6$ET[i]=abs(sensor6$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd5+sd4+sd3+sd2+sd1) 
  {sensor6$ET[i]=NA}
}


#plot(sensor6$Date, sensor6$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 6', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# soil depth 7
SM7=sm7*(sd7)/100 # soil moisture (mm)

# 1st derivative - slope of the SM curve
f1=c() # 1st derivative
f1[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=SM7[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  #  f1[i]=(wc.1[i+1]-wc.1[i-1])/w
  f1[i]=reg$coefficients[2]
}
# 2nd derivative - calculate the inflection point
f2=c() # 2nd derivative
f2[1]=NA
for (i in ((w+1)/2):n) {
  low_val=i-(w-1)/2
  hig_val=i+(w-1)/2
  y=f1[low_val:hig_val]
  x=1:w
  reg=lm(y~x)
  
  #  f2[i]=(f1[i+1]-f1[i-1])/w
  f2[i]=reg$coefficients[2]
}

#plot(data.101$Date, f1, 
#     type='o', pch=19, cex=0.8, lwd=2, col='darkgoldenrod', 
#     ylim=c(min(c(f1,f2),na.rm=T),max(c(f1,f2), na.rm=T)),
#     main='SM derivatives for sensor 6', xlab='2021', ylab='')
#lines(data.101$Date, f2, 
#      type='o', pch=19, cex=0.8, lwd=2, col='brown2')
#abline(h=0, lwd=2)
#legend('topleft', legend=c('1st derivative', '2nd derivative'), col=c('darkgoldenrod', 'brown2'), pch=19, lwd=2, inset=0.01)

sensor7=data.frame(Date=data.101$Date, SM=SM7, f1, f2) # data frame with the results for sensor6

# Estimate ET rate - if f1<0 & f2>0: ET=|f1|
sensor7$ET=c(rep(NA, n)) # create ET column
for (i in w:(n-w)) {
  if (sensor7$f1[i]<0 ) {sensor7$ET[i]=abs(sensor7$f1[i])} 
}

for (i in 1:n) {
  if (data.101$Zr[i]<sd6+sd5+sd4+sd3+sd2+sd1) 
  {sensor7$ET[i]=NA}
}


#plot(sensor6$Date, sensor6$ET,
#     type='h', lwd=2,
#     main='ET from - soil depth 6', xlab='2021', ylab='ET (mm)',
#     ylim=c(0,5))

# Alfalfa North

# Total ET from the entire soil profile
sensor1$ET[is.na(sensor1$ET)]=0 # turn NA values into zeros
sensor2$ET[is.na(sensor2$ET)]=0 # turn NA values into zeros
sensor3$ET[is.na(sensor3$ET)]=0 # turn NA values into zeros
sensor4$ET[is.na(sensor4$ET)]=0 # turn NA values into zeros
sensor5$ET[is.na(sensor5$ET)]=0 # replaces NA values with zero
sensor6$ET[is.na(sensor6$ET)]=0 # replaces NA values with zero
sensor7$ET[is.na(sensor7$ET)]=0 

ET.F.1=sensor1$ET+
  sensor2$ET+
  sensor3$ET+
  sensor4$ET+
  sensor5$ET+
  sensor6$ET+
  sensor7$ET

ET.F.1[ET.F.1==0]=NA # turn zeros into NAs
plot(data.101$Date, ET.F.1, 
     type='h', lwd=2, col='darkslategray3',
     main='Method F Daily ET for Alfalfa North', xlab='2021', ylab='ET (mm)',
     ylim=c(0,15))
lines(data.101$Date, ETo, type='l', xlab='2021', ylab='Reference ET (mm)', col='palegreen3', lwd=3)
legend('topright', legend=c('ETo', 'ET'), col=c('palegreen3', 'darkslategray3'), lwd=2, inset=0.02)

# Heatmap
#x=data.101$Date
#y=paste('sensor', 8:1)
#ET=c(sensor8$ET, sensor7$ET, sensor6$ET, sensor5$ET, sensor4$ET, sensor3$ET, sensor2$ET, sensor1$ET)
#ET[ET==0]=NA # turns zeros into NAs
#heatmap.data=expand.grid(x=x, y=y)
#ggplot(heatmap.data, mapping=aes(x, y, fill=ET),) +
#  geom_tile() +
#  labs(x='', y='', title='Daily ET (mm)') +
#  scale_fill_viridis(direction=-1, na.value='white') +
#  theme_ipsum()

# ET/ETo ~ crop coeficcient
#Kc.F.1=ET.F.1/ETo
#plot(data.101$Date, Kc.F.1,
#     ylim=c(0, 1),
#     type='h', lwd=2, col='orchid3',
#     main='Daily ET/ETo for Alfalfa North', xlab='2021', ylab='Kc')


# Comparison of the methods ####

# Alfalfa North
method=rep(c('A','B','C','D','E','F'), each=n)
ET=c(ET.A.1, ET.B.1, ET.C.1, ET.D.1, ET.E.1, ET.F.1) # Evapotranspiration
#Kc=c(Kc.A.1, Kc.B.1, Kc.C.1, Kc.D.1, Kc.E.1, Kc.F.1) # ET/ETos
comparison.1=data.frame(date=data.101$Date, method, ET)

# Box-plot for ET
ggplot(comparison.1, aes(x = method, y = ET, fill = date)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Daily ET method comparison for Alfalfa North") +
  xlab("")+ylab("ET (mm)")

# Box-plot for ET/ETo
#ggplot(comparison.1, aes(x = method, y = Kc, fill = date)) +
#  geom_boxplot() +
#  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    plot.title = element_text(size=11)
#  ) +
#  ggtitle("Daily ET/ETo method comparison") +
#  xlab("") + ylab("ET/ETo")

# Save the results ####
# Write an excel spreadsheet with the results for ET and ET/ETo (~Kc) for all 6 methods

# Alfalfa North
Results.1=data.frame(Date=data.101$Date, ET.A.1, ET.B.1, ET.C.1, ET.D.1, ET.E.1, ET.F.1)
write_xlsx(Results.1, path="Allen/Soil Moisture/2021/Corn/ET Alfalfa North.xlsx") # write the excel with the daily data
