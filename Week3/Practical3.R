data24 <-  read.csv("temps2012_24hr.txt")
data72 <-  read.csv("temps2012_72hr.txt")

plot(data24$day, data24$fcst,
     xlab = "Day", ylab = "Forecast Temperature")
plot(data24$day, data24$obs,
     xlab = "Day", ylab = "Observed Temperature", col = "red")
plot(data72$day, data72$obs,
     xlab = "Day", ylab = "Observed Temperature", col = "blue")

plot(data24$fcst, data24$obs,
     xlab = "Forecast", ylab = "Temperature", col = "red")
abline(0,1, col = "black")
plot(data72$fcst, data72$obs,
     xlab = "Forecast", ylab = "Temperature", col = "blue")
abline(0,1, col = "black")

plot(data24$day, data24$obs - data24$fcst,
     xlab = "Day", ylab = "Forecast Error")

plot(data24$fcst, data72$fcst)

sum(is.na(data24$obs == data72$obs))

is.na(data24$fcst)
sum(is.na(data24$fcst))
sum(is.na(data24))
sum(is.na(data72))

# So 19 missing values in both dataframes, but these aren't necessarily the same
# missing data.

good_days <- !(is.na(data24$obs) | is.na(data24$fcst) | is.na(data72$fcst) | is.na(data72$obs))
data24 <- data24[good_days,]
data72 <- data72[good_days,]

##### Exercise 2

bias24 <- mean(data24$fcst) - mean(data24$obs)
bias72 <- mean(data72$fcst) - mean(data72$obs)

IQR(data24$fcst)
IQR(data72$fcst)
IQR(data24$obs)
# So the IQR of the 24 hr dataset is similar to that of the observations, 
# and the IQR of the 72 h dataset is slightly higher,

sd(data24$fcst)
sd(data72$fcst)
sd(data24$obs)
# So standard deviations are similar, but with slightly lower sd in the 72 hr.

hist(data24$fcst, breaks = 30,
     xlab = "24 hour forecast", main = "Histogram of 24 hour forecasts")
hist(data24$obs - data24$fcst, 
     xlab = "24 hour forecast", main = "Histogram of 24 hour forecast errors")
hist(data72$fcst, breaks = 30,
     xlab = "72 hour forecast", main = "Histogram of 72 hour forecasts")
hist(data72$obs - data72$fcst, 
     xlab = "24 hour forecast", main = "Histogram of 72 hour forecast errors")

cor(data24$obs, data24$fcst)
cor(data72$obs, data72$fcst)
# So correlation is greater for the 24 hour forecast.

sqrt(mean((data24$obs - data24$fcst)**2))
sqrt(mean((data72$obs - data72$fcst)**2))

#### Exercise 3
threshold <- 19.5
obs_warm <- (data24$obs >= threshold)
fcst24_warm <- (data24$fcst >= threshold)
fcst72_warm <- (data72$fcst >= threshold)

# Contingency Table values for 24 Hr forecast
a24 <- sum(fcst24_warm & obs_warm)
b24 <- sum(fcst24_warm & !obs_warm)
c24 <- sum(!fcst24_warm & obs_warm)
d24 <- sum(!fcst24_warm & !obs_warm)

# Contingency Table values for 24 Hr forecast
a72 <- sum(fcst72_warm & obs_warm)
b72 <- sum(fcst72_warm & !obs_warm)
c72 <- sum(!fcst72_warm & obs_warm)
d72 <- sum(!fcst72_warm & !obs_warm)

accuracy <- function(a, b, c, d){
  return((a + d) / (a + b + c + d) )
}
acc_24 <- accuracy(a24, b24, c24, d24)
acc_72 <- accuracy(a72, b72, c72, d72)
# So accuracy of 24hr forecast is greater than 72 hr forecast.

pod <- function(a, c){
  return(a /(a + c))
}
pod_24 <- pod(a24, c24)
pod_72 <- pod(a72, c72)
# The prob of detection for the 24 hour forecast is notably higher than the 72hr

pofd <- function(b, d){
  return(b / (b + d))
}
pofd_24 <-  pofd(b24, d24)
pofd_72 <- pofd(b72, d72)
# 24 hour forecast has fewer misses.

far <- function(a, b){
  return(b / (a + b))
}
far_24 <- far(a24, b24)
far_72 <- far(a72, b72)
# There is a slight higher false alarm ration for the 72hr forecast.

csi <- function(a, b, c){
  return(a / (a + b + c))
}
csi_24 <- csi(a24, b24, c24)
csi_72 <- csi(a72, b72, c72)
#Higher csi for 24 hr forecast indicates a beter forecast.

bias <- function(a, b, c){
  return((a + b) / (a + c))
}
bias_24 <- bias(a24, b24, c24)
bias_72 <- bias(a72, b72, c72)

# Both forecasts have a negative bias, 
# but the 72 hr forecast has a greater magnitude of bias.

hss <- function(a, b, c, d) {
  num <- 2 * (a * d - b * c)
  denom <- (a + c) * (c + d) + (a + b) * (b + d)
  return(num/denom)
}
hss_24 <- hss(a24, b24, c24, d24)
hss_72 <- hss(a72, b72, c72, d72)
# So the Heidke skill score of the 24 hour forecast is greater, indicating that 
#the 24 hour forecast has greater skill compared to climatology.
# However the 72 hour forecast also has considerable skill compared to climatology.

