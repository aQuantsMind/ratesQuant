
#create vectors for maturity, period, Zero price, and yield
time_t <- c(seq(from=0.5,to=10,by=0.5))
period_i <- c(seq(from=1,to=20))
Zeros <- c(97.1114, 94.0013, 90.8556, 87.7708, 84.792, 81.9361, 79.2646, 76.5931, 74.1444, 71.6956, 69.4367, 67.1778, 65.0814, 62.985, 61.0308, 59.0766, 57.2499, 55.4232, 53.7127, 52.0022)
yield <- c()

for (i in 1:(length(Zeros))){
  yield[i] <- -(1/time_t[i])*log(Zeros[i]/100)
}
#safety check: find Zero prices computationally
yieldcurve <- rbind(time_t,period_i, Zeros, yield)
colnames(yieldcurve) <-c(1:20)
yieldcurve <- as.data.frame(yieldcurve)

