# ===== LOCAL REGRESSION

# ===== Full Dataset
data = cars
colnames(data) = c('x','y')
plot(data$x, data$y,
     main = 'Plot Between Speed and Distances of Cars',
     xlab = 'Speed',
     ylab = 'Distances')

# Making Model Local Regresion for Dataset Cars
loessmod14 = loess(y~x, data = data, span = 0.4)
loessmod15 = loess(y~x, data = data, span = 0.5)
loessmod19 = loess(y~x, data = data, span = 0.9)

#Get Smooth Output
smoothed14 = predict(loessmod14)
smoothed15 = predict(loessmod15)
smoothed19 = predict(loessmod19)

#Plot
lines(smoothed14, x = data$x, col = 'red', lwd = 3)
lines(smoothed15, x = data$x, col = 'green', lwd = 3)
lines(smoothed19, x = data$x, col = 'blue', lwd = 3)

# ===== 10 rows of Dataset
data2 = cars[1:10,]
colnames(data2) = c('x','y')
plot(data2$x, data2$y,
     main = 'Plot Between Speed and Distances of Cars',
     xlab = 'Speed',
     ylab = 'Distances')

# Making Model Local Regresion for Dataset Cars
loessmod24 = loess(y~x, data = data2, span = 0.2)
loessmod25 = loess(y~x, data = data2, span = 0.5)
loessmod29 = loess(y~x, data = data2, span = 0.9)

# Get Smooth Output
smoothed24 = predict(loessmod24)
smoothed25 = predict(loessmod25)
smoothed29 = predict(loessmod29)

# Plot
lines(smoothed24, x = data2$x, col = 'red', lwd = 3)
lines(smoothed25, x = data2$x, col = 'green', lwd = 3)
lines(smoothed29, x = data2$x, col = 'blue', lwd = 3)

# ===== LOESS Algorithm
x = c(0.5578,2.0217,2.5773,3.4140,4.3014,4.7448,5.1074,6.5412,6.7216,7.2601)
y = c(17.7780,108.6017,142.2226,166.9969,205.8401,217.6891,224.9667,232.0901,231.1128,226.4877)
data_xy = cbind(x,y)

loess_audhi = function(data, q){
  data = as.data.frame(data)
  length_data = dim(data)[1]
  sort(data[,1], decreasing = FALSE)
  difference_x = matrix(0, ncol = length_data, nrow = length_data)
  value_h = matrix(0, ncol = length_data, nrow = length_data)
  
  for (i in 1:length_data) {
    focal_point = data[i,1]
    difference_x[,i] = data[,1] - focal_point
    
    class_size = q*length_data
    if (i <= class_size-1) {
      value_h[,i] = max(difference_x[1:class_size,i])
    } 
    
    else {
      for (j in 1:length_data/class_size) {
        if ((i/class_size) > (i %/% class_size) & (i/class_size) < j) {
          value_h[,i] = max(difference_x[(class_size*(i %/% class_size) + 1):j*class_size,i])
        } 
        
        if (i/class_size == j) {
          if (abs(difference_x[i-1,i]) < abs(difference_x[i+1,i])) {
            value_h[,i] = max(difference_x[(class_size*((i-1) %/% class_size) + 1):j*class_size,i])
          }
          if (abs(difference_x[i-1,i]) > abs(difference_x[i+1,i])){
            value_h[,i] = max(difference_x[(class_size*((i+1) %/% class_size) + 1):j*class_size,i])
          }
        }
        
      }
      
    }
  }
  return(value_h)
  return(difference_x)
}