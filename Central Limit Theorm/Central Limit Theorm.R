Dadu <- sample(1:6,10000, replace= TRUE)
hist(Dadu, col ="light blue", main="Histogram Keluaran Dadu", xlab="Keluaran Dadu", ylab="Frekuensi")
abline(v=3.5, col = "red",lty=1)


x30 <- c()
x100 <- c()
x1000 <- c()
k =10000
for ( i in 1:k){
  x30[i] = mean(sample(1:6,30, replace = TRUE))
  x100[i] = mean(sample(1:6,100, replace = TRUE))
  x1000[i] = mean(sample(1:6,1000, replace = TRUE))
}
par(mfrow=c(1,3))
hist(x30, col ="green",main="n=30",xlab ="Pelemparan Dadu", ylab="Frekuensi")
abline(v = mean(x30), col = "blue")

hist(x100, col ="light blue", main="n=100",xlab ="Pelemparan Dadu", ylab="Frekuensi")
abline(v = mean(x100), col = "red")

hist(x1000, col ="orange",main="n=1000",xlab ="Pelemparan Dadu", ylab="Frekuensi")
abline(v = mean(x1000), col = "red")