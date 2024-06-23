library(readxl)
Data=read_excel("Data.xlsx")

par(mfrow=c(1,1))
plot(Data$HMSP, type="l",
     ylim=c(1000,6000), lwd=2, col="blue",
     axes=FALSE, ylab="Harga Saham", xlab="Waktu")
labels <- as.numeric(format(as.Date(Data$Date, "%m/%d/%Y"), "%Y"))
where.put <- c(1, which(diff(labels) == 1) + 1)
axis(side=1, at=where.put,
     label=labels[where.put], lwd=0.5)
axis(side=2, at=seq(0,6000, by=2000),
     label=seq(0,6000, by=2000), lwd=1)
lines(Data$JPFA, type="l", ylim=c(1000,6000),
      lwd=2, col="red")
legend("topright", legend=c("JPFA", "HMSP" ), lwd=2,
       col=c("red", "blue"), bg="white")
