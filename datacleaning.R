spread_data <- read.csv("~/Library/CloudStorage/OneDrive-YaleUniversity/SPREAD study/prospective_SPREAD.csv")
colnames(spread_data) <- c(names(spread_data)[1:3],
                           paste0("week",rep(1:48,each=4),rep(c("Bronchiolitis","Test","RSV","comment"),48)))
spread_data <- spread_data[-c(1:2),]
spread_test <- spread_data[48,]
spread_RSVposrate <- spread_data[49,]
spread_data <- spread_data[-c(48:49),]

Bronchiolitis <- as.numeric(spread_test[,grep("Bronchiolitis",colnames(spread_test))])
RSV <- as.numeric(spread_test[,grep("RSV",colnames(spread_test))])
Test <- as.numeric(spread_test[,grep("Test",colnames(spread_test))])
Posrate <- as.numeric(spread_RSVposrate[,grep("RSV",colnames(spread_RSVposrate))])

cor.test(Bronchiolitis,RSV) # 0.9729342 P<0.001

plot(Bronchiolitis,type="l",xlab="week",bty="l",xaxt="n")
lines(RSV,col="red")
lines(Posrate*Bronchiolitis/100,col="blue",lty=2)
legend(35,300, legend=c("Bronchiolitis","RSV","Pos*Bro"),
       col=c("black", "red","blue"),lty=c(1,1,2),cex=0.5,box.lty=0)
legend(18,300, legend=c("correlation=0.8", "P<0.0001"),
       col=c("black"),cex=0.5,box.lty=0)
axis(1, at=seq(3,48,4.3), labels=c("May","Jun","Jul","Aug",
                                 "Sep","Oct","Nov","Dec",
                                 "Jan","Feb","Mar"))

plot(Test/Bronchiolitis,type="l",col="blue",lty=2,xlab="week",ylim=c(0,1))
lines(Posrate/100,col="purple",lty=2)
legend(20,0.36, legend=c("Test proportion", "Positive rate"),
       col=c("blue", "purple"),lty=2,cex=0.5,box.lty=0)
