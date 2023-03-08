CH01PR20 <- read.table("~/Documents/Classroom/STA 108/CH01PR20.txt", quote="\"", comment.char="")
View(CH01PR20)

X<-CH01PR20[,2]
Y<-CH01PR20[,1]
n = nrow(CH01PR20)




Xbar = sum(X)/n
Ybar = sum(Y)/n

resid = xy.lm$residuals

XpY = t(X) %*% Y 
XpX = t(X) %*% X
YpY = t(Y) %*% Y


SSE = t(resid) %*% resid
MSE = SSE / (n-2)
sqrt(MSE)

beta1_hat = (XpY - n * Xbar * Ybar) / (XpX - n * Xbar^2)
beta0_hat = Ybar - beta1_hat * Xbar


Xh = c(3, 5, 7)
SE.Yh.hat = sqrt(MSE * (1/n + (Xh - Xbar)^2 / (XpX - n * Xbar^2)))


alpha = 0.10
W <- sqrt(2 * qf(p = 1 - alpha, df1 = 2 , df2 = n - 2))


g=2
bon_mult <- qt(1 - alpha/2/g, df = n-2) 
scheffe <- sqrt(g * qf(p = 1 - alpha, g, df2 = n - 2))

Xh.pred = c(4,7)
SE.Yh.pred.hat = sqrt(MSE * (1 + 1/n + (Xh.pred - Xbar)^2 / (XpX - n * Xbar^2)))

b1_curly<- XpY/XpX

sse_curly<-YpY- ((b1_curly)^2*XpX)

mse_curly<-sse_curly/(n-1)



 qt(1 - alpha/2, df = n-1) 
 
 
 
 
 standard_error_new<-sqrt(mse_curly[1+(36/XpX)])
 


st_err_b1_curly <- sqrt(mse_curly/XpX)



