dat <- read.table("~/Documents/Classroom/STA 108/CH01PR22_data.txt", quote="\"", comment.char="")
View(dat)

n=nrow(dat)

colnames(dat)<-c('Y','X')

X<-dat$X
Y<-dat$Y
Xbar=sum(X)/n
Ybar=sum(Y)/n

xy.lm=lm(Y~X)
summary(xy.lm)

XpY = t(X) %*% Y 
XpX = t(X) %*% X
YpY = t(Y) %*% Y

resid = xy.lm$residuals
SSE = t(resid) %*% resid
MSE = SSE / (n-2)
sqrt(MSE)


XpX - n * Xbar^2

beta0_hat = xy.lm$coef[1]
beta1_hat = xy.lm$coef[2]
resid = xy.lm$residuals

Y_new=238
X_new_hat=(Y_new-beta0_hat)/beta1_hat
 
std_err_calib = sqrt(MSE/beta1_hat^2) * sqrt(1 + 1/n + (X_new_hat - mean(X))^2/sum((X-mean(X))^2))


alpha=0.01
qt(1 - alpha/2,14) 

calib_interval = X_new_hat  + c(-1,1)*qt(1-0.01/2,n-2)*std_err_calib


