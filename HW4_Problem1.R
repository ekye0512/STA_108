CH03PR15 <- read.table("~/Documents/Classroom/STA 108/CH03PR15.txt", quote="\"", comment.char="")
View(CH03PR15)



X<-CH03PR15[,2]
Y<-CH03PR15[,1]


xy.lm=lm(Y~X)
summary(xy.lm)


Y.hat=xy.lm$fitted.values
coeff=xy.lm$coefficients
resid=xy.lm$residuals


plot(X,Y,cex=0.5)
abline(coeff, col='blue')
plot(X, xy.lm$residuals, 
     ylab = "Residuals", 
     main = "Residuals vs X")


n <- length(X)
Xbar = sum(X)/n
Ybar = sum(Y)/n

aov.reg <- anova(xy.lm)
aov.reg

#### group the data by X values ####

Y.groupbyX <- split(Y, X)

Xj <- names(Y.groupbyX)

nj <- lapply(Y.groupbyX, length)
nj = unlist(nj) # a vector of group sample size 



#### Take the group average ####

Yj.bar <- lapply(Y.groupbyX, mean)


#### Testing for lack of fit ANOVA table #### 

c <- length(Xj)

SSTO <- aov.reg$`Sum Sq`[1] + aov.reg$`Sum Sq`[2]

SSPE <- lapply(Xj, function(xj){
  yij = Y.groupbyX[[xj]]
  yj.bar = Yj.bar[[xj]]
  return(sum((yij - yj.bar)^2))
})
SSPE = sum(unlist(SSPE))

SSEred <- aov.reg$`Sum Sq`[2]

SSLF <- SSEred - SSPE

df.SSPE <- n - c

df.SSLF <- c - 2

MSLF <- SSLF / df.SSLF

MSPE <- SSPE / df.SSPE

F.stat <- MSLF / MSPE


#### Compare with critical value #### 

alpha <- .025
cr.val <- qf(p = 1 - alpha, df1 = df.SSLF, df2 = df.SSPE)






#problem 2
qt(.9875, 43, lower.tail = TRUE)


