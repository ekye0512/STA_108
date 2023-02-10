CH01PR20 <- read.table("~/Documents/Classroom/STA 108/CH01PR20.txt", quote="\"", comment.char="")
View(CH01PR20)
#import data set

#set our x and y values
X<-CH01PR20[,2]
Y<-CH01PR20[,1]

#plot our points
plot(X,Y,cex=0.5)

#plotting our Y bar value
Ybar=sum(Y)/n
abline(h=Ybar, col='red')

#plotting our estimated regression function
xy.lm=lm(Y~X)
coeff=xy.lm$coefficients
abline(coeff, col='blue')

n=nrow(CH01PR20)

#### New ANOVA table, based of discussion #### 
aov.reg <- anova(xy.lm)
aov.reg

#calculating SSR and SSE and SSTO
SSE <- aov.reg$`Sum Sq`[2]
sum((xy.lm$residuals)^2)
SSR <- aov.reg$`Sum Sq`[1]
SSTO = sum((Y - Ybar)^2)


#setting the degress of freedom 
df.SSE <- aov.reg$Df[2]
df.SSR <- aov.reg$Df[1]

#getting MSE and MSR values
MSE <- aov.reg$`Mean Sq`[2]
SSE / df.SSE
MSR <- aov.reg$`Mean Sq`[1]
SSR / df.SSR

#finding the F* test statistic
F.stat <- aov.reg$`F value`[1]

#getting pvalue 
p.val <- aov.reg$`Pr(>F)`[1] 
pf(F.stat, df1 = 1, df2 = n - 2, lower.tail = FALSE)

#finding critical value for f distribution of our data set
qf(p=.05, df1=1, df2=43, lower.tail=FALSE)



XpY=sum(X+Y)
XpY=t(X)%*%Y
XpX=t(X)%*%X
YpY=t(Y)%*%Y

#plotting our 90% confidence band 
alpha=0.1
SEY.hat = MSE * (1/n + (X - Xbar)^2 / (XpX - n * Xbar^2))
W <- sqrt(2 * qf(p = 1 - alpha, df1 = 2, df2 = n - 2))
conf.band.upper <- Y.hat + W * SEY.hat
conf.band.lower <- Y.hat - W * SEY.hat
plot(X,Y,cex=0.5)
lines(X[order(X)], conf.band.upper[order(X)], col='red')
lines(X[order(X)], conf.band.lower[order(X)], col='red')
W <- sqrt(2 * qf(p = 1 - alpha, df1 = 2, df2 = n - 2))

#plotting regression line
xy.lm=lm(Y~X)
coeff=xy.lm$coefficients
abline(coeff, col='blue')

#problem 2
#finding critical value for t distribution 
qt(.05, 5, lower.tail=FALSE)
2.015048




