hwproblem3 <- read.table("~/Documents/Classroom/STA 108/CH01PR27.txt", quote="\"", comment.char="")
View(hwproblem3)
#import data set

X<-hwproblem3[,2]
Y<-hwproblem3[,1]

head(hwproblem3)
n = nrow(hwproblem3)
n


Xbar = sum(X)/n
Ybar = sum(Y)/n

xy.lm = lm(Y ~ X)
summary(xy.lm)

aov.reg <- anova(xy.lm)
aov.reg

SSE <- aov.reg$`Sum Sq`[2]
sum((xy.lm$residuals)^2)
SSR <- aov.reg$`Sum Sq`[1]
SSTO = sum((Y - Ybar)^2)
SSTO - SSE


df.SSE <- aov.reg$Df[2]
df.SSR <- aov.reg$Df[1]

MSE <- aov.reg$`Mean Sq`[2]
SSE / df.SSE
MSR <- aov.reg$`Mean Sq`[1]
SSR / df.SSR

F.stat <- aov.reg$`F value`[1]
MSR / MSE

p.val <- aov.reg$`Pr(>F)`[1] 
pf(F.stat, df1 = 1, df2 = n - 2, lower.tail = FALSE)


par(mfrow = c(1, 2))
plot(X, xy.lm$residuals, 
     ylim = c(-25, 25),
     xlab = 'X', ylab = 'residuals')
plot(X, (xy.lm$fitted.values - Ybar), 
     ylim = c(-25, 25),
     xlab = 'X', ylab = 'fitted values - Ybar')


# R^2
SSTO <- SSR + SSE
R.sq <- SSR / SSTO

# correlation coefficient 
r <- cor(X, Y)


#finding f distribution critical value 
qf(p=.05, df1=1, df2=58, lower.tail=FALSE)


#plotting our points and regression line
plot(X,Y,cex=0.5)
xy.lm=lm(Y~X)
coeff=xy.lm$coefficients
abline(coeff, col='blue')