#Regression on Pearson's father-son data

#Loading the previously installed the packages using library command
library(UsingR)
data(father.son)#To load the dataset
str(father.son)#To check the structure of the data set

#Assigning variables for the data columns
father.h <- father.son$fheight 
son.h <- father.son$sheight

#Producing the scatter plot of the data
plot(father.h, son.h, xlab="Father's height (in)",
     ylab="Son's height (in)", pch=20);

#Performing linear regression
regremod<- lm(son.h~father.h)

#Adding the regression line
abline(regremod, col= 'yellow', lwd=2)

#Finding the standard deviation and mean
sdev_fatherh<- sd(father.h)
sdev_sonh<-sd(son.h)
mn_fatherh<-mean(father.h)
mn_sonh<-mean(son.h)

#Finding the slope and intercept
b<- (sdev_sonh/sdev_fatherh)

# The SD line passes through the center of regression, (mean(X), mean(Y))=(67.6871, 68.68407)
# So the equation is given as:
# y-y1=b(x-x1)
#(x1,y1)= (mean(X), mean(Y))
# The value of x=0 for the y intercept.
a<- mn_sonh-(mn_fatherh*b)

#Adding the SD line
abline(a=a,b=b,col="blue",lty=4,lwd=3)

#Marking the center of regression
points(mn_fatherh, mn_sonh,col='red',pch=17,cex=2);

#Adding vertical and horizontal lines through the center of regression
abline(v=mn_fatherh, col= 'green', lwd=2)
abline(h=mn_sonh, col= 'green', lwd=2)

#Creating a report of the regression model
summary(regremod)
