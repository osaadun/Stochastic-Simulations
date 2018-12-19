install.packages("ggplot2")
library(ggplot2)

#The goal is to build an algorithm that approximate Differential equations
x0<- readline(prompt="Enter the initian point")
x0<-as.numeric(x0)
MU<-readline(prompt="Enter the mu parameter")
MU<-as.numeric(MU)
DELTA<-readline(prompt="Enter the discretized step parameter")
DELTA<-as.numeric(DELTA)
N<-readline(prompt="Enter the number of steps you wish to consider")
N<-as.numeric(N)

#Let us assign X to be a random constant term

x<-x0
vect<-c()
norm_vect<-rnorm(N)

for(i in 1:N)
{
  x<- x+x*(MU-x^2)*DELTA+norm_vect[i]*sqrt(DELTA)
  
  vect<-c(vect,x)
  
}

M <-N/1000
Xi<-c()
for(i in 1:M)
{
  mn<-mean(vect[1:1000*i])
  Xi<-c(Xi,mn)
}
print(mn)
plot(Xi,col='blue',main='Ornstein-Uhlenbeck Equation')

labels<-c('Delta=.01','mu=5','sigma=1')
legend("topright",inset=.01,title="Legend",labels)
#hist(Xi)
hist(vect, breaks=1000,freq=FALSE )

vect1<-vect[30000:N]

plot(density(vect), col='red')
lines(density(vect1))






