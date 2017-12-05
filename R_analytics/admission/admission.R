library(ggplot2)
library(data.table)
library(magrittr)

ex2data1 <- fread("ex2data1.txt",col.names=c("Exam1","Exam2","Label"))    
head(ex2data1)

cols % ggplot(aes(x=Exam1,y=Exam2,color=factor(Label)))+geom_point(size = 4, shape = 19,alpha=0.6)+
  scale_colour_manual(values = cols,labels = c("Not admitted", "Admitted"),name="Admission Status")

my_sigmoid=function(z){
  1/(1+exp(-z))
}

x=seq(-10,10, by=0.1)
y=my_sigmoid(x)

plot(x,y,type="l",col="red",xlab="",ylab="",main="Sigmoid function (Logistic curve)")
abline(v=0,lty = 2,col="gray 70")
abline(h=0.5,lty = 2,col="gray70")

y=ex2data1$Label
X=cbind(1,ex2data1$Exam1,ex2data1$Exam2)
head(X)

initial_theta =matrix(rep(0,ncol(X)))

my_cost=function(theta, X, y){
  cost_gradient=list()
  h_theta= my_sigmoid(X%*%theta)
  cost= 1/nrow(X)*sum(-y*log(h_theta)-(1-y)*log(1-h_theta))
  return(cost)
}

round(my_cost(initial_theta, X, y),3)

