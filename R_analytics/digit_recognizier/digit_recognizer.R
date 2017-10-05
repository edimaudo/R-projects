library(readr)
library(caret)
library(ggplot2)

#load data
train<- read.csv("train.csv")
test <- read.csv("test.csv")

#check training data
dim(train)

str(train)

summary(train)


ggplot(train,aes(x=as.factor(label),fill=label))+
  geom_bar(stat="count",color="white")+
  scale_fill_gradient(low="lightblue",high="pink",guide=FALSE)+
  labs(title="Digits in Train Data",x="Digits")

#take a look at data sample
sample <- sample(1:nrow(train),50)
var <- t(train[sample,-1])
var_matrix <- lapply(1:50,function(x) matrix(var[,x],ncol=28))
opar <- par(no.readonly = T)
par(mfrow=c(5,10),mar=c(.1,.1,.1,.1))

for(i in 1:50) {
  for(j in 1:28) {
    var_matrix[[i]][j,] <- rev(var_matrix[[i]][j,])
  }
  image(var_matrix[[i]],col=grey.colors(225),axes=F)
}

#check for predictors which have few unique values
nzr <- nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7)
sum(nzr$zeroVar)

cutvar <- rownames(nzr[nzr$nzv==TRUE,])
var <- setdiff(names(train),cutvar)
train <- train[,var]

#apply PCA to reduce # of predictors
label <- as.factor(train[[1]])
train$label <- NULL
train <- train/255
covtrain <- cov(train)

train_pc <- prcomp(covtrain)
varex <- train_pc$sdev^2/sum(train_pc$sdev^2)
varcum <- cumsum(varex)
result <- data.frame(num=1:length(train_pc$sdev),
                     ex=varex,
                     cum=varcum)

plot(result$num,result$cum,type="b",xlim=c(0,100),
     main="Variance Explained by Top XX Components",
     xlab="Number of Components",ylab="Variance Explained")
abline(v=25,lty=2)

train_score <- as.matrix(train) %*% train_pc$rotation[,1:25]
train <- cbind(label,as.data.frame(train_score))

colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(train$PC1,train$PC2,type="n",main="First Two Principal Components")
text(train$PC1,train$PC2,label=train$label,col=colors[train$label])

