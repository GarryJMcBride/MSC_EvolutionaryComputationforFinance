library(quantmod) 
library(nnet)
library(caret)

t <- seq(0,20,length=200)                       # time stamps
y <- 1 + 3*cos(4*t+2) +.2*t^2 + rnorm(200)      # the time series we want to predict
dat <- data.frame( y, x1=Lag(y,1), x2=Lag(y,2)) # create a triple with lagged values
names(dat) <- c('y','x1','x2')
head(dat)

#Fit model
model <- train(y ~ x1+x2, dat, method='nnet', linout=TRUE, trace = FALSE)

ps <- predict(model, dat)

#Examine results

plot(t,y,type="l",col = 2)
lines(t[-c(1:2)],ps, col=3)
legend(1.5, 80, c("y", "pred"), cex=1.5, fill=2:3)

inTrain <- createDataPartition(y=iris$Species, p=0.75, list=FALSE)   # We wish 75% for the trainset 

train.set <- iris[inTrain,]
test.set  <- iris[-inTrain,]
nrow(train.set)/nrow(test.set) # should be around 3

model <- train(Species ~ ., train.set, method='nnet', trace = FALSE) # train
# we also add parameter 'preProc = c("center", "scale"))' at train() for centering and scaling the data
prediction <- predict(model, test.set[-5])                           # predict
table(prediction, test.set$Species)                                  # compare

# predict can also return the probability for each class:
prediction <- predict(model, test.set[-5], type="prob")  
head(prediction)
