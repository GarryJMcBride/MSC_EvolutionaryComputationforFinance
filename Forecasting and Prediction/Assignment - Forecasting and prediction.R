install.packages('neuralnet')
install.packages('caret')
install.packages('quantmod')

library("neuralnet")
library("caret")
library("quantmod")


#
# IMPORTING DATA AND PLOTS
#

# Important the data
data <- scan("http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts/m-ibmln99.dat")

# General Data analysis
datats <- ts(data)
datats

# Calling the help command for the Neural Network
help("neuralnet")

# Calculating the Mean, Medium Mode and range of the data
mean(data)
mode(data)
median(data)
range(data)

# Data summary with a frequency of 12 since the data is taken yearly
datats <- ts(data, frequency = 12)
datats

# Calculating various moving averages 
datats <- SMA(datats, 12)
datats

# Removing NA values in the data set
datats <- na.omit(datats)
datats

# Plotting the data for first look at the time series
plot.ts(datats)

# Plot for looking at trend, season and random graph of the time series data
datatscomponents <- decompose(datats)
plot(datatscomponents)


#
# CREATING MATRIX, WINDOWS & PREPARING FOR NEURAL NETWORK
#

# Creating the matrix and preparing the inputs to be used

datamatrix <- cbind(datats, datats1 = Lag(datats, k = 1), datats2 = Lag(datats, k = 2), datats3 = Lag(datats, k = 3),
                    datats4 = Lag(datats, k = 4), datats5 = Lag(datats, k = 5), datats6 = Lag(datats, k = 6), datats7 = Lag(datats, k = 7),
                     datats8 = Lag(datats, k = 8), datats9 = Lag(datats, k = 9))

# Looking at the first 20 entries in the data
datamatrix[1:20,]

# Removing missing values from the data matrix
datamatrix <- na.omit(datamatrix)

# Declaring the input names, 9 have been chosen and will experiment with above and below
colnames(datamatrix) <- c("input1", "input2", "input3", "input4", "input5", "input6", "input7", "input8", "input9", "output")


#
# SPLITTING DATA
#

# Splitting the data with 75% and 25%
index <- round(0.75 * nrow(datamatrix))
index

# declaring the training data to 75%
datatrain <- datamatrix[1:index,]
datatrain[1:20,]

# Declaring the testing data to the remaining 25% of the data
datatest <- datamatrix[(index + 1) : nrow(datamatrix),]
datatest[1:20,]


#
# NEURAL NETWORK
#

# Using the Neural network and calling the inputs with hidden layers and thresholds. step max is for the amount of errors the NN will allow
nnetwork <- neuralnet(output ~ input1 + input2 + input3 + input4 + input5 + input6 + input7 + input8 + input9, datatrain, hidden = c(20, 20, 20), stepmax = 1e+06, threshold = 0.1)
plot(nnetwork)

# Calling the neural network and the first row for the error value 
nnetwork$result.matrix
nnetwork$result.matrix[1,]

# Training

# Declaring the NN for the training data and for the predictions
trainresult <- as.data.frame(nnetwork$net.result)

# Labels that exist in the data
trainlabels <- datatrain[, ncol(datatrain)]

# Combining both names to then be called for predictions
combtrain <- cbind(trainlabels, trainresult)

# Calling predictions beside the actual valies (labels)
colnames(combtrain) <- c("actual results", "neural net results")
combtrain

# Testing

# Decarling the test data labels
testlabels <- datatest[,ncol(datatest)]

datatest <- datatest[,1:(ncol(datatest) - 1)]
datatest

# Computing the labels and results from the NN for the testing predictions
testresults <- compute(nnetwork, datatest)
testresults$net.result


testresults <- as.data.frame(testresults$net.result)

# Combing the names of the labels and the test results for predictions on testing
combtest <- cbind(testlabels, testresults)

# Printing the labels and predictions based on the testing data from the NN
colnames(combtest) <- c("actual results", "neural net results")
combtest

# Running the root means sqaure error for the predictions from the training set
rmse <- function(labels, pred){
  return(mean(sqrt((labels-pred)^2)))
}
rmse(testlabels, testresults)


#
# DIFFERENT METHODS
#

# Using the hot-winters method to test the data against another way of prediciton.
hw <- HoltWinters(datatrain, beta=FALSE, gamma = FALSE)
plot(hw)

forecast <- predict(hw, n.ahead = 12, level = 0.95)
plot(hw, forecast)










