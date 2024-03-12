install.packages('neuralnet')
install.packages('quantmod')
install.packages('Metrics')
install.packages('pracma')

library("neuralnet")
library("quantmod")
library('Metrics')
library('pracma')

# mystocks variable calling the stocks we want to use, in this case technology companies, currently active is 17 companies.
myStocks <- c("AMZN")

# this code get the stocks using yahoo as its resource and the dates are for the length from the new york stock exchange
getSymbols(myStocks, src="yahoo", from="2018-01-01", to="2019-01-01")

str(AMZN)  # this will give you the structure
head(AMZN)    #will give you the first few lines of the data#

# Mean, Mode, Median and range of the Amazon data
mean(AMZN)
mode(AMZN)
median(AMZN)
range(AMZN)

# Plotting the Amazon data
plot(AMZN, main = "Amazon stock year")

datats <- getSymbols

# Plotting the data for first look at the time series
plot.ts(data)

# Plot for looking at trend, season and random graph of the time series data
datatscomponents <- decompose(data)
plot(datatscomponents)








# The data heads for each of the factors allowed to explore 

# Open information
OP <- coredata(AMZN$AMZN.Open)
str(OP)
head(OP)
OP <- coredata(AMZN$AMZN.Open)[,1]
OP

# Closeing Information
CL <- coredata(AMZN$AMZN.Close)
str(CL)
head(CL)
CL <- coredata(AMZN$AMZN.Close)[,1]
CL


# Highest value Information
HV <- coredata(AMZN$AMZN.High)
str(HV)
head(HV)
HV <- coredata(AMZN$AMZN.High)[,1]
HV

# Lowest Value Information
LV <- coredata(AMZN$AMZN.Low)
str(LV)
head(LV)
LV <- coredata(AMZN$AMZN.Low)[,1]
LV


# Volume Information
VOL <- coredata(AMZN$AMZN.Volume)
str(VOL)
head(VOL)
VOL <- coredata(AMZN$AMZN.Volume)[,1]
VOL


# Adjusted Price Information
AJC <- coredata(AMZN$AMZN.Adjusted)
str(AJC)
head(AJC)
AJC <- coredata(AMZN$AMZN.Adjusted)[,1]
AJC


# Exponential Moving Average (EMA)
eMA <- coredata(AMZN$AMZN.Adjusted)
str(eMA)
head(eMA)
eMA <- coredata(AMZN$AMZN.Adjusted)[,1]
eMA

# The Relative String Index (RSI) Information
rSI <- coredata(AMZN$AMZN.Adjusted)
str(rSI)
head(rSI)
rSI <- coredata(AMZN$AMZN.Adjusted)[,1]
rSI



# Calculating the Mean, Medium Mode and range of the factors in the data

# Opening
mean(OP)
mode(OP)
median(OP)
range(OP)

# CLosing
mean(CL)
mode(CL)
median(CL)
range(CL)

# Highest Value
mean(HV)
mode(HV)
median(HV)
range(HV)

# Lowest Value
mean(LV)
mode(LV)
median(LV)
range(LV)

# Volume
mean(VOL)
mode(VOL)
median(VOL)
range(VOL)

# Adjusted Price
mean(AJC)
mode(AJC)
median(AJC)
range(AJC)

# Exponential Moving Average (EMA)
mean(eMA)
mode(eMA)
median(eMA)
range(eMA)
# The Relative String Index (RSI) Information

mean(rSI)
mode(rSI)
median(rSI)
range(rSI)



# RUN THESE AFTER THE MATRIX CODE

# plotting for RSI and EMA separetly
plot.ts(Ema, main ="Ema")
plot.ts(Rsi, main ="Rsi")

# Scatter Plots for EMA and RSI together and alone
plot(Rsi, Ema, main ="Rsi & Ema", sub="Scapper Plot")
plot(Rsi, main ="Rsi", sub="Scapper Plot")
plot(Ema, main ="Ema", sub="Scapper Plot")





#
###### CREATING MATRIX, WINDOWS & PREPARING FOR NEURAL NETWORK
#

# Creating the matrix and preparing the inputs to be used
# Scaling the data for more accurate results

Rsi <- scale(RSI(AJC, 10))
rsilagged <- Lag(Rsi, k = seq(1:5))
#Sma <- SMA(AJC, 10)
Ema <- scale(EMA(AJC, 10))
emalagged <- Lag(Ema, k = seq(1:5))

# Scaled data and lagged together in one Variable
datats <- cbind(Rsi, Ema, rsilagged, emalagged)
datats

# Lengths of each factor
length(Rsi)
length(Ema)
length(AJC)

# Removing missing values from the data matrix
datats <- na.omit(datats)

# Viewing rows and colums of new data without missing values
nrow(datats)

ncol(datats)

head(datats)

# Declaring the input names, 12 have been chosen and will experiment with above and below
colnames(datats) <- c("RSI", "EMA", "input1", "input2", "input3", "input4", "input5", "input6", "input7", "input8", "input9", "input10")
colnames








#
###### SPLITTING DATA
#

# Splitting the data with 75% and 25%
index <- round(0.75 * nrow(datats))
index

# declaring the training data to 75%
datatrain <- datats[1:index,]
datatrain[1:20,]

# Declaring the testing data to the remaining 25% of the data
datatest <- datats[(index + 1) : nrow(datats),]
datatest[1:20,]









#
###### NEURAL NETWORK
#

# Using the Neural network and calling the inputs with hidden layers and thresholds. step max is for the amount of errors the NN will allow
nnetwork <- neuralnet(RSI + EMA ~ input1 + input2 + input3 + input4 + input5 + input6 + input7 + input8 + input9  + input10, datatrain, hidden = c(20, 20, 20), stepmax = 1e+06, threshold = 0.1)
plot(nnetwork, main = "Neural Network with Rsi and Ema")

# Calling the neural network and the first row for the error value 
nnetwork$result.matrix
nnetwork$result.matrix[1,]









#
##### Training
#

# Declaring the NN for the training data and for the predictions
trainresult <- as.data.frame(nnetwork$net.result)
trainresult

# Labels that exist in the data
trainlabels <- datatrain[, ncol(datatrain)]

# Combining both names to then be called for predictions
combtrain <- cbind(trainlabels, trainresult)

# Calling predictions beside the actual valies (labels)
colnames(combtrain) <- c("actual results", "neural net results")
combtrain

trainlabels











#
##### Testing
#


# Net results computed together for the network and then the data testing
net.results = compute(nnetwork, datatest)
head(net.results)
new = cbind(net.results$net.result, datatest[,1:2])
head(new)

colnames(new) <- c("predicted_ema", "predicted_rsi", "actual_ema", "actual_rsi")


# Error in the column for all 59
# RSI Columns both Predicted and Actual
rmse(new[,1], new[,3])
# EMA Columns both Predicted and Actual
rmse(new[,2], new[,4])

nrow(new)

# for loop for ema, is the moving average is more than prediction is the next day, then sell because its going to go down, and will make money
# so if the ema is high today, we are predicting it will be low tomorrow, so sell, if its low today, we are predicting it will be high more so buy

list= c()

for (i in seq(2, nrow(new))){
  if (new[i,'predicted_ema'] < (new[i - 1, 'actual_ema'])){
    list = rbind(list, "sell")
  }else{
    list = rbind(list, "buy")
  }
}

head(list)
print(list)

open_data = AMZN[(nrow(AMZN)-nrow(list)+1):nrow(AMZN),"AMZN.Open"]

buy = 0
# unlimited debt
money = 0

list[nrow(list)] = "sell"
print(list)

for (i in seq(1, nrow(list))){
  if (strcmp(list[i], "sell")){
    if (buy != 0){
      buy = buy - 1
      money = money + as.numeric(open_data[i])
    }
  }else{
    if (buy == 0){
      buy = buy + 1
      print(as.numeric(open_data[i]))
      money = money - as.numeric(open_data[i])  
    }
  }
}


print(buy)
print(money)







#
##### Ramdomly generated method for comparison against neural network
#



sample(0:1, 1)
list2 = c()

for (i in seq(1, nrow(list))){
  list2 = rbind(list2, sample(0:1, 1))
}

print(list2)

sample(0:1, 1)
list3 = c()

for (i in seq(1, nrow(list2))){
  if (list2[i] == 1){
    list3 = rbind(list3, "buy")
  }else{
    list3 = rbind(list3, "sell")
  }
}

print(list3)




buy = 0
# unlimited debt so see if after 59 rows or iterationg the model has made a profit. The for loop will always end in sell
money = 0

list3[nrow(list3)] = "sell"
print(list3)

for (i in seq(1, nrow(list3))){
  if (strcmp(list3[i], "sell")){
    if (buy != 0){
      buy = buy - 1
      money = money + as.numeric(open_data[i])
    }
  }else{
    if (buy == 0){
      buy = buy + 1
      print(as.numeric(open_data[i]))
      money = money - as.numeric(open_data[i])  
    }
  }
}


print(buy)
print(money)

