# The following code is installing the packages used in the code
install.packages("quantmod")
install.packages('GA')
#install.packages('parrallell')
install.packages('magicfor')

# Calling the installed packages into the code using library
library(GA)
library(quantmod)
library(magicfor)

# ***TRAINING***

# mystocks variable calling the stocks we want to use, in this case technology companies, currently active is 17 companies.
myStocks <- c("AAPL", "CSCO", "GOOG", "INTC", "MSFT", "ADBE", "AMD", "DELL"
              , "INTU", "MRVL", "SAP", "TSM", "ALTR", "CYBR", "NTNX", "OKTA", "WIX")

# the following tickeres were used before companies were removed based on results from running the weekly returns.
#myStocks <- c("AAPL", "CSCO", "GOOG", "INTC", "MSFT", "ADBE", "AMD", "DELL", "DBX", "EA", "GDDY", "IBM",
#"NVDA", "SNAP", "INTU", "MRVL", "SAP", "TSM", "AL", "ALTR", "ASX", "AVLR", "CYBR", "NTNX", 
#"OKTA", "SINA", "SWCH", "WIX")

# calling number of stocks as a variables and then saying to use the full length of the mystocks variable
numStocks <- length(myStocks)

# this code get the stocks using yahoo as its resource and the dates are for the length from the new york stock exchange
getSymbols(myStocks, src="yahoo", from="2018-01-01", to="2019-01-01")

# making the data frame and merging all the selected stocks together
myRetData <- data.frame(as.xts(merge(weeklyReturn(AAPL), weeklyReturn(CSCO), weeklyReturn(GOOG), 
                                     weeklyReturn(INTC), weeklyReturn(MSFT), weeklyReturn(ADBE), weeklyReturn(AMD), 
                                     weeklyReturn(DELL), weeklyReturn(INTU), 
                                     weeklyReturn(MRVL), weeklyReturn(SAP), weeklyReturn(TSM), 
                                     weeklyReturn(ALTR), weeklyReturn(CYBR), 
                                     weeklyReturn(NTNX), weeklyReturn(OKTA), 
                                     weeklyReturn(WIX))))


#this prints all the data from the reta data
myRetData

#returns for specific stock with .then number
mean(myRetData$weekly.returns.11)

#returns for all stocks
returns <- colMeans(myRetData)
#Calling all the means to run
returns

# putting the ret data into a cov'matrix
covMat <- cov(myRetData)
# calling the function
covMat

# Setting the lower and upper limit for the weights
lower <- c(rep(0, numStocks))
upper <- c(rep(1, numStocks))

# creating a scaling function for when the GA produces results and they need to be scaled to equal 1
scalingfunction <- function(x) {
  return(x/sum(x))
  
}


# code for trying to scale based on random data entered
#testvector <- c(0.58, 0.55, 0.36, 0.20)

#scalingfunction(testvector)

# Code and calculation for producing the return function
returnfunction <- function(x) {
  return(sum(x*returns))
  
}

# Code and calculation for producing the risk function
riskfunction <- function(x) {
  return(sum(t(x*covMat)*x))
  
}

# using the magic_for to store variables automatically in the for loop for the GA.
# Setting the weights and GA to run 11 times with a different weight eachother from 0-1.
magic_for(print,silent=TRUE)
for(weight in seq(from = 0, to = 1, 0.1)){
# Multi-objective function to take our two variables we are considering when making this GA, risk and return with chromosomes
Multiobjfunction <- function(x) {
  chromosome <- scalingfunction(x)
  fitness <- (weight*returnfunction(chromosome)) + ((1 - weight) * (-riskfunction(chromosome)))
  return(fitness)
}

# Setting the GA and calling the package, a population of 700 was used as any higher it wasnt getting better results and was taking longer.
# The GA ends after 50 runs of the same fitness value. 
GeneticAlgorithm <- ga(type = "real-valued", fitness = Multiobjfunction, lower = lower, upper = upper,
                       popSize = 700, 
                       run = 50,
                       elitism = 2,
                       maxiter = 300, 
                       monitor = TRUE, 
                       seed = 1)

scaledsolution <- c(GeneticAlgorithm@solution)
print(scaledsolution)
}

# Using Magic to retrieve the variables inside the GA and calling the soltuions (pre-scaled) along with its sum
solutions <- matrix(magic_result_as_vector(),ncol=17)
solutions
sum(solutions[1,])

# Scaling the solutions to equal to 1, using the scaling function we created earlier. Also can call on a certain soltions in the last line.
scaled <- apply(solutions,1,scalingfunction)
scaled <- t(scaled)
scaled
sum(scaled[7,])

# Creating the return value with the scaled data, and having a sum line to make sure they equal to 1
solutionReturns <- apply(scaled,1,returnfunction)
solutionReturns
sum(solutionReturns)

# Creating the risk value with the scaled data, and having a sum line
solutionRisks <- apply(scaled,1,riskfunction)
solutionRisks
sum(solutionRisks)

#Plotting the graph for both risk and return after being scaled
plot(solutionReturns,solutionRisks)


#help(package = 'GA')


# ***RANDOM SEARCH***

# Random search finding chromosomes in the population of 5000 and scaling them to 1
randomsearchlist <- list()
for(i in (1:5000)) {
  randomsearchlist[[i]] <- runif(numStocks, 0, 1)
}

randomsearchlist

#The following code calls the returns of the scaled solutions, the risk and return.
randomscaled <- lapply(randomsearchlist,scalingfunction)
randomscaled

randomsolutionReturns <- sapply(randomscaled,returnfunction)
randomsolutionReturns

randomsolutionRisks <- sapply(randomscaled,riskfunction)
randomsolutionRisks

plot(randomsolutionReturns, randomsolutionRisks)


# ***TESTING***


# this code get the stocks using yahoo as its resource and the dates are for the length from the new york stock exchange
getSymbols(myStocks, src="yahoo", from="2019-01-01", to="2019-02-02")

# making the data frame and merging all the selected stocks together
testmyRetData <- data.frame(as.xts(merge(weeklyReturn(AAPL), weeklyReturn(CSCO), weeklyReturn(GOOG), 
                                         weeklyReturn(INTC), weeklyReturn(MSFT), weeklyReturn(ADBE), weeklyReturn(AMD), 
                                         weeklyReturn(DELL), weeklyReturn(INTU), 
                                         weeklyReturn(MRVL), weeklyReturn(SAP), weeklyReturn(TSM), 
                                         weeklyReturn(ALTR), weeklyReturn(CYBR), 
                                         weeklyReturn(NTNX), weeklyReturn(OKTA), 
                                         weeklyReturn(WIX))))
#returns for all stocks
testreturns <- colMeans(testmyRetData)
#Calling all the means to run
testreturns

# putting the ret data into a matrix
testcovMat <- cov(testmyRetData)
# calling the function
testcovMat

#test return function calucaltion created 
testreturnfunction <- function(x) {
  return(sum(x*testreturns))
  
}

#test risk function calucaltion created 
testriskfunction <- function(x) {
  return(sum(t(x*testcovMat)*x))
  
}


scaled

# Calling the risk and return using the test functions with sums
testsolutionReturns <- apply(scaled,1,testreturnfunction)
testsolutionReturns
sum(testsolutionReturns)

testsolutionRisks <- apply(scaled,1,testriskfunction)
testsolutionRisks
sum(testsolutionRisks)

plot(testsolutionReturns, testsolutionRisks)
