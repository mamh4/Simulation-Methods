source("rscript.r")

#install.packages("MASS")
library(MASS)

sample_mean <- mean(data$CLM_FREQ) #calculating the sample mean
sample_mean
sample_variance <- var(data$CLM_FREQ) #calculating the sample variance
sample_variance

#################################QUESTION 1###########################


#after observing the data$FREQ_claim we understood that the number of claims ranges from 0 to 7
#observed_data is a list with key values referring to the number of claims
observed_data <- list("0claim"=0, "1claim" = 0, "2claim" = 0, "3claim" = 0, "4claim" = 0,
                      "5claim" = 0, "6claim" = 0, "7claim" = 0)

# Iterate through the elements of data$CLM_FREQ
for (i in data$CLM_FREQ) {
  if (i ==0) {
    observed_data[["0claim"]] <- observed_data[["0claim"]] + 1
  }else if (i==1){
    observed_data[["1claim"]] <- observed_data[["1claim"]] + 1
  } else if (i == 2) {
    observed_data[["2claim"]] <- observed_data[["2claim"]] + 1
  } else if (i == 3) {
    observed_data[["3claim"]] <- observed_data[["3claim"]] + 1
  } else if (i == 4) {
    observed_data[["4claim"]] <- observed_data[["4claim"]] + 1
  } else if (i == 5) {
    observed_data[["5claim"]] <- observed_data[["5claim"]] + 1
  } else if (i == 6) {
    observed_data[["6claim"]] <- observed_data[["6claim"]] + 1
  } else if (i == 7) {
    observed_data[["7claim"]] <- observed_data[["7claim"]] + 1
  }
}



for (i in 1:length(observed_data)) {
  observed_data[[i]] <- observed_data[[i]] / length(data$CLM_FREQ)
}


observed_data



##Negative Binomial#############################

pdf_negative_bin <- function(x, r, p) {
  choose(x + r - 1, x) * (1 - p)^x * p^r
} #pdf of Negative Binomial

##According to the method of moments we assume that the sample mean equals E(X)
#and the sample variance equals VAR(X)

#E(X)=(1-p)*r/p
#VAR(X)=r*(1-p)/p^2


# Define a class called "MyClass"
setClass("MLE_neg_bin",
         representation(sample_mean = "numeric", sample_variance = "numeric", r = "numeric", p = "numeric"))

# Create a constructor function for the class
MLE_neg_bin <- function(sample_mean, sample_variance) {
  obj <- new("MLE_neg_bin")
  obj@sample_mean <- sample_mean
  obj@sample_variance <- sample_variance
  obj@r <- NA_real_
  obj@p <- NA_real_
  return(obj)
}

# Define a method for the class that calculates r and p values
calculate <- function(obj) {
  
  obj@p <- obj@sample_mean / obj@sample_variance
  obj@r <- obj@sample_mean ^2 / (obj@sample_variance - obj@sample_mean)
  
  return(obj)
}


my_obj <- MLE_neg_bin(sample_mean, sample_variance)
my_obj <- calculate(my_obj)
print(paste("the MLE estimator of r, in Negative Binomial is",my_obj@r))
print(paste("the MLE estimator of p, in Negative Binomial is", my_obj@p))

my_obj@r
my_obj@p


empirical_data <- list()

for (i in 1:length(observed_data)) {
  empirical_data[i] <- pdf_negative_bin(i, my_obj@r, my_obj@p)
}


empirical_data

chisq.test(unlist(empirical_data),unlist(observed_data))

chisq.test(unlist(observed_data),unlist(observed_data))

#p-value = 0.2289
#since the p-value (0.2289) is greater than the typical significance level of 0.05
#we cannot reject the null hypothesis.
#there is no significant evidence to suggest that the observed data 
#significantly deviates from the expected values based on the empirical data

#################Binomial############################

pdf_binomial <- function(x, n, p) {
  choose(n, x) * (1 - p)^(n-x) * p^x
}


###############################NEGATIVE BINOMIAL################

###########calculates the number of of trials required 
#to achieve r successes in a negative binomial distribution







##############################ACCEPTANCE/REJECTION METHOD########################

X = runif(4500, 0, 1)
U = runif(4500, 0, 1)

pdf_negative_bin <- function(x, r, p) {
  choose(x + r - 1, x) * (1 - p)^x * p^r
}

count = 1
accept = c()

while (count <= 4500 & length(accept) < 1000) {
  test_u = U[count]
  test_x = pdf_negative_bin(X[count], r, p) / (3.125 * dunif(X[count], 0, 1))
  
  if (test_u <= test_x) {
    accept = c(accept, X[count])
  }
  
  count = count + 1
}

hist(accept)







############################################QUESTION 4################################


#Risk premiums calculated using the data

total_loss <- 0
# Assuming 'matrix' is your two-dimensional matrix
for (i in 1:nrow(data)) {
  for (j in 2:8) { 
    # Iterating from the 2nd to the 8th column, the columns including claims height 
    #and claims severity
    total_loss <- total_loss + data[i, j]
  }
}
riskpremium <- total_loss/ nrow(data) #the estimation is basically the total amount paid
#for claims divided by the number of claims
riskpremium #the premium using our data should be 766.067



#Risk premium calculated based on our estimated frequency and severity models

# Function to generate random samples from a negative binomial distribution using inversion method

simulate_negative_binomial <- function(r, p, size){
  simulated_negative_binomial <- rep(0, size)
  for (i in 1:size){
    #cat("item", i)
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    #print(lower_bound) 
    #print(upper_bound) 
    random_number <- runif(1,0,1)
    #print(random_number)
    while (! ((upper_bound > random_number ) & (lower_bound <= random_number) )) {
      #print(" ")
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
      #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    }
    simulated_negative_binomial[i] <- counter
  }
  return (simulated_negative_binomial)
}

simulated_negative_binomial(my_obj@r,my_obj@p,1000)

##########estimate the parameters of LN##############

lossesVector <- vector()
for  (i in 3:9 ){
  for (j in 1:length(data[,i])){
    if ( data[j,i] > 0 ){
      lossesVector <- append(lossesVector, data[j,i])
      print (data[j,i])
    }
  }
}

sample_mean_los<- mean(lossesVector)
sample_var_los <- var(lossesVector)

###estimating the parameters for LN using the method of moments
logNormalSigmaSquareEstimator <- function(random_vector){
  return(log(1 + sample_var_los(random_vector)/(sample_mean_los(random_vector)^2)))
}

logNormalMuEstimator <- function(random_vector){
  return(log(sample_mean_los(random_vector)) - (logNormalSigmaSquareEstimator(random_vector)/2) )
}

##############assigning the results to variables sigmaSquareLN,muLn
sigmaSquareLN <- logNormalSigmaSquareEstimator(lossesVector)
muLN <- logNormalMuEstimator(lossesVector)
