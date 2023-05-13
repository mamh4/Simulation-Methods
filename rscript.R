############################################################################################################################
####################################################### Read libraries #####################################################
############################################################################################################################
library(readr) # Check encoding
library(magrittr) # Pipe operator
library(dplyr) # data wrangling
library(purrr) # efficient one liner pattern-matching
library(ggplot2) # additional graphics



############################################################################################################################
#################################################### Read and store data ###################################################
############################################################################################################################

data_orig <-read.csv(file = "DATA_SET_8.csv",sep = ";",encoding = "ASCII") %>%
  as.data.frame() # specify encodeing to use regex

#Always keep original data as is.
data <- data_orig
rm(data_orig) #memory management
############################################################################################################################
#################################################### Function Definitions ##################################################
############################################################################################################################
# Check whether number of claims matches the number of row entries.

# In the end store data here
############################################## Prototype ##################################################################
models <- list( claim_freq_models = list (
                                            poisson = list(
                                                            pdf = function(lambda,x){
                                                              return(lambda^x * exp(-lambda) / factorial((x)))}
                                                            random_poisson = "to be defined",
                                                            hist,
                                                            lines),
                                            
                                            negative_binomial = list(
                                                            pdf = function(r, p, k){return(choose((k+r-1),k) * p^r * (1-p)^k)}
                                            )),
                claim_severity_models = list (
                                            exponential = list(
                                                          pdf = function(lambda,x){
                                                            return(lambda *exp(-x*lambda))}),
                                            gamma = list(
                                                          pdf = function(alpha, lamba, x ){
                                                            #no idea if the following works 
                                                            if (x < 0) 
                                                              {return(0.0) } 
                                                            else 
                                                              #needs gamma function 
                                                              {return(lambda * exp(- lambda * x )*(lambda*x)^(alpha -1 )/1 )}
                                                          }
                                            )
                                            )

##########################################################################################################################


pdf_poisson <- function(lambda,x){
  return(lambda^x * exp(-lambda) / factorial((x)))
}

pdf_exponential <- function(lambda,x){
  return(lambda *exp(-x*lambda))
}

#Our Own RNG? function?

# TODO 
random_poisson <- function(normal_vector){
  
}






############################################################################################################################
################################################# Data Quality Check #######################################################
############################################################################################################################
#Read data types
for(col in colnames(data)){
  print(typeof(data[,col]))
}
#[1] "integer"
#[2] "integer"
#[3] "character"  => should be numeric
#[4] "character"  => should be numeric
#[5] "character"  => should be numeric
#[6] "character"  => should be numeric
#[7] "character"  => should be numeric
#[8] "character"  => should be numeric
#[9] "character"  => should be numeric
#[10] "character" => should be numeric
#[11] "character" => should be numeric
#[12] "double"
#[13] "character"
#[14] "character"
#[15] "double"

##Careful conversion from string to numeric
#Count data amount in the columns to be converted
missing_data_count <- c() # To be used for validation after conversion
for(i in 3:9){
  counter <- 0
  for(j in 1:nrow(data)){
    if(is.na(data[j,i])){
      counter= counter + 1
    }
    missing_data_count[i-2] <- counter
  }
}
#Result: No missing data

#Use regex to remove potential white spaces.
for(i in 3:9){
    data[,i] <- gsub("\\s+", "", data[,i])
}

#Count data amount in the columns to be converted
zero_data_count <- c() # To be used for validation after conversion
for(i in 3:9){
  counter = 0
  for(j in 1:nrow(data)){
    if(data[j,i]=="-"){
      data[j,i]<-"0" #to make-sure conversion went correctly without NA coercion
      counter= counter + 1
    }
  }
  zero_data_count[i-2] <- counter
}

#Convert to numeric
for(i in 3:9){
    data[i] <- as.double(data[[i]])
}


zero_data_count_after_conversion <- c() # To be used for validation after conversion
for(i in 3:9){
  counter_2 = 0
  for(j in 1:nrow(data)){
    if(data[j,i]==0){
      counter_2= counter_2 + 1
    }
    zero_data_count_after_conversion[i-2] <- counter_2
  }
  counter_2 <- 0
}
# zero data count matches
# check correct conversion of -ve values
data[3][data[3]<0] #still exist
#check greater than 3 digit values
data[4][data[4]>999]


# Analyse negative values
negative_value_indices <- rep(FALSE,nrow(data))
for(i in 3:9){
  for(j in 1:nrow(data)){
    if(data[j,i]<0){
      negative_value_indices[j] <- TRUE
    }
  }
}
dplyr::filter(data,negative_value_indices)
#Result: We decided on excluding two policies with -ve CLM_AMT_1 values
data <- dplyr::filter(data,!negative_value_indices)



############################################################################################################################
################################################### Data Exploration #######################################################
############################################################################################################################


# Get summary of the data
summary(data)

features_cat <- c(  'CAR_USE',
                    'CAR_TYPE',
                    'GENDER',
                    'AREA')

# convert to factors for plotting
for (v in features_cat) {
  data[,v] <- as.factor(data[,v])
}


features_int <- 'Age'


claims_amounts <- c('CLM_AMT_1',
                    'CLM_AMT_2',
                    'CLM_AMT_3',
                    'CLM_AMT_4',
                    'CLM_AMT_5',
                    'CLM_AMT_6',
                    'CLM_AMT_7')



for (f in features_cat) {
  plot(data[,f], main=f, xlab='', las=2,col=data$CLM_FREQ)
  grid()
}
# Results: (%-wise)
# Rural has higher impact on the number of claims
# Being Female has higher impact on the number of claims
# Panel truck has higher impact on the number of claims
# Commercial car use has higher impact on the number of claims


for (f in claims_amounts) {
  plot(data[,f], main=f, xlab='' ,las=2,col=data$CLM_FREQ)
  grid()
}
#CLM_AMT_6 and CLM_AMT_7 rare occurrence


plot(data$CLM_FREQ, data$AGE, main='Age', xlab='',las = 2)
grid()


############################################################################################################################
#################################################### Q1 Check Poisson ######################################################
############################################################################################################################

#We check the expected number of claims per policy
#N~Poi(lambda_hat)


#Maximum likelihood Estimator for poisson case to estimate lambda
lambda_hat <- 1/length(data$CLM_FREQ) * sum(data$CLM_FREQ)


claims <- data$CLM_FREQ
hist(claims, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 1000)
y <- pdf_poisson(lambda_hat,x) #Theoretical Distribution
lines(x,y)


#Monte Carlo Estimator with the same parameter lambda


#Mixed Poisson Approach N follows  LAMBDA which it self follows Poi(lambda_hat)?

############################################################################################################################
############################################# Q1 Check Negative binomial ###################################################
############################################################################################################################













############################################################################################################################
############################################# Q2 Check Exponential #########################################################
############################################################################################################################

claim_size_vector <- apply(data[,3:9],1 ,sum)

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = 1000)

#Maximum Likelihood Estimator
lambda_hat <- 1 / (1/nrow(data) * sum(claim_size_vector))
y <- pdf_exponential(lambda_hat,x) #Theoretical Distribution
lines(x,y)



