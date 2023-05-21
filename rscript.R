##################################################### Table of Content #####################################################
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#
#Lines ABCD to AEDF#

############################################################################################################################
####################################################### Read libraries #####################################################
############################################################################################################################
library(readr) # check encoding
library(magrittr) # pipe operator
library(dplyr) # data wrangling
library(purrr) # efficient one liner pattern-matching
library(ggplot2) # additional graphics
library(gridExtra) #for combining ggplots
library(TeachingDemos) # for overlaying plots
library(statmod)

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

simulate_poisson <- function(lambda, size){
  simulated_poisson <- rep(0, size)
  for (i in 1:size){
    sum_of_exponentials <- 0.0 
    counter <- 0 
    while (! (sum_of_exponentials > lambda )) {
      initial_random_number <- runif(1,0,1) 
      #length 1 lower bound 0 upper bound 1 
      simulated_exponential <- - log(1 - initial_random_number ) 
      #given by inversion method for parameter = 1 
      sum_of_exponentials <- sum_of_exponentials + simulated_exponential
      counter <- counter + 1 
    }
    simulated_poisson[i] <- counter -1
  }
  return (simulated_poisson)
}

simulate_exponential <- function(lambda, size){
  simulated_exponential <- rep(0, size)
  for (i in 1:size){
    initial_random_number <- runif(1,0,1)
    single_simulated_exponential <- - log(1 - initial_random_number )/lambda
    #given by inversion method
    simulated_exponential[i] <- single_simulated_exponential
  }
  return (simulated_exponential)
}

simulate_erlang <- function(size,shape,scale_lambda){
  simulation_vector <- c()
  
  for(j in 1:size){
    simulated_gamma <- 0
      for(i in 1:shape){
        simulated_gamma <- simulated_gamma + simulate_exponential(scale_lambda,1)
      }
    simulation_vector[j] <- simulated_gamma
  }
  return(simulation_vector)
}

simulate_nb <- function(size, r, p) {
  random_numbers <- runif(size,0,1)
  result <- c()
  for(i in 1:size){
  counter <- 0 
  lower_bound <- 0 
  upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
  while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
    counter <- counter + 1 
    lower_bound <- upper_bound 
    upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    #print(" ")
  }
  result[i] <- counter
  }
  return(result)
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



#General Analysis of Categorical Variables
temp <- data %>%
  dplyr::mutate(agg_clm = CLM_AMT_1 + CLM_AMT_2 + CLM_AMT_3 +CLM_AMT_4+CLM_AMT_5+CLM_AMT_6+CLM_AMT_7,
                is_claim           = ifelse(CLM_AMT_1>1,1,0),
                car_use_char       = ifelse(CAR_USE=="Commercial","C","P"),
                CAR_USE            = ifelse(CAR_USE=="Commercial","Comm","Private"))


plot1 <-ggplot(temp, aes(x = CAR_TYPE, y = CLM_FREQ, fill = GENDER))+ #CLM_FREQ is only used as a gap filler
  geom_col(position = "fill") +
  xlab("")+
  ylab("")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


plot2 <- ggplot(temp, aes(x = CAR_TYPE, y = CLM_FREQ, fill = CAR_USE))+
  geom_col(position = "fill")+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


plot3 <- ggplot(temp, aes(x = CAR_TYPE, y = CLM_FREQ, fill = AREA))+
  geom_col(position = "fill")+
  xlab("")+
  ylab("")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()


grid.arrange(plot1, plot2, plot3, nrow = 3)


###### Claim Frequency Analysis
for (f in features_cat) {
  plot(data[,f], main=f, xlab='', las=2,col=data$CLM_FREQ)
  grid()
}
for (f in claims_amounts) {
  plot(data[,f], main=f, xlab='' ,las=2,col=data$CLM_FREQ)
  grid()
}

par(mfrow = c(1, 2),cex.main = 0.8, main = "Age")
plot(data$CLM_FREQ, data$AGE, main='',las = 2)
grid()



ggplot(temp, aes(x = CAR_TYPE, y = CLM_FREQ)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitter(width = 0.2, height = 0.08),
    size = 1
  ) +
  scale_y_continuous(breaks = seq(0, 7)) +
  geom_text(
    data = temp,
    aes(x = CAR_TYPE, y = 7, label = paste("Count:", table(CAR_TYPE)[as.character(CAR_TYPE)])),
    vjust = -1,
    size = 3,
    color = "black"
  ) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(title = "Claim Frequency by Car Type") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 40, 20)  # Adjust the top, right, bottom, and left margins
  )




###### Claim Severity Analysis
#Need to pivot the data by CLM_AMT
temp2 <-  tidyr::gather(data,key = "CLM_Number" , value = "CLM_AMT",3:9)
#Need to replace 0 with NA otherwise it ruins the data
temp2$CLM_AMT[temp2$CLM_AMT==0] <- NA
#Include additional variables for analysis
temp2 <- temp2 %>%
  dplyr::mutate(is_claim           = ifelse(CLM_AMT>1,1,0),
                car_use_char       = ifelse(CAR_USE=="Commercial","C","P")) %>%
  dplyr::filter(!is.na(CLM_AMT))



for (f in features_cat) {
  plot(temp2[,f], main=f, xlab='', las=2,col=temp2$CLM_AMT)
  grid()
}


plot(temp2$CLM_AMT, temp2$AGE, main='Age',las = 1)
grid()



ggplot(temp2, aes(x = CAR_TYPE, y = CLM_AMT)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitter(width = 0.2, height = 0.08),
    size = 1
  ) +
  #scale_y_continuous(breaks = seq(0, 2000)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  labs(title = "Claim Severity by Car Type") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 20, 40, 20)  # Adjust the top, right, bottom, and left margins
  )


#Premium Analysis notes:
hist(data$PREMIUM~data$CAR_TYPE)
boxplot(data$PREMIUM~data$CLM_FREQ)

plot(data$CLM_FREQ,data$PREMIUM, main = "Premium across different # claim occurences")
abline(lm(data$PREMIUM~data$CLM_FREQ),col = "blue")
legend("topright", legend = c("Linear model"),
       col = c("blue"), lty = 1)


############################################### Big Picture! Warning!! BAD plots ############################################ 

ggplot(temp, aes(x = CAR_TYPE, y = PREMIUM)) +
  geom_boxplot(outlier.shape = NA) +
  scale_shape_manual(values = c(1, 4)) +
  geom_jitter(aes(col = GENDER, shape = AREA),
              position = position_jitter(width = 0.2, height = 0.2),
              size = 3,
              check_overlap = T) +
  geom_text(aes(label = car_use_char),
            position = position_jitter(width = 0.2, height = 0.2),
            vjust = 1, size = 3,
            check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(0, 7)) +
  theme_minimal()


ggplot(temp2, aes(x = CAR_TYPE, y = PREMIUM)) +
  geom_boxplot(outlier.shape = NA) +
  scale_shape_manual(values = c(1, 4)) +
  geom_jitter(aes(col = GENDER, shape = AREA),
              position = position_jitter(width = 0.2, height = 0.2),
              size = 3,
              check_overlap = T) +
  geom_text(aes(label = car_use_char),
            position = position_jitter(width = 0.2, height = 0.2),
            vjust = 1, size = 3,
            check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(0, 7)) +
  theme_minimal()


##############################################################################################################################


#************************************************* Data Exploration Results *************************************************#

#Rural has higher impact on the number of claims although data is pretty low compared to Urban
#Being Female has higher impact on the number of claims However Many SUVs, 
#so it could be being an SUV + could be 3rd liability. Also Female are more in the data set
#Panel truck and pick up have higher impact on the number of claims but also not many
#Commercial car use has higher impact on the number of claims but correlated with Panel Truck and 
#pick so one cause the other
#CLM_AMT_6 and CLM_AMT_7 rare occurrence
#Age between 30 and 60 corresponds to more claims
#Both claim freq and severity increase for ages between 30 and 60
#Although Panel truck has many claims they are relatively small
#Although rural has high impact on freq severity is relatively low, but then again data is low on rural policies
#Female claims tend to be small in size although higher freq. but again do not forget confounding with sport cars and SUVs
#Note we can condition on variables to try to disintangle confounding but most often we have data only on male/female.
#Sports car are driven by women of 45 median age. In fact the oldest woman is driving a sports car! 72 years
#of Age actually 2 women. And the one who had more claims pays less premium.
#Oldest person in the data set  80 male comes from urban area and never had claims, nonetheless he pays 
#more than average premium.
#Premium is uniformly distributed between 500 and 1000.


rm(temp)#memory management
rm(temp2)#memory management
############################################################################################################################
########################################################### Q1 #############################################################
############################################################################################################################


#################################################### Q1 Check Poisson ######################################################
#Here we assume that the number of claims follows a poisson distribution. We apply Method of Moment to obtain
#an estimator for the poisson parameter lambda.
#E(X) = Lambda => E(x)_hat = 1/n * sum(X)


#Method of Moment for poisson case to estimate lambda
lambda_hat_poisson <- 1/length(data$CLM_FREQ) * sum(data$CLM_FREQ)
par(mfrow = c(1, 1))
#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter lambda hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 11)
y <- dpois(x,lambda=lambda_hat_poisson) #Theoretical Distribution
lines(x,y)


#Result: Graphically poisson seems to be a good model, to quantify how good of a fit it is we test it against a theoretical pdf.
# To make the pdfs comparable we will discetise the theoretical pdf to make them "comparable".
empirical_pdf <- rep(0,9)
for(i in 1:8){
  empirical_pdf[i] <- table(data$CLM_FREQ)[i] / sum(data$CLM_FREQ)
}
theoretical_pdf <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- dpois(i-1,lambda =lambda_hat_poisson)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])

#We now have two comparable vectors. We test them using Chi-squared test.
chisq.test(empirical_pdf,theoretical_pdf)
# Warning message because the frequency in some bins is less than 5 which considered to be minimum


#FF plots
plot.ecdf(data$CLM_FREQ, main = "FF Poisson", xlab = "Loss Frequency", ylab = "CDF", col.points = "Gray")
x = seq(0,10, 1)
y = ppois(x, lambda = lambda_hat_poisson ) #gamma as an example 
lines(x,y, col = "blue")


############################################# Q1 Check Negative binomial ###################################################

#MoM Estimator
p_hat <- 1 / var(data$CLM_FREQ) * mean(data$CLM_FREQ)
r_hat <- mean(data$CLM_FREQ)*p_hat /(1-p_hat)

#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter p_hat and r_hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 11)
y_2 <- dnbinom(x,size = r_hat,prob = p_hat) #Theoretical Distribution
lines(x,y)


#To make the pdf comparable, we discretise and apply the chi.sq test
theoretical_pdf_nb <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- dnbinom(x=i-1 , size = r_hat, prob = p_hat)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])

chisq.test(empirical_pdf,theoretical_pdf)
# Warning message because the frequency in some bins is less than 5 which considered to be minimum

#FF plots
plot.ecdf(data$CLM_FREQ, main = "FF Negative Binomial", xlab = "Loss Frequency", ylab = "CDF", col.points = "Gray")
x <- seq(0,10, 1)
y_2 <- pnbinom(x, size = r_hat, prob = p_hat )
lines(x,y, col = "blue")



#************************************************** Q1 Result *************************************************************#

hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDFs", xlab = "CLAIM FREQUENCY")
x <- seq(0,10,length.out = 11)
y <- dpois(x, lambda = lambda_hat_poisson) #Theoretical Distribution
y_2 <- dnbinom(x,size = r_hat,prob = p_hat) #Theoretical Distribution
lines(x,y, col = "Red")
lines(x,y_2, col = "Blue")
legend("topright", legend = c("Poisson Distribution", "Negative Binomial Distribution"),
       col = c("red", "blue"), lty = 1, cex = 0.9, box.lwd = 0.9)


plot.ecdf(data$CLM_FREQ, main = "FF plots Poisson and Negative Binomial", xlab = "Loss Frequency", ylab = "CDF", col.points = "Gray")
x = seq(0,10, 1)
y = pnbinom(x, size = r_hat, prob = p_hat )
lines(x, y, col = "blue", lwd = 2, lty = 1) 
lines(x, y_2, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Poisson", "Negative Binomial"),
       col = c("blue", "red"), lty = c(1, 2))


#Claim Frequency:
#We decided to stick with NB for claim frequency. since the Mean from the data is further away from the variance. Neg. Bin.
#allows more flexibility in that regard


############################################################################################################################
########################################################### Q2 #############################################################
############################################################################################################################
# Here we analyse Exponential, Gamma, Inverse Gaussian and Log-normal.


#Extract a vector of all single claim amounts
claim_size_vector <- vector()
for  (i in 3:9 ){
  for (j in 1:nrow(data)) 
    if ( data[j,i] > 0 ){
      claim_size_vector <- append(claim_size_vector, data[j,i])
    }
}




############################################# Q2 Check Exponential #########################################################

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))

#Method of Moments Estimator
lambda_hat_exp <- 1 / (1/length((claim_size_vector)) * sum(claim_size_vector))
y <- dexp(x, rate = lambda_hat_exp) #Theoretical Distribution
lines(x,y)

# Get the the theoretical qunatiles for testing
theoretical_quantiles <- qexp(ppoints(length(claim_size_vector)),rate = lambda_hat_exp )

# Test whether exponential is a good model.
ks.test(claim_size_vector,theoretical_quantiles) #kolomogorov smirnov test


qqplot(theoretical_quantiles, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for Exponential Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line

#FF plots
plot.ecdf(claim_size_vector, main = "FF Exponential", xlab = "Loss Amount", ylab = "CDF", col.points = "Gray")
x = seq(0,1500, 0.1)
y = pexp(x, rate = lambda_hat_exp )
lines(x,y, col = "blue")




################################################ Q2 Check Gamma ############################################################

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))

#Maximum Likelihood Estimator
gamma_estimated_theta <- var(claim_size_vector) / mean(claim_size_vector)
gamma_estimated_k <- mean(claim_size_vector)^2/ var(claim_size_vector)

y_2 <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta ) #Theoretical Distribution
lines(x,y_2)

#Additionally we include 11 plot against the theoretical distribution
theoretical_quantiles <- qgamma(ppoints(100), shape = gamma_estimated_k, scale = gamma_estimated_theta )

qqplot(theoretical_quantiles, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for Gamma Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line

ks.test(claim_size_vector, theoretical_quantiles)


qqplot(claim_size_vector,rgamma(length(claim_size_vector),
                                            shape = gamma_estimated_k,scale = gamma_estimated_theta),
                                            main = "QQ Plot for Gamma Distribution")


plot.ecdf(claim_size_vector, main = "FF Gamma", xlab = "Loss Amount", ylab = "CDF", col.points = "Gray")
x = seq(0,1500, 0.1)
y_2 = pgamma(x, shape = gamma_estimated_k, scale = gamma_estimated_theta )
lines(x,y, col = "Blue")



################################################ Q2 Check Lognormal ########################################################

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))

#Maximum Likelihood Estimator
logNormalSigmaSquareEstimator <- function(random_vector){
  return(log(1 + var(random_vector)/(mean(random_vector)^2)))
}
logNormalMuEstimator <- function(random_vector){
  return(log(mean(random_vector)) - (logNormalSigmaSquareEstimator(random_vector)/2) )
}
logNormal_estimator_sd <- sqrt(logNormalSigmaSquareEstimator(claim_size_vector))
logNormal_estimator_mu <- logNormalMuEstimator(claim_size_vector)


y_3 <- dlnorm(x,meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd) #Theoretical Distribution
lines(x,y_3)

#Additionally we include 11 plot against the theoretical distribution
theoretical_quantiles_logNormal <- qlnorm(ppoints(100), meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd )

qqplot(theoretical_quantiles_logNormal, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for Log Normal Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line

ks.test(claim_size_vector, theoretical_quantiles_logNormal)
#ties due to data point repetitions.

plot.ecdf(claim_size_vector, main = "FF Log Normal", xlab = "Loss Amount", ylab = "CDF", col.points = "Gray")
x = seq(0,1500, 0.1)
y_3 = plnorm(x, meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd )  
lines(x,y, col = "Blue")



########################################### Q2 Check Inverse Gaussian #####################################################

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))


#Method of Moments
inverseGaussianMuEstimator <- function(randomVector) {
  return (mean(randomVector)) 
} 
inverseGaussianLambdaEstimator <- function(randomVector) {
  return( mean(randomVector)^3 / var(randomVector)) 
}

IG_estimator_mu <- inverseGaussianMuEstimator(claim_size_vector)
IG_estimator_lambda <- inverseGaussianLambdaEstimator(claim_size_vector) 

y_4 <- dinvgauss(x,mean = IG_estimator_mu, shape = IG_estimator_lambda) #Theoretical Distribution
lines(x,y_3)

theoretical_quantiles_invgauss <- qinvgauss(ppoints(100), mean = IG_estimator_mu,shape = IG_estimator_lambda)

qqplot(theoretical_quantiles_invgauss, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for IG Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line

ks.test(claim_size_vector, theoretical_quantiles_invgauss)


plot.ecdf(claim_size_vector, main = "FF Inverse Gaussian", xlab = "Loss Amount", ylab = "CDF", col.points = "Gray")
x = seq(0,1500, 0.1)
y_4 = pinvgauss(x, mean = IG_estimator_mu, shape = IG_estimator_lambda )  
lines(x,y, col = "Blue")


#******************************************************* Q2 Result *********************************************************

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF",xlab = "Claim Frequency")
x <- seq(0,3500,length.out = length(claim_size_vector))

y <- dexp(x,rate = lambda_hat_exp)
y_2 <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta ) #Theoretical Distribution
lines(x,y, col = "Red")
lines(x,y_2, col = "Blue")
lines(x,y_3,col = "springgreen")
lines(x,y_4,col = "Purple")
legend("topright", legend = c("Exponential", "Gamma", "Log-Normal", "Inverse Gaussian"),
       col = c("red", "blue", "springgreen", "Purple"), lty = 1)




plot.ecdf(claim_size_vector, main = "", xlab = "Loss Amount", ylab = "CDF", col.points = "Gray")
x = seq(0,1500, 0.1)
y = pexp(x, rate = lambda_hat_exp )
y_2 = pgamma(x, shape = gamma_estimated_k, scale = gamma_estimated_theta )
y_3 = plnorm(x, meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd )
y_4 = pinvgauss(x, mean = IG_estimator_mu, shape = IG_estimator_lambda ) 
lines(x,y, col = "Red")
lines(x,y_2, col = "Blue")
lines(x,y_3, col = "springgreen")
lines(x,y_4, col = "magenta")
legend("bottomright", legend = c("Exponential","Gamma", "Log-Normal","Inverse Gaussian"),
       col = c("red", "blue", "springgreen", "magenta"), lty = c(1))


#Log-normal appears to fit data best, as well as inverse gaussian, we decide on the log-normal since it is more prudent 
# (heavy tailed).


############################################################################################################################
########################################################### Q3 #############################################################
############################################################################################################################

set.seed(23)
############################################## Q3 Monte Carlo Negative Binomial ###########################################
#To assess the reliability of our monte carlo estimators in relation to the data we have, we will simulate 1000 negative 
#binomial random variables using the same parameter p and r we obtained from the MoM estimator. We will do the following
#1) Plot histograms of the mean and variance values of the simulation to compare to the data.
#2) Plot a histogram of the average of those 1000 simulations (converting to the true value) and compare them to the data.
#3) Make a sample t-test to test whether the mean value could serve as an expectation of the mean value of the simulations.



#Approach 1
nb_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list[[i]] <- vector(mode = "list", length = 1000)
}
for(i in 1:1000){
  nb_simulations_list[[i]] <- rnbinom(1000,size = r_hat, p = p_hat)##Switch with our own simulation function
}

mean_vector_nb <- c()
var_vector_nb <- c()

for(i in 1:1000){
  mean_vector_nb[i] <- mean(nb_simulations_list[[i]])
  var_vector_nb[i] <- var(nb_simulations_list[[i]])
}

par(mfrow = c(1, 2))
hist(mean_vector_nb, main = "",breaks = 40,
     xlab = "Expected Value")
abline(v = mean(data$CLM_FREQ),col="Red")
#legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_nb, main = "",
     xlab = "Variance")
abline(v = var(data$CLM_FREQ),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)


#Approach 2
#We can also plot the claim frequency against the average of the 10,000 simulations
generate_random_comparable_nb <- function(r,p){
  
  my_nb_simulation <- rnbinom(1000,size = r, p = p)
  
  
  random_vector <- c()
  
  for (i in 1:8) {
    result <- table(my_nb_simulation)[as.character(i-1)] 
    if (is.na(result)) {
      random_vector[i] <- 0
    } else {
      random_vector[i] <- result
    }
    
  }
  random_vector[9] <- 1000- sum(random_vector[1:8])
  return(random_vector)    
}




nb_simulations_list_comparable <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_comparable[[i]] <- vector(mode = "list", length = 9)
}


for(i in 1:1000){
  nb_simulations_list_comparable[[i]] <- generate_random_comparable_nb(r=r_hat,p = p_hat)
}

# We can plot the average of the 1000 simulations against the data
avg_of_simulations_nb <- c()
for(j in 1:9){
  for(i in 1:1000){
    avg_of_simulations_nb[j] <- mean(sapply(nb_simulations_list_comparable, "[[", j))
  }
}

barplot(table(data$CLM_FREQ), col = gray(0,0.5), beside = T, xlab = "Claim Frequency")
barplot(avg_of_simulations_nb, col = gray(1,0.8),beside = T,add = T)
legend("topright", legend = c("Original Data", "Average of 1000 Simulations"), 
       fill = c(gray(0, 0.5), gray(1, 0.8)))


#Approach 3
t.test(x = mean_vector_nb, mu = mean(data$CLM_FREQ))
t.test(x = var_vector_nb, mu = var(data$CLM_FREQ))

#Mean of the data could be seen as to serve as a mean for the simulations
#var of the data could be seen as to serve as a var for the simulations

########################################### Q3 Variance reduction Negative Binomial #######################################


### NB Antithetic Method###################################################################################################
simulate_nb_with_u_input <- function(random_numbers, r, p) {
  result <- c()
  for(i in 1:length(random_numbers)){
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    }
    result[i] <- counter
  }
  return(result)
}


antithetic_variates_nb <- function(size, r, p){
  if(size %% 2 == 0){
    u1_vector <- runif(size/2,0,1)
    u2_vector <- 1-u1_vector
  } else {
    u1_vector <- runif(size/2 +1,0,1)
    u2_vector <- 1-u1_vector
  }
  
  f_u1 <- simulate_nb_with_u_input(random_numbers=u1_vector,r = r, p = p)
  f_u2 <- simulate_nb_with_u_input(random_numbers=u2_vector,r = r, p = p)
  
  return(append(f_u1,f_u2))
}



nb_simulations_list_anthithetic <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_anthithetic[[i]] <- vector(mode = "list", length = 1000)
}
for(i in 1:1000){
  nb_simulations_list_anthithetic[[i]] <- antithetic_variates_nb(size = 1000,r = r_hat, p = p_hat)
}

mean_vector_nb_antithetic <- c()
var_vector_nb_antithetic <- c()



for(i in 1:1000){
  mean_vector_nb_antithetic[i] <- mean(nb_simulations_list_anthithetic[[i]])
  var_vector_nb_antithetic[i] <- var(nb_simulations_list_anthithetic[[i]])
}

#Histogram of Antithetic covariate method estimators 
hist(mean_vector_nb_antithetic, main = "Expectation of 1000 NB Simulations - Antithetic Method")
abline(v = mean(mean_vector_nb),col="Red")
legend("topright", legend = "CMC", col = "red", lty = 1)
mtext(paste0("Mean: ",round(mean(mean_vector_nb_antithetic),4),
             " vs CMC:",round(mean(mean_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)

#are they biased?
hist(mean_vector_nb_antithetic-mean_vector_nb, main = "Differences: Antithetic Method - CMC")
mtext(paste0("pval: ", round(t.test(mean_vector_nb, mean_vector_nb_antithetic)$p.val, 6), "\n",
            "conf.Int 95%: (", round(t.test(mean_vector_nb, mean_vector_nb_antithetic)$conf[1],6), ";",
            round(t.test(mean_vector_nb, mean_vector_nb_antithetic)$conf[2],6), ")")
      , side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



#Compare variance
hist(var_vector_nb_antithetic, main = "Variance - Antithetic Method")
abline(v = mean(var_vector_nb_antithetic),col="Red")
legend("topright", legend = "Var CMC", col = "red", lty = 1)
mtext(paste0("Var: ",round(mean(var_vector_nb_antithetic),4),
             " vs CMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



### NB Control Variate Method #############################################################################################

simulate_nb_cv_poisson <- function(sample_size,size,prob, lambda ){
  x <- sort(rnbinom(n = sample_size, size = size, prob = prob))
  y <- sort(rpois(n = sample_size,lambda = lambda))
  z <- x - cov(x,y)*1/var(y)*(y - lambda)
  return(z)
}

nb_simulations_list_cv <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_cv[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  nb_simulations_list_cv[[i]] <- simulate_nb_cv_poisson(sample_size = 1000,
                                                             size = r_hat,
                                                             prob = p_hat,
                                                             lambda = lambda_hat_poisson)
}

mean_vector_nb_cv <- c()
var_vector_nb_cv <- c()

for(i in 1:1000){
  mean_vector_nb_cv[i] <- mean(nb_simulations_list_cv[[i]])
  var_vector_nb_cv[i] <- var(nb_simulations_list_cv[[i]])
}


#Histogram of control covariate method estimators 
hist(mean_vector_nb_cv, main = "Expectation of 1000 NB Simulations - Antithetic Method")
abline(v = mean(mean_vector_nb_cv),col="Red")
legend("topright", legend = "Mean CMC", col = "red", lty = 1)
mtext(paste0("Mean: ",round(mean(mean_vector_nb_cv),4),
             " vs CMC:",round(mean(mean_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)

#are they biased? #Theoretically unbiased
hist(mean_vector_nb_cv-mean_vector_nb, main = "Differences: CV Method - CMC")
mtext(paste0("pval: ", round(t.test(mean_vector_nb, mean_vector_nb_cv)$p.val, 6), "\n",
             "conf.Int 95%: (", round(t.test(mean_vector_nb, mean_vector_nb_cv)$conf[1],6), ";",
             round(t.test(mean_vector_nb, mean_vector_nb_cv)$conf[2],6), ")")
      , side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



#Compare variance
hist(var_vector_nb_cv, main = "Variance - CV Method")
abline(v = mean(var_vector_nb_cv),col="Red")
legend("topright", legend = "Var CMC", col = "red", lty = 1,cex = 0.8)
mtext(paste0("Var: ",round(mean(var_vector_nb_cv),4),
             " vs CMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)


### NB Importance Sampling Method #########################################################################################

#try geometric
simulate_nb_IS_geometric <- function(size_n, size_nb, prob_nb , prob_geometric){
  x <- rgeom(n = size_n, prob = prob_geometric)
  ISE <- dnbinom(x,size = size_nb,prob = prob_nb) / 
    dgeom(x,prob = prob_geometric) * x
  #browser()
  return(ISE)
}


nb_simulations_list_IS_geometric <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_IS_geometric[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  nb_simulations_list_IS_geometric[[i]] <- simulate_nb_IS_geometric(size_n = 1000,
                                                                    size_nb = r_hat,
                                                                    prob_nb = p_hat,
                                                                    prob_geometric = p_hat_geometric)
}

mean_vector_nb_IS_geo <- c()
var_vector_nb_IS_geo <- c()


for(i in 1:1000){
  mean_vector_nb_IS_geo[i] <- mean(nb_simulations_list_IS_geometric[[i]])
  var_vector_nb_IS_geo[i]<- var(nb_simulations_list_IS_geometric[[i]])
}


#Skip mean comparison. it's again almost exactly the same as per theory


#Compare variance
hist(var_vector_nb_IS_geo, main = "Variance - CV Method")
abline(v = mean(var_vector_nb_IS_geo),col="Red")
legend("topright", legend = "Var CMC", col = "red", lty = 1,cex = 0.8)
mtext(paste0("Var: ",round(mean(var_vector_nb_IS_geo),4),
             " vs CMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



### NB Stratified Sampling Method #########################################################################################

# summary(glm(data$CLM_FREQ~data$AREA+data$GENDER+data$AGE+data$CAR_TYPE+data$CAR_USE,family = "poisson")) # no patterns

################################################ Q3 Monte Carlo Log Normal ################################################

#Here we will simulate 1000 random log noraml distributions with the method of moment scale and shape parameters and test our data against
#each of them and take the mean p-value.
#Approach 1 and 3
ln_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  ln_simulations_list[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}


for(i in 1:1000){
  ln_simulations_list[[i]] <- rlnorm(length(claim_size_vector),meanlog = logNormal_estimator_mu,
                                     sdlog = logNormal_estimator_sd)##Switch with our own simulation function
}


mean_vector_ln <- c()
var_vector_ln <- c()

for(i in 1:1000){
  mean_vector_ln[i] <- mean(ln_simulations_list[[i]])
  var_vector_ln[i] <- var(ln_simulations_list[[i]])
}



par(mfrow = c(1, 2))
hist(mean_vector_ln, main = "", xlab = "Expected Value")
abline(v = mean(claim_size_vector),col="Red")
#legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_ln, main = "", xlab = "Variance")
abline(v = mean(var(claim_size_vector)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)


t.test(x = mean_vector_ln, mu = mean(claim_size_vector))
t.test(x = var_vector_ln, mu = mean(var(claim_size_vector)))


############################################ Q3 variance reduction Log normal #############################################


### Antithetic Method #####################################################################################################


simulate_ln_with_u_input <- function(random_numbers, mean, sd) {
  return(qlnorm(random_numbers, meanlog = mean , sdlog =  sd))
}


simulate_ln_antithetic_variates <- function(size, mean, sd){
  if(size %% 2 == 0){
    u1_vector <- runif(size/2,0,1)
    u2_vector <- 1-u1_vector
  } else {
    u1_vector <- runif(size/2 +1,0,1)
    u2_vector <- 1-u1_vector
  }
  
  f_u1 <- simulate_ln_with_u_input(random_numbers=u1_vector,mean = mean, sd = sd)
  f_u2 <- simulate_ln_with_u_input(random_numbers=u2_vector,mean = mean, sd = sd)
  
  return(append(f_u1,f_u2))
}




ln_simulations_list_anthithetic <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  ln_simulations_list_anthithetic[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}

for(i in 1:1000){
  ln_simulations_list_anthithetic[[i]] <- simulate_ln_antithetic_variates(size = length(claim_size_vector),
                                                                        mean = logNormal_estimator_mu,
                                                                        sd = logNormal_estimator_sd)
}

mean_vector_ln_antithetic <- c()
var_vector_ln_antithetic <- c()


for(i in 1:1000){
  mean_vector_ln_antithetic[i] <- mean(ln_simulations_list_anthithetic[[i]])
  var_vector_ln_antithetic[i] <- var(ln_simulations_list_anthithetic[[i]])
}

#Histogram of Antithetic covariate method estimators 
hist(mean_vector_ln_antithetic, main = "Expectation of 1000 Log Normal Simulations - Antithetic Method")
abline(v = mean(mean_vector_ln_antithetic),col="Red")
legend("topright", legend = "CMC", col = "red", lty = 1)
mtext(paste0("Mean: ",round(mean(mean_vector_ln_antithetic),4),
             " vs CMC:",round(mean(mean_vector_ln_antithetic),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)

#are they biased? #Theoretically unbiased
hist(mean_vector_ln-mean_vector_ln_antithetic, main = "Differences: Antithetic Method - CMC")
mtext(paste0("pval: ", round(t.test(mean_vector_ln, mean_vector_ln_antithetic)$p.val, 6), "\n",
             "conf.Int 95%: (", round(t.test(mean_vector_ln, mean_vector_ln_antithetic)$conf[1],6), ";",
             round(t.test(mean_vector_ln, mean_vector_ln_antithetic)$conf[2],6), ")")
      , side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



#Compare variance
hist(var_vector_ln_antithetic, main = "Variance - Antithetic Method")
abline(v = mean(var_vector_ln_antithetic),col="Red")
legend("topright", legend = "Var Antithetic", col = "red", lty = 1,cex = 0.8)
mtext(paste0("Var: ",round(mean(var_vector_ln_antithetic),4),
             " vs CMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



### Control Variate ######################################################################################################

simulate_ln_cv_IG <- function(size,mean_ln,sd_ln, mean_IG, shape_IG ){
  x <- sort(rlnorm(size,meanlog = mean_ln,sdlog = sd_ln))
  y <- sort(rinvgauss(size,mean = mean_IG, shape = shape_IG))
  z <- x - cov(x,y)*1/var(y)*(y - mean_IG)
  return(z)
}

ln_simulations_list_cv_IG <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  ln_simulations_list_cv_IG[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  ln_simulations_list_cv_IG[[i]] <- simulate_ln_cv_IG(size = length(claim_size_vector),
                                                                       mean_ln = logNormal_estimator_mu,
                                                                       sd_ln = logNormal_estimator_sd,
                                                                       mean_IG = IG_estimator_mu,
                                                                       shape_IG = IG_estimator_lambda)
}


mean_vector_ln_cv_IG <- c()
var_vector_ln_cv_IG <- c()


for(i in 1:1000){
  mean_vector_ln_cv_IG[i] <- mean(ln_simulations_list_cv_IG[[i]])
  var_vector_ln_cv_IG[i] <- var(ln_simulations_list_cv_IG[[i]])
  }

#Histogram of Antithetic covariate method estimators 
hist(mean_vector_ln_cv_IG, main = "Expectation of 1000 Gamma Simulations - CV Method")
abline(v = mean(mean_vector_ln_cv_IG),col="Red")
legend("topright", legend = "CMC", col = "red", lty = 1)
mtext(paste0("Mean: ",round(mean(mean_vector_ln_cv_IG),4),
             " vs CMC:",round(mean(mean_vector_ln_cv_IG),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)

#are they biased? #Theoretically unbiased
hist(mean_vector_ln-mean_vector_ln_cv_IG, main = "Differences: CV Method - CMC")
mtext(paste0("pval: ", round(t.test(mean_vector_ln, mean_vector_ln_cv_IG)$p.val, 6), "\n",
             "conf.Int 95%: (", round(t.test(mean_vector_ln, mean_vector_ln_cv_IG)$conf[1],6), ";",
             round(t.test(mean_vector_ln, mean_vector_ln_cv_IG)$conf[2],6), ")")
      , side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



#Compare variance
hist(var_vector_ln_cv_IG, main = "Variance - CV Method")
abline(v = mean(var_vector_ln_cv_IG),col="Red")
legend("topright", legend = "Var CV", col = "red", lty = 1,cex = 0.8)
mtext(paste0("Var: ",round(mean(var_vector_ln_cv_IG),4),
             " vs CMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)




### Importance Sampling ###################################################################################################

#I = Gamma
#F = F_tilda = lambda*e^(-lambda*x), g(x) = I(x) = x
x <- rgamma(n = length(claim_size_vector),shape = gamma_estimated_k,scale = gamma_estimated_theta)
ISE <- dlnorm(x,meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd) / 
  dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta) * x




simulate_ln_IS_gamma <- function(size, shape_gamma, scale_gamma, mean_ln, sd_ln){
  x <- rgamma(n = size, shape = shape_gamma, scale = scale_gamma)
  return( dlnorm(x,meanlog =  mean_ln,sdlog = sd_ln) / 
            dgamma(x,shape = shape_gamma, scale = scale_gamma) * x )
}



ln_simulations_list_IS_gamma <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  ln_simulations_list_IS_gamma[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  ln_simulations_list_IS_gamma[[i]] <- simulate_ln_IS_gamma(size = length(claim_size_vector),
                                                             shape_gamma = gamma_estimated_k,
                                                             scale_gamma = gamma_estimated_theta,
                                                             mean_ln = logNormal_estimator_mu,
                                                             sd_ln = logNormal_estimator_sd)
}



mean_vector_ln_IS_gamma <- c()
var_vector_ln_IS_gamma <- c()


for(i in 1:1000){
  mean_vector_ln_IS_gamma[i] <- mean(ln_simulations_list_IS_gamma[[i]])
  var_vector_ln_IS_gamma[i]<- var(ln_simulations_list_IS_gamma[[i]])
}


hist(mean_vector_ln_IS_gamma, main = "Expectation of 1000 Log Normal Simulations - IS Method")
abline(v = mean(mean_vector_ln_IS_gamma),col="Red")
legend("topright", legend = "CMC", col = "red", lty = 1)
mtext(paste0("Mean: ",round(mean(mean_vector_ln_IS_gamma),4),
             " vs CMC:",round(mean(mean_vector_ln_cv_IG),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)

#are they biased? #Theoretically unbiased
hist(mean_vector_ln-mean_vector_ln_IS_gamma, main = "Differences: IS Method - CMC")
mtext(paste0("pval: ", round(t.test(mean_vector_ln, mean_vector_ln_IS_gamma)$p.val, 6), "\n",
             "conf.Int 95%: (", round(t.test(mean_vector_ln, mean_vector_ln_IS_gamma)$conf[1],6), ";",
             round(t.test(mean_vector_ln, mean_vector_ln_IS_gamma)$conf[2],6), ")")
      , side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



#Compare variance
hist(var_vector_ln_IS_gamma, main = "Variance - IS Method")
abline(v = mean(var_vector_ln_IS_gamma),col="Red")
legend("topright", legend = "Var CV", col = "red", lty = 1,cex = 0.8)
mtext(paste0("Var: ",round(mean(var_vector_ln_IS_gamma),4),
             " vs CMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)



### Stratified Sampling ###################################################################################################

#summary(glm(temp2$CLM_AMT~temp2$AREA+temp2$GENDER+temp2$AGE+temp2$CAR_TYPE+temp2$CAR_USE,family = "Gamma")) no patterns



#****************************************************** Q3  Result ********************************************************#

par(mfrow = c(2, 2),cex.main = 0.8)
hist(var_vector_nb, main = "Variance NB w/o variance reduction",xlab = "", breaks = 20)
subplot(hist(nb_simulations_list[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "Sample Simulation",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))



hist(var_vector_nb_antithetic, main = "Variance - Antithetic Method", xlab = "", breaks = 20)
mtext(paste0("Var: ",round(mean(var_vector_nb_antithetic),4),
             " vs\nCMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(nb_simulations_list_anthithetic[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))


hist(var_vector_nb_cv, main = "Variance - CV Method (Poisson)", xlab = "", breaks = 20)
mtext(paste0("Var: ",round(mean(var_vector_nb_cv),4),
             " vs\nCMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(nb_simulations_list_cv[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))


hist(var_vector_nb_IS_geo, main = "Variance - IS Method (Geometric)", xlab = "", breaks = 20)
mtext(paste0("Var: ",round(mean(var_vector_nb_IS_geo),4),
             " vs \nCMC:",round(mean(var_vector_nb),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(nb_simulations_list_IS_geometric[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))

#The monte carlo estimates seem to resemble what we expect given our choice of negative binomial. As a variance reduction technique
#Control variate with poisson resulted in the lowest variance.

par(mfrow = c(2, 2),cex.main = 0.8)
hist(var_vector_ln, main = "Variance Log Normal w/o variance reduction", xlab = "", breaks = 10)
subplot(hist(ln_simulations_list[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))


hist(var_vector_ln_antithetic, main = "Variance Log Normal - Antithetic Method", xlab = "", breaks = 10)
mtext(paste0("Var: ",round(mean(var_vector_ln_antithetic),4),
             " vs\nCMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(ln_simulations_list_anthithetic[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))


hist(var_vector_ln_cv_IG, main = "Variance Log Normal - CV Method (Inverse Gaussian)", xlab = "", breaks = 10)
mtext(paste0("                          Var: ",round(mean(var_vector_ln_cv_IG),4),
             " vs CMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(ln_simulations_list_cv_IG[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))

hist(var_vector_ln_IS_gamma, main = "Variance Log Normal - IS Method (Gamma)", xlab = "", breaks = 10)
mtext(paste0("            Var: ",round(mean(var_vector_ln_IS_gamma),4),
             " vs CMC:",round(mean(var_vector_ln),4)),  side = 3, line = -1, adj = 0, col = "black", cex = 0.9)
subplot(hist(ln_simulations_list_IS_gamma[[1]],ylab = "",xlab = "",ylim = NULL,yaxt = "n", main = "",cex.main = 0.7)
        , grconvertX(c(.75, 1), "npc"), grconvertY(c(0.75, 1), "npc"))


#The monte carlo estimates seem to resemble what we expect given our choice of log-nomal. As a variance reduction technique
#Control variate with inverse gaussian resulted in the lowest variance.


############################################################################################################################
########################################################### Q4 #############################################################
############################################################################################################################

########################################### Risk Premium Calculation through Data ##########################################

data_risk_premium_estimation <- sum(claim_size_vector) / nrow(data)


########################################## Risk Premium Calculation through Model ##########################################


monte_carlo_claim_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  monte_carlo_claim_simulations_list[[i]] <- vector(mode = "list", length = 1000)
}

agg_monte_carlo_claims <- c()
for(i in 1:1000){
  clm_freq = monte_carlo_claim_freq <- rnbinom(1000,size = r_hat, prob = p_hat)
  monte_carlo_claim_sev <- purrr::map(monte_carlo_claim_freq,function(x){rlnorm(x,meanlog = logNormal_estimator_mu,
                                                                                sdlog = logNormal_estimator_sd)})
  monte_carlo_claim_simulations_list[[i]] <- sapply(monte_carlo_claim_sev,sum)
  agg_monte_carlo_claims[i] <- sum(sapply(monte_carlo_claim_sev,sum))
}

mean_monte_carlo_claims <- c()


for(i in 1:1000){
  mean_monte_carlo_claims[i] <- mean(monte_carlo_claim_simulations_list[[i]])
}

#****************************************************** Q4  Result ********************************************************#

#The calculated empirical risk premium was 765.451, while the Monte Carlo risk premium converged
#to an average value of 766.2344.

############################################################################################################################
########################################################### Q5 #############################################################
############################################################################################################################

par(mfrow = c(1, 1))
#histogram of the mean per policy view 
hist(mean_monte_carlo_claims,xaxt = "n", main = "Data vs CMC Risk Premium esitmate", xlab = "CMC Expected Claims")
abline(v=data_risk_premium_estimation, col = "Red")
abline(v=mean(mean_monte_carlo_claims, col = "black"))
legend("topright", legend = c("Calculated Data Risk Premium", "Caclulated CMC Risk Premium"),
       col = c("red", "Black"), lty = 1, cex = 0.75, box.lwd = 0.5)
axis(side = 1, at = seq(690, 850, length.out = 10))

#The premium charged in the data set is even lower than the expected value of the claims!!
#Analysis of Existing Premium & Claims Against Monte Carlo Simulations on per policy basis
hist(agg_monte_carlo_claims, main = "Aggregate claim amounts - 1000 simulations",xaxt = "n",xlab = "")
axis(side = 1, at = seq(7e5, 9e5, length.out = 10))
abline(v=sum(data$PREMIUM), col = "Red")
abline(v=sum(claim_size_vector), col = "Blue")
abline(v=mean(agg_monte_carlo_claims), col = "Green")
legend("topright", legend = c("Portfolio Agg. Premium", "Portfolio Agg. Claims", "Model Premium estimates*"),
       col = c("red", "blue", "green"), lty = 1, cex = 0.75, box.lwd = 0.5)
mtext(paste0("Coverage of approx. \n",
             length(agg_monte_carlo_claims[agg_monte_carlo_claims<sum(data$PREMIUM)])/length(agg_monte_carlo_claims)*100,
             "% under current premium \nscheme\n",
             length(agg_monte_carlo_claims[agg_monte_carlo_claims<mean(agg_monte_carlo_claims)])/length(agg_monte_carlo_claims)*100,
             "% under our model (w/o loadings)"),
      side = 3, line = -2, adj = 0, col = "black", cex = 0.9)


#****************************************************** Q5  Result ********************************************************#

#The tariff/premium charged by us only covers approx. 27% of the claims. Our model excl. loading performs as expected
#covering around 50%


############################################################################################################################
########################################################### Q6 #############################################################
############################################################################################################################

#
####delete?
crudeMCSimAlphaQuantile <- function(alpha, simulatedVector) {
  #initialise simulatedvector 
  # find percentage of vector 
  #integer casting of
  orderedSimulatedVector <- sort(simulatedVector, decreasing= TRUE)
  index <- as.integer(alpha *length(orderedSimulatedVector)) #or  round(alpha *length(simulatedVector), digits = 0) 
  return(orderedSimulatedVector[index])
}



for (i in 1:1000) {
  VaR_value <-0
  for(j in 1:1000){#simulate one portfolio
  clm_freq <- rnbinom(1000, size = r_hat, prob = p_hat)
  clm_sev <- unlist(purrr::map(clm_freq, function(x) {
    rlnorm(x, meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd)}))
  VaR_value <- VaR_value + crudeMCSimAlphaQuantile(0.05,clm_sev)
  }
  capital_req[i] <- VaR_value
}

##### Check with pierre delete until here


ES_q_vector <-c()
capital_req <- c()
for (i in 1:1000) {
  clm_freq <- rnbinom(1000, size = r_hat, prob = p_hat)
  clm_sev <- unlist(purrr::map(monte_carlo_claim_freq, function(x) {
    rlnorm(x, meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd)
  }))
  single_portfolio <- c(clm_sev, rep(0, times = max(0, 1000 - length(clm_sev))))
  
  vaR <- 0
  ES_q <- 0
  for (j in 1:1000) {
    vaR <- vaR + sort(single_portfolio, decreasing = TRUE)[5]
    ES_q <- ES_q + sort(single_portfolio, decreasing = TRUE)[50]
  }
  capital_req[i] <- vaR
  ES_q_vector <- ES_q
}



capital_req_ES <-c()
capital_req_var <- c()
for (i in 1:1000) {
  clm_freq <- rnbinom(1000, size = r_hat, prob = p_hat)
  clm_sev <- unlist(purrr::map(monte_carlo_claim_freq, function(x) {
    rlnorm(x, meanlog = logNormal_estimator_mu, sdlog = logNormal_estimator_sd)
  }))
  single_portfolio <- c(clm_sev, rep(0, times = max(0, 1000 - length(clm_sev))))
  #5 Corresponds to 0.5%
  #50 Corresponds to 5% here the sorting is descending.
  vaR <- 0
  ES_q <- 0
  for (j in 1:1000) {
    vaR <- vaR + sort(single_portfolio, decreasing = TRUE)[5]
    ES_q <- ES_q + mean(sort(single_portfolio, decreasing = TRUE)[1:50])
  }
  capital_req_var[i] <- vaR
  capital_req_ES[i] <- ES_q
}



hist(capital_req_var, breaks = 100, main = "1,000 VaR simulations (Neg. Binomial & Log-Normal)",xlab = "Required Capital",
     xlim = c(800000, max(capital_req_var)))
abline(v=mean(capital_req_ES), col = "red")
legend("topright", legend = c("Expected Shortfall"),
       col = "red", lty = 1, cex = 0.75, box.lwd = 0.5)

#****************************************************** Q6  Result ********************************************************#
#VaR ranging between 1,046,588 and 1,549,250 at the 0.5% level.
T#he Expected Shortfall at the 5% level appears to be less strict in terms of capital requirement. The
#simulation provided values ranging between 929,414 and 1,129,192


############################################################################################################################
###################################################### Final Results #######################################################
############################################################################################################################
##Data Exploration #Report referencing
median(data[data$GENDER=="M","AGE"])
median(data[data$GENDER=="F","AGE"])
nrow(data[data$AREA=="Urban",])/nrow(data)
nrow(data[data$CAR_USE=="Private",])/nrow(data)
nrow(data[data$GENDER=="F" & data$CAR_TYPE =="Sports Car",])/nrow(data[data$CAR_TYPE=="Sports Car",])
nrow(data[data$GENDER=="F" & data$CAR_TYPE =="SUV",])/nrow(data[data$CAR_TYPE=="SUV",])
mean(data[data$CAR_TYPE=="SUV","CLM_FREQ"])
median(data[data$CAR_TYPE=="SUV","CLM_FREQ"])
cor(temp2$CLM_AMT,temp2$CLM_FREQ) #need to re-run temp2 at the top
data[data$AGE>=70,]
