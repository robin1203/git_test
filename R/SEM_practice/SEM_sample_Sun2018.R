popmod1 <-'
# variances of X1 and X2 are fixed at 1 
x1~~1*x1 
x2~~1*x2

# correlation between X1 and X2 is assumed to be .30 
x1~~.3*x2

# regression path for Y on X1 is assumed to be .10 
y~.10*x1

# regression path of interest, Y on X2, is assumed to be .20 
y~.20*x2

# residual variance of Y is 1 - (.1^2 + .2^2) = .95 
y~~.95*y 
'

fitmod <- ' 
# variances of X1 and X2 
x1~~x1 
x2~~x2

# correlation between X1 and X2 
x1~~x2

# regression path for Y on X1 
y~x1

# regression path of interest, Y on X2 
y~x2

# residual variance of Y 
y~~y
'

set.seed(20181102) # setting a seed for reproducibility of the example 
data <- simulateData(popmod1, sample.nobs = 500) # assume 500 participants for now
fit <- sem(model = fitmod, data=data, fixed.x=F)
parameterEstimates(fit) # see all parameters




#---------------------------
results <- NULL # create empty object to store results

for (i in 1:1000){ #simulating 1000 datasets 
  data <- simulateData(popmod1, sample.nobs = 500) # each dataset contains 500 participants
  fit <- sem(model = fitmod, data=data, fixed.x=F) # fit the model to each dataset 
  results <- rbind(results,parameterEstimates(fit)[5,]) # save the row for y ~ x2 for each dataset 
}
# Count the proportion of significant parameter estimates out of 1000 datasets 

paste0('Estimated power = ', mean(results$pvalue < .05))


# now that we are trying a few different sample sizes, we need to create a list to store these results 
powerlist <- list()

# extending the for() loop with a loop for different sample sizes 
for (j in seq(100, 400, by=100)){ # trying sample sizes of 100, 200, 300, 400
  results <- NULL 
  for (i in 1:100) { #starting with 100 datasets for each sample size 
    data <- simulateData(popmod1, sample.nobs = j) # j corresponds to the sample size 
    fit <- sem(model = fitmod, data=data, fixed.x=F) 
    results <- rbind(results,parameterEstimates(fit)[5,]) # row for y ~ x2 
    powerlist[[j]] <- mean(results$pvalue < .05)
  }
}


# Convert the power list into a table 
library(plyr) 
powertable <- ldply(powerlist) 
names(powertable)[1] <- c('power')

# Add a column for the sample size 
powertable$N <- seq(100,400,by=100)

# Here are all the power estimates: 
powertable

paste0('The smallest sample size that provided at least 80% power was N = ',
       powertable[which(powertable$power>.80),'N'][1])
