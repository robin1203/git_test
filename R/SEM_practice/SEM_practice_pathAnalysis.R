library(lavaan)
library(semPlot)

#모수에 대한 가정.
pop_model <-'
  anth ~~ .2 * stat
  
  #regression paths
  agency ~ 0.3 * anth #Yam(2020): .28
  agency ~ 0.3 * stat #임의로 지정
  
  patiency ~ 0.3 * anth #Yam(2020): .27
  patiency ~ 0.3 * stat #임의로 지정
  
  moral ~ 0.25 * agency #Yam(2020): 0.22, Bigman(2018): 0.16/0.26/0.21/0.35
  moral ~ 0.3 * patiency #Yam(2020): 0.26, Bigman(2018): 0.31/0.26/0.40/ns
  '

my_model <-'
  anth ~~ stat
  
  #regression paths
  agency ~ anth
  agency ~ stat
  
  patiency ~ anth
  patiency ~ stat
  
  moral ~ agency
  moral ~ patiency
'
#set.seed(20210630)
data <- simulateData(pop_model, sample.nobs = 200) #sample size
fit <- sem(model = my_model, data = data, fixed.x = F)
parameterEstimates(fit) #2, 5, 11

fit <- cfa(model = my_model, data = data)
summary(fit, fit.measures=TRUE)

#---------------------------
checklist <- 2:7

#significant test
results <- NULL # create empty object to store results

for (i in 1:1000){ #simulating 1000 datasets 
  data <- simulateData(pop_model, sample.nobs = 300) # each dataset contains 500 participants
  fit <- sem(model = my_model, data=data, fixed.x=F) # fit the model to each dataset
  result <- parameterestimates(fit)
  for (j in checklist)
    results <- rbind(results, result[j,]) # save the row for y ~ x2 for each dataset 
}

# Count the proportion of significant parameter estimates out of 1000 datasets 
paste0('Estimated power = ', mean(results$pvalue < .05))

#####
cnt = 0
for (i in 1:10000){ #simulating 1000 datasets 
  data <- simulateData(pop_model, sample.nobs = 230) # each dataset contains 500 participants
  fit <- sem(model = my_model, data=data, fixed.x=F) # fit the model to each dataset 
  result <- parameterEstimates(fit) # save the row for y ~ x2 for each dataset 
  for (j in checklist){
    if (result$pvalue[j] >= .05){
      cnt = cnt + 1
      break
    } 
  }
}
print(cnt/10000) #10000, 290 obs => 0.0414

# now that we are trying a few different sample sizes, we need to create a list to store these results 
powerlist <- list()

# extending the for() loop with a loop for different sample sizes 
for (j in seq(100, 400, by=100)){ # trying sample sizes of 100, 200, 300, 400
  results <- NULL 
  for (i in 1:100) { #starting with 100 datasets for each sample size 
    data <- simulateData(pop_model, sample.nobs = j) # j corresponds to the sample size 
    fit <- sem(model = my_model, data=data, fixed.x=F) 
    results <- rbind(results,parameterEstimates(fit)[7,]) # row for y ~ x2 
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

#---------
# create a list to store these results for different sample sizes 
powerlist <- list()

# extending the for() loop with a loop for different sample sizes 
for(j in seq(200,300,by=10)){ # trying sample sizes between 200 to 300, in increments of 10 
  results <- NULL 
  for (i in 1:1000){ # now simulating 1000 datasets for each sample size 
    data <- simulateData(pop_model, sample.nobs = j) # j corresponds to the sample size 
    fit <- sem(model = my_model, data=data, fixed.x=F) 
    results <- rbind(results,parameterEstimates(fit)[7,]) # row for y ~ x2 
    powerlist[[j]] <- mean(results$pvalue < .05) 
    }
}

# Convert the power list into a table 
powertable <- ldply(powerlist) 
names(powertable)[1] <- c('power')

# Add a column for the sample size 
powertable$N <- seq(200,300,by=10)

# Here are all the power estimates: 
powertable

# Conclusion:
paste0('The smallest sample size that provided at least 80% power was N = ', 
       powertable[which(powertable$power>.80),'N'][1])
