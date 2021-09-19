library(lavaan)
library(semPlot)

#모수에 대한 가정.
pop_model <-'
  anth ~~ .2 * stat
  
  #for the latent variables
  agency =~ 0.7*a1 + 0.7*a2 + 0.7*a3 +0.7*a4
  patiency =~ 0.7*p1 + 0.7*p2 + 0.7*p3 + 0.7*p4
  stat =~ 0.7*s1 + 0.7*s2 + 0.7*s3
  anth =~ 0.7*h1 + 0.7*h2 + 0.7*h3 + 0.7*h4 + 0.7*h5
  
  #regression paths
  agency ~ 0.3 * anth
  agency ~ 0.3 * stat #임의로 지정
  
  patiency ~ 0.3 * anth
  patiency ~ 0.3 * stat #임의로 지정
  
  moral ~ 0.25 * agency
  moral ~ 0.3 * patiency
  '

my_model <-' #DF = 113
  anth ~~ stat
  
  #for latent variables
  agency =~ a1 + a2 + a3 + a4
  patiency =~ p1 + p2 + p3 + p4
  stat =~ s1 + s2 + s3 
  anth =~ h1 + h2 + h3 + h4 + h5
  
  #regression paths
  agency ~ anth
  agency ~ stat
  
  patiency ~ anth
  patiency ~ stat
  
  moral ~ agency
  moral ~ patiency
'
set.seed(20210702)
data <- simulateData(pop_model, sample.nobs = 250) #sample size
#data$anth <- ifelse(data$anth >= 0, 1, 0)
#data$stat <- ifelse(data$stat >= 0, 1, 0)
fit <- sem(model = my_model, data = data, fixed.x = F)
parameterEstimates(fit)

fit <- cfa(model = my_model, data = data)
summary(fit, fit.measures=TRUE)

#---------------------------
#significant test
results <- NULL # create empty object to store results

for (i in 1:1000){ #simulating 1000 datasets 
  data <- simulateData(pop_model, sample.nobs = 200) # each dataset contains 500 participants
  fit <- sem(model = my_model, data=data, fixed.x=F) # fit the model to each dataset 
  results <- rbind(results, parameterEstimates(fit)[15,]) # save the row for y ~ x2 for each dataset 
}

# Count the proportion of significant parameter estimates out of 1000 datasets 
paste0('Estimated power = ', mean(results$pvalue < .05))

#####
cnt = 0
checklist <- c(3, 4, 5, 7, 8, 9, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
for (i in 1:1000){ #simulating 1000 datasets 
  data <- simulateData(pop_model, sample.nobs = 550) # each dataset contains 500 participants
  fit <- sem(model = my_model, data=data, fixed.x=F) # fit the model to each dataset 
  result <- parameterEstimates(fit) # save the row for y ~ x2 for each dataset 
  for (j in checklist){
    if (result$pvalue[j] >= .05){
      cnt = cnt + 1
      break
    } #탈락 카운트.
  }
}
print(cnt/1000) #10000simul, 500 samples => 0.0589//1000simul, 600 samples => 0.041

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
for(j in seq(200,300,by=10)){ # trying sample sizes between 150 to 250, in increments of 10 
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
